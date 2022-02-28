library(data.table)
library(dplyr)
library(lubridate)

rm(list=ls())
dt<- fread("./data/preprocessing/exam_impol_nox.csv", encoding = "UTF-8", na.string = c("", NA))

# 특수, 일반만 하면 이전보다 발생인원이 줄어듦
dt = dt[EXMKND %in% c(1, 2, 3, 6, 7, 9, 10, 30, 99)]
table(dt$EXMKND)

# 이완기, 당화혈색소 numeric
str(dt)
dt$HA002 = as.numeric(dt$HA002)
dt$C3825 = as.numeric(dt$C3825)

# 오류값 NA
dt[CIRABD>900, CIRABD := NA]
dt[HA001 >900, HA001 := NA]
dt[HA002 >900, HA002 := NA]

summary(dt$L0LC2443)
summary(dt$L0MC3711)
summary(dt$L0VC2420)
summary(dt$CIRABD)

# BMI 
dt[,BMI:=HA002/(HA001/100)^2]
colSums(is.na(dt))

#가족력 수정(한 번이라도 있으면 있는걸로)
#가족력 다 NA면 가족력 없는걸로

dt[,F1:= as.numeric(sum(F1, na.rm = T)>0), by = PATNUM]
dt[,F2:= as.numeric(sum(F2, na.rm = T)>0), by = PATNUM]
dt[,F3:= as.numeric(sum(F3, na.rm = T)>0), by = PATNUM]
dt[,F4:= as.numeric(sum(F4, na.rm = T)>0), by = PATNUM]
dt[,F := as.numeric(sum(F, na.rm = T)>0), by = PATNUM]

colSums(is.na(dt))



# 흡연 수정-----------------------------
# 흡연경력있으면 NA-> 과거흡연자

dttmp <- copy(dt)
dttmp[,table(smoke, useNA = "always")]

a = dttmp[,.(.(smoke)), by=PATNUM]
a[,V2:=lapply(a$V1, unique)]
a[,V3:=as.character(V2)]
a[V3 =="NA"]
table(a$V3)
dttmp[,table(smoke, useNA = "always")]

dttmp[, smokestat:= sum(packyear, na.rm =T)>0, by = PATNUM]

dttmp[is.na(smoke) & smokestat == 1, smoke:=3]
dttmp[is.na(smoke) & smokestat == 0, smoke:=1]
dttmp[,table(smoke, useNA = "always")]

dt = copy(dttmp)
rm("dttmp", "a")
dt$smokestat = NULL

colSums(is.na(dt))

# 약 복용 missing 처리 -----------------------------------------------------

#당뇨약 NA 제거완료
pat = dt[is.na(DMtrt),PATNUM]
b = dt[PATNUM %in% pat,]
c = b[,.(.(DMtrt)),by=PATNUM]
c[,V2:=sapply(c$V1, unique)]
c[,V3:=as.character(V2)]
c[,table(V3)]

table(dt$DMtrt, useNA = "ifany")
dt[PATNUM%in% pat & is.na(DMtrt), DMtrt:= 0]
table(dt$DMtrt, useNA = "ifany")

# 혈압약 NA 제거완료
pat = dt[is.na(Hypertensiontrt),PATNUM]
b = dt[PATNUM %in% pat,]
c = b[,.(.(Hypertensiontrt)),by=PATNUM]
c[,V2:=sapply(c$V1, unique)]
c[,V3:=as.character(V2)]
c[,table(V3)]

dt[PATNUM%in% pat & is.na(Hypertensiontrt), Hypertensiontrt:= 0]
table(dt$Hypertensiontrt, useNA = "ifany")
rm("b", "c" ,"pat")

colSums(is.na(dt))


# 음주량, 운동량, 혈압, 중성지방, 혈당, HDL, 허리둘레,  BMI 직전 직후 대체 ---------------------------------------------------------

# 하나도 없는 애들
pat0 = dt[,mean(HB001, na.rm = T), by = PATNUM][is.na(V1), PATNUM] 
pat1 = dt[,mean(HB002, na.rm = T), by = PATNUM][is.na(V1), PATNUM] 
pat2 = dt[,mean(L0LC2443, na.rm = T), by = PATNUM][is.na(V1), PATNUM] 
pat3 = dt[,mean(L0MC3711, na.rm = T), by = PATNUM][is.na(V1), PATNUM]
pat4 = dt[,mean(L0VC2420, na.rm = T), by = PATNUM][is.na(V1), PATNUM]
pat5 = dt[,mean(CIRABD, na.rm = T), by = PATNUM][is.na(V1), PATNUM] 
pat6 = dt[,mean(BMI, na.rm = T), by = PATNUM][is.na(V1), PATNUM] 
pat7 = dt[,mean(drinkperweek, na.rm = T), by = PATNUM][is.na(V1), PATNUM] 
pat8 = dt[,mean(E, na.rm = T), by = PATNUM][is.na(V1), PATNUM] 


# 전부 NA면 제거 9366명 제거됨...
dt[,n_distinct(PATNUM)]

patexcept = union(pat0, c(pat1, pat2, pat3, pat4, pat5, pat6, pat7, pat8))

dt = dt[!(PATNUM %in% patexcept),]

str(dt)
colSums(is.na(dt))
rm("pat0" ,"pat1", "pat2", "pat3", "pat4", "pat5", "pat6", "pat7", "pat8", "patexcept")

library(zoo)

dttmp = copy(dt)


var = "HB001"

locf<-function(dt, var){
dtt = dttmp[,.(PATNUM, get(var))]
pat = dtt[is.na(V2), PATNUM]
dtt[PATNUM%in%pat, new:= na.locf(na.locf(.SD, na.rm = F), fromLast = T), by = PATNUM, .SDcols = 'V2']
dtt[!(PATNUM%in%pat), new:= V2]
dttmp[,paste0(var,"_new") := dtt$new]
return(dttmp)
}


vars = c("HB001", "HB002", "L0LC2443", "L0MC3711",
         "L0VC2420", "CIRABD", "BMI", "drinkperweek", "E")
for (i in vars){
  dttmp = locf(dttmp, i)
}

dttmp
colSums(is.na(dttmp))


dttmp[,(vars):= lapply(.SD,as.null), .SDcols = vars]
colSums(is.na(dttmp))
dt = copy(dttmp)

list=setdiff(ls(), "dt")
rm(list=setdiff(ls(), "dt"))





n_distinct(dt$PATNUM)
#당뇨 판정 


dt[,`more200` := ifelse(L0MC3711_new>=200, 1, 0)]
dt[,`more6.5` := ifelse(C3825>=6.5, 1, 0)]
dt[,`diabetes`:= ifelse((!is.na(DMtrt)&DMtrt==1)|(!is.na(more200)&more200==1)|(!is.na(more6.5)&more6.5==1),1,0)]
dt[,table(diabetes, useNA = "always")]

# 첫 시점에 발생한 사람 제거
dt[,id:=rowid(PATNUM)]
firstocc <- dt[id==1 & diabetes==1, PATNUM]
dttmp = dt[!(PATNUM%in%firstocc),]
dttmp[,length(unique(PATNUM))]

# 데이터가 하나 뿐인 사람 제거
f1 = dttmp[,.N,by = PATNUM][N==1, PATNUM]
dttmp = dttmp[!(PATNUM%in% f1),]

# 데이터가 두개뿐이지만 같은 날인 사람 제거
a = dttmp[,.(PATNUM, ORDDAT)]
b = dttmp[duplicated(a)|duplicated(a, fromLast = T),]
pat = b[,unique(PATNUM)]

f2 = dttmp[,.N, by = PATNUM][N==2, PATNUM]
pat1 = intersect(f2,  pat)
dttmp = dttmp[!(PATNUM %in% pat1),]

# 한 날짜 두 검진 처리 ----------------------------------------------------------

a = dttmp[,.(PATNUM, ORDDAT)]
#날짜별로 봤을 때 중복되는게 있나? 38건 19명 특수일반 같이 받으신분
# 특수검진으로 고른다.
b = dttmp[duplicated(a)|duplicated(a, fromLast = T),]
dttmp = dttmp[!duplicated(a),]

#####################################


# 당뇨 발생 후 제거 ---------------------------------------------------------

dttmp[,cum1:=cumsum(diabetes), by= PATNUM]
dttmp[,cum2:=cumsum(cum1), by= PATNUM]

dttmp = dttmp[cum2<=1,]
dttmp[, n_distinct(PATNUM)]

dt=copy(dttmp)

rm(list=setdiff(ls(), "dt"))

colSums(is.na(dt))



# 나이, start, stop
# 이 부분 고민이필요한데 어떻게 할까 원래햇던대로
str(dt)
nrow(dt[diabetes == 1,])


dt[, start := as.numeric(ORDDAT - shift(ORDDAT, type = "lag")), by = PATNUM]
dt[, stop  := shift(start, type = 'lead'), by= PATNUM]
dt[is.na(start), start:=0]
dt[is.na(stop), stop:=0]
dt[, start:=cumsum(start),by= PATNUM]
dt[, stop:=cumsum(stop),by= PATNUM]

dt[,Tend:=max(stop),by=PATNUM]

num_var= c("start", "stop", "Tend")

dt[,(num_var):=lapply(.SD, as.numeric), .SDcols = num_var]

dt[, gr:=as.numeric(sum(diabetes)>0),by = PATNUM]
dt

#마지막 데이터 삭제 ???????????
dt = dt[,.SD[-.N,],by = PATNUM]
# status 정해주기

dt[,num:=rowid(PATNUM)]
dt[,filter:=(num==.N),by=PATNUM]
dt[filter==1 & gr==1, diabetes:=1]

# mean imputation
dt[diabetes==1, stop:=round((start+stop)/2)]

# 변수순서정리 및 삭제
setcolorder(dt, c("PATNUM", "start", "stop", "Tend", "diabetes", "gr",
                  "EXMKND","ORDDAT", "SEXTYP", "age", "agegr", "smoke", "drinkperweek_new", "E_new",
                  "HB001_new", "HB002_new", "L0LC2443_new", "L0MC3711_new",
                  "L0VC2420_new", "CIRABD_new", "BMI_new"))



rm_var = c("id", "cum1", "cum2", "filter", "num",
           "more200", "more6.5", 
           "HA001", "HA002", "EXMDAT", "BITDAT", "packyear",
           "dailysmokeamount", "howoftendrink", "howmuchdrink", "E1", "E2",
           "EGR")

dt[,(rm_var):=lapply(.SD,as.null), .SDcols = rm_var]
names(dt)

dt[,n_distinct(PATNUM)]
dt[gr==1,n_distinct(PATNUM)]

# 총합 0인 유해물질 제거
nox_var <- grep("^nox", names(dt), value = T)
noxrm_var = nox_var[colSums(dt[,..nox_var])==0]
dt[,(noxrm_var):=lapply(.SD, as.null), .SDcols = noxrm_var]
summary(dt)
colSums(is.na(dt))

# 변수 범주화 ------------------------------------------------------------------

# WAIST

dt[CIRABD_new>=90 & SEXTYP == "M", WAIST := "1"]
dt[CIRABD_new<90 & SEXTYP == "M", WAIST := "0"]
dt[CIRABD_new>=85 & SEXTYP == "F", WAIST:= "1"]
dt[CIRABD_new<85 & SEXTYP == "F", WAIST := "0"]

# BMI
dt[BMI_new<23, BMI:= "0"]
dt[BMI_new>=23 & BMI_new<25, BMI:= "1"]
dt[BMI_new>=25, BMI:= "2"]

# TRIGLYfac
dt[L0LC2443_new<150, TRIGLY:="0"]
dt[L0LC2443_new>=150 & L0LC2443_new<200, TRIGLY:="1"]
dt[L0LC2443_new>=200, TRIGLY:="2"]


# HDL
dt[L0VC2420_new<40, HDL:="2"]
dt[L0VC2420_new>=40 & L0VC2420_new<60, HDL:="1"]
dt[L0VC2420_new>=60, HDL:="0"]

# 혈압fac
dt[HB001_new < 120 & HB002_new < 80, BP:="0"]
dt[(HB001_new >= 120 & HB001_new < 140) | (HB002_new >= 80 & HB002_new < 90), BP := "1"]
dt[HB001_new >=140 | HB002_new >= 90, BP := "2"]

colSums(is.na(dt))

# drinkfac # 8잔 8*14 = 112
summary(dt$drinkperweek)
dt[drinkperweek_new==0, drink:="0"]
dt[drinkperweek_new>0 & drinkperweek_new<112, drink:="1"]
dt[drinkperweek_new>=112, drink:="2"]
table(dt$drinkfac)

# exercisefac
summary(dt$E_new)
dt[E_new<2, Exercise:="0"]
dt[E_new>=2 & E_new < 5, Exercise:="1"]
dt[E_new>=5, Exercise:="2"]
table(dt$Exercise)


fwrite(dt, "diabetesforsurv.csv")
