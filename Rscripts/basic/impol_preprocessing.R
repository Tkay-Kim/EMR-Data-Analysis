rm(list=ls())
pkgs = c("dplyr", "data.table", "gmodels", "progress", "tidyverse", "lubridate",  "gridExtra", "readxl")
lapply(pkgs, library, character.only = TRUE)



# 1. Import Data Set-------------------------------------------------------------------
impol <- fread("data/preprocessing/impol12_21.csv", encoding = "UTF-8")
impat <- fread("data/preprocessing/impatbat_22.csv", encoding = "UTF-8")
impat <- impat[,.(PATNUM, SEXTYP, BITDAT)]

# impat 중복있음
impat[duplicated(impat)]
impat[PATNUM == "96685853",]
impat[PATNUM == "106815322",]
impat = impat[!duplicated(impat)]

setkey(impol, PATNUM)
setkey(impat, PATNUM)

#impol1<- merge(impol, impat, all.x= T)
#impol2<- merge(impol, impat)
# 환자정보가 없는 경우가 있다 14건(11명)
#test = setdiff(impol1$PATNUM,impol2$PATNUM)
#impol[PATNUM %in% test,]
#length(unique(test))

#환자정보 없는 impol 삭제
impol<- merge(impol, impat)
##############################################################

test2 = impol[,.(substr(ORDDAT, 1, 4), nchar(QUE061))]

table(test2)
# 2012~2017인데 QUE061==1 or 2 or 4인 경우는 NA로 처리
impol[test2$V2 %in% c(1, 2, 4) & !(test2$V1 %in% c(2018, 2019, 2020, 2021)), QUE061 := NA]
test2 = impol[,.(substr(ORDDAT, 1, 4), nchar(QUE061))]
table(test2, useNA = "ifany")

# 2018~2021인데 QUE061 == 3인경우는 설문지가 업데이트 안된것
a = impol[test2$V2 == 3 & test2$V1 %in% c(2018, 2019, 2020, 2021),]
View(a)
table(test2, useNA = "ifany")



test1 = impol[,.(substr(ORDDAT, 1, 4), QUE053)]

table(test1, useNA = "ifany")

#QUE053 0:NA, 1:아니오 2: 예
# survey 분류--------------------------------------------------------------------
impol[nchar(QUE061) == 3 | is.na(QUE061) , survey := 1]
impol[nchar(QUE061) == 1 & QUE053 == 0 , survey := 2]
impol[nchar(QUE061) == 1 & QUE053 != 0 , survey := 3]

test3 = impol[,.(survey, substr(ORDDAT, 1, 4))]
table(test3, useNA = "ifany" )


########################################################


# 흡연 ----------------------------------------------------------------------
# smoke(1): 비흡연 smoke(2): 현재흡연 smoke(3): 과거흡연
impol[survey == 1, table(QUE041, useNA = "ifany")]
impol[survey == 1 & QUE041 == 0, QUE041:=NA]
impol[survey == 1, smoke := QUE041]

impol[survey == 2, table(QUE041, useNA = "ifany")]
impol[survey == 2 & QUE041 == 0, QUE041 := NA] # 비흡연
impol[survey == 2 & QUE041 == 1, smoke := 1] # 비흡연
impol[survey == 2 & QUE041 == 2, smoke := 3] # 과거흡연
impol[survey == 2 & QUE041 == 3, smoke := 2] # 현재흡연
impol[survey == 2, table(QUE041, useNA = "ifany")]

impol[survey == 3, table(QUE041, useNA = "ifany")]
impol[survey == 3 & QUE041 == 0, QUE041 := NA]
impol[survey == 3, table(QUE041, useNA = "ifany")]

impol[survey == 3 & QUE041 == 1, smoke := 1] # 비흡연
impol[survey == 3 & QUE041 == 2 , table(QUE042, useNA = "ifany")] # 현재흡연
impol[survey == 3 & QUE041 == 2 & QUE042 > 0, smoke := 2] # 현재흡연
impol[survey == 3 & QUE041 == 2 , table(QUE044, useNA = "ifany")] # 과거흡연
impol[survey == 3 & QUE041 == 2 & QUE044 > 0, smoke := 3] # 과거흡연

impol[,table(smoke, useNA = "ifany")]

impol[smoke == 1, packyear:=0]
impol[smoke == 2 & survey %in% c(1, 2), packyear:= QUE044 * QUE045 / 20]
impol[smoke == 2 & survey == 3, packyear:= QUE042 * QUE043 / 20] #과거흡연
impol[smoke == 3 & survey %in% c(1, 2), packyear:= QUE042 * QUE043 / 20]
impol[smoke == 3 & survey == 3, packyear:= QUE044 * QUE045 / 20]

# dailysmokeamount 현재 흡연량
impol[smoke == 1, dailysmokeamount := 0]
impol[smoke == 2 & survey %in% c(1, 2), dailysmokeamount := QUE045/20]
impol[smoke == 2 & survey == 3, dailysmokeamount := QUE043/20]
impol[smoke == 3, dailysmokeamount := 0]

impol[smoke==1, dailysmokeamount]
impol[smoke==2, dailysmokeamount]
impol[smoke==3, dailysmokeamount]

# 현재, 과거흡연자인데 흡연량이 불분명할 경우 NA
impol[smoke%in% 2:3 & packyear==0,]
impol[smoke%in% 2:3 & packyear==0, packyear := NA]

# 음주 ----------------------------------------------------------------------

# 음주(QUE051) 0: 결측치 1 : 거의 안 마심/ 2: 주 1회 /8 : 매일/ 9:오타   근거-> 1의경우 잔 수 데이터가 모두 0
# drink: 거의 마시지않는다 - > 1(49%) 
# drink: 일주일에 한 두번 - > 2(35%)
# drink: 나머지 - > 3(16%)
round((impol[survey == 1,table(QUE051)]/nrow(impol[survey == 1])) * 100 , 2)

impol[survey == 1 & QUE051 %in% 1:8, howoftendrink := QUE051-1]
impol[survey == 1 & QUE051 %in% c(0, 9), howoftendrink := NA]
impol[survey == 1, table(howoftendrink, useNA = "ifany")]


# 음주_new(QUE061) 0: 결측치 1: 일주일에   2: 한달에 3: 일년에 4: 거의 마시지 않는다.(23%) -> drink = 1 근거: 4번인경우 일주일에 몇 회 양이없다
#          일주일에 한 두번 (65%) -> drink = 2
#                      그 이상 (10%) -> drink = 3
# 1: 일주일에 몇번 2: 한달에 몇번 3: 일년에 몇번

round(impol[survey %in% c(2, 3),table(QUE061)]/ nrow(impol[survey %in% c(2, 3),]) * 100 ,2)

impol[survey%in% c(2, 3) & QUE061 == 1 , table(QT0601)]
impol[survey %in% c(2, 3) & QUE061 == 1 & QT0601 %in% 0:7, howoftendrink := QT0601]
impol[survey %in% c(2, 3) & QUE061 == 1, table(howoftendrink, useNA = "ifany")]

impol[survey %in% c(2, 3) & QUE061 == 2, table(QT0602)]
impol[survey %in% c(2, 3) & QUE061 == 2, howoftendrink := QT0602 /30 * 7]
impol[survey %in% c(2, 3) & QUE061 == 2, table(howoftendrink)]

impol[survey %in% c(2, 3) & QUE061 == 3, table(QT0603)]
impol[survey %in% c(2, 3) & QUE061 == 3, howoftendrink := QT0603 /365 * 7]
impol[survey %in% c(2, 3) & QUE061 == 3, table(howoftendrink)]

impol[survey %in% c(2, 3) & QUE061 == 4, howoftendrink := 0]

impol[!(is.na(howoftendrink)), summary(howoftendrink)]

# howoftendrink 그룹나누기
# impol[howoftendrink >= 0 & howoftendrink < 1, groupdrink := 1]
# impol[howoftendrink >= 1 & howoftendrink < 3, groupdrink := 2]
# impol[howoftendrink >= 3 , groupdrink := 3]
# 
# impol[,table(groupdrink, useNA = "ifany")]

##########################################################################

# 음주: howmuchdrink
impol[survey == 1, table(QUE052, useNA = "ifany")]
impol[survey == 1, howmuchdrink := QUE052] # 최대 101잔
impol[survey == 1 & howoftendrink != 0, table(QUE052, useNA = "ifany")]
impol[survey == 1 & howoftendrink != 0 & howmuchdrink == 0 , howmuchdrink:= NA]

colnames(impol)
impol[survey %in% c(2, 3), table(Q06111, useNA = "ifany")]#소주잔 최대 48잔
impol[survey %in% c(2, 3), table(Q06112, useNA = "ifany")]#소주병 최대 71병 -> 71병제거
impol[Q06112==71, Q06112:=NA]
impol[survey %in% c(2, 3), table(Q06112, useNA = "ifany")]#소주병 최대 71병 -> 71병제거

impol[survey %in% c(2, 3), table(Q06121)] # 맥주잔 최대 500잔->최대 32잔
impol[Q06121==500, Q06121:=NA]

impol[survey %in% c(2, 3), table(Q06122)] # 맥주병 최대 1000병 -> 최대 22병
impol[Q06122%in%c(1000, 500, 300, 71), Q06122:=NA]
impol[survey %in% c(2, 3), table(Q06122)] # 맥주병 최대 1000병 -> 최대 22병

impol[,table(Q06123)] # 맥주캔 최대 22캔
impol[Q06123 == 500 , Q06123:=NA]

impol[,table(Q06124)] # 맥주cc 최대 5000cc 1,2,5,7cc 제거
impol[Q06124%in%c(1,2,5,7, 10, 20, 50),Q06124:=NA] 

impol[survey %in% c(2, 3),table(Q06131)] # 양주잔 최대 10잔
impol[survey %in% c(2, 3),table(Q06132)] # 양주병 최대 5병

impol[survey %in% c(2, 3),table(Q06141)] # 막걸리잔 최대 10잔
impol[survey %in% c(2, 3),table(Q06142)] # 막걸리병 최대 22병 -> 최대 5병 :22가 너무 분포에서 벗어남...
impol[Q06142==22, Q06142:=NA]

impol[survey %in% c(2, 3),table(Q06151)] # 와인 최대 10잔
impol[survey %in% c(2, 3),table(Q06152)] # 와인 최대 11병 -> 최대 5병 : 11이 너무 분포에서 벗어남..
impol[Q06152==11, Q06152:=NA]


#소주->맥주->.. 순으로 있는거 채운다

impol[survey%in%c(2, 3) & Q06111 !=0 & is.na(howmuchdrink), howmuchdrink:= Q06111]
impol[survey%in%c(2, 3) & Q06112 !=0 & is.na(howmuchdrink), howmuchdrink:= Q06112 * 7]
impol[survey %in% c(2, 3), table(howmuchdrink, useNA = "ifany")]

impol[survey %in% c(2,3) & Q06121 !=0 & is.na(howmuchdrink), howmuchdrink:= Q06121]
impol[survey %in% c(2,3) & Q06122 !=0 & is.na(howmuchdrink), howmuchdrink:= Q06122]
impol[survey %in% c(2,3) & Q06123 !=0 & is.na(howmuchdrink), howmuchdrink:= Q06123]
impol[survey %in% c(2, 3), table(howmuchdrink, useNA = "ifany")]

impol$howmuchdrink = as.numeric(impol$howmuchdrink)
impol[survey%in%c(2, 3) & Q06124 !=0 & is.na(howmuchdrink), howmuchdrink:= Q06124/355]
impol[survey%in%c(2, 3) & Q06124 !=0 & is.na(howmuchdrink),]
impol[survey %in% c(2, 3),table(howmuchdrink, useNA = "ifany")]

impol[survey%in%c(2,3) & Q06131 !=0 & is.na(howmuchdrink), howmuchdrink:= Q06131]
impol[survey%in%c(2,3) & Q06132 !=0 & is.na(howmuchdrink), howmuchdrink:= Q06132 * 20]
impol[survey %in% c(2, 3),table(howmuchdrink, useNA = "ifany")]

impol[survey%in%c(2,3) & Q06141 !=0 & is.na(howmuchdrink), howmuchdrink:= Q06141]
impol[survey%in%c(2,3) & Q06142 !=0 & is.na(howmuchdrink), howmuchdrink:= Q06142 * 3]
impol[survey %in% c(2, 3),table(howmuchdrink, useNA = "ifany")]

impol[survey%in%c(2,3) & Q06141 !=0 & is.na(howmuchdrink), howmuchdrink:= Q06151]
impol[survey%in%c(2,3) & Q06142 !=0 & is.na(howmuchdrink), howmuchdrink:= Q06152 * 6]
impol[survey %in% c(2, 3),table(howmuchdrink, useNA = "ifany")]

impol[,table(howoftendrink, useNA = "ifany")]
impol[howoftendrink == 0, table(howmuchdrink)]
impol[howoftendrink == 0, howmuchdrink:=0]
impol[howoftendrink > 0, summary(howmuchdrink)]

impol[,table(howmuchdrink, useNA = "ifany")]
impol[,table(howoftendrink, useNA = "ifany")]

impol[,drinkperweek:=howoftendrink*howmuchdrink*14]
impol[,summary(drinkperweek)]
impol[drinkperweek==4550,]->a
#############################################################


# 가족력 ---------------------------------------------------------------------
#뇌.심.고.당
impol[,F1:=as.numeric(substr(QUE021, 1,1))]
impol[,F2:=as.numeric(substr(QUE021, 2,2))]
impol[,F3:=as.numeric(substr(QUE021, 3,3))]
impol[,F4:=as.numeric(substr(QUE021, 4,4))]
impol[F1==0, F1:= NA]
impol[F2==0, F2:= NA]
impol[F3==0, F3:= NA]
impol[F4==0, F4:= NA]
impol[F1 == 1|F2 == 1| F3==1| F4==1, F := 1]
impol[F1+F2+F3+F4 == 8 , F := 0]
impol[,table(F1, useNA = "ifany")]
impol[,table(F2, useNA = "ifany")]
impol[,table(F3, useNA = "ifany")]
impol[,table(F4, useNA = "ifany")]
impol[,table(F, useNA = "ifany")]

# 신체활동 --------------------------------------------------------------------
impol[survey == 1, QUE061]
impol[survey == 1, E1 := as.numeric(substr(QUE061,1,1))]
impol[survey == 1, E2 := as.numeric(substr(QUE061,2,2))]

impol[survey %in% 2:3 , E1 := QT0711]
impol[survey %in% 2:3 , E2 := QT0811]



# 0:결측치 1:없음 2: 1일 8: 매일
impol[survey  == 1 , table(E1 ,useNA = "ifany")]
#          0:없음 1: 1번 7: 매일
impol[survey %in% 2:3 , table(E1, useNA = "ifany")]

# 0:결측치 1:없음 2: 1일 8: 매일 9:오타
impol[survey  == 1 , table(E2 ,useNA = "ifany")]
#          0:없음 1: 1번 7: 매일
impol[survey %in% 2:3 , table(E2, useNA = "ifany")]


impol[, E1:= as.numeric(E1)]
impol[, E2:= as.numeric(E2)]

impol[survey == 1 , E1:= E1-1]
impol[survey == 1 , E2:= E2-1]

# 0:없음 1: 1일 7: 매일 9:오타
impol[survey  == 1 , table(E1 ,useNA = "ifany")]
impol[survey %in% 2:3 , table(E1, useNA = "ifany")]

# 0:없음 1: 1일 7: 매일
impol[survey  == 1 , table(E2 ,useNA = "ifany")]
impol[E2 %in% c(-1, 8), E2 := NA]
impol[survey  == 1 , table(E2 ,useNA = "ifany")]
impol[survey %in% 2:3 , table(E2, useNA = "ifany")]

impol[,E:= E1+E2]
impol[,table(E)]/nrow(impol) * 100

impol[E == 0 , EGR := 1]
impol[E %in% 1:4 , EGR := 2]
impol[E %in% 5:14 , EGR := 3]
impol[,table(EGR, useNA = "ifany")]






# 전입일자 없는 데이터 분리 ---------------------------------------------------------
# impol = impol[!(is.na(JNIPDT)), ]


# 전입일자 < 검사일자 분리 -----------------------------------------------------------


#impol[, ORDDAT:=ymd(impol$ORDDAT)]
#impol[, PATTYP:=ymd(impol$PATTYP)]
#impol[, JNIPDT:=ymd(impol$JNIPDT)]
#impol = impol[JNIPDT<ORDDAT,]


# nrow(impol[is.na(JNIPDT),])
# impol[!(is.na(JNIPDT)), JNIPDT][nchar(impol[!(is.na(JNIPDT)), JNIPDT]) == 7]
# 
# 
# impol[, JNIPDT1:=ymd(impol$JNIPDT)]
# impol[JNIPDT == 1110823, JNIPDT1]

#impol[,age_jnip := (JNIPDT - PATTYP)/365.25]

# 전입기준 나이로 고정
impol$ORDDAT = ymd(impol$ORDDAT)

impol$BITDAT = ymd(impol$BITDAT)


impol[, age:= .SD[1,ORDDAT] - BITDAT  , by = PATNUM]
impol[,age:= age/365.25]
impol$age = as.numeric(impol$age)
impol[,agegr := cut(impol$age, breaks = c(0, 30, 50, 100), right = F)]
str(impol)
impol[is.na(impol$agegr),]




#impol[,workyear:= age_ORD - age_jnip]
#impol[,workyear_gr:= floor(workyear)]

table(impol$agegr, useNA = "ifany")
## 100세는 테스트라서 삭제
impol = impol[!is.na(agegr),]

table(impol$SEXTYP, useNA = "ifany")
table(impol$F1, useNA = "ifany")
table(impol$F2, useNA = "ifany")
table(impol$F3, useNA = "ifany")
table(impol$F4, useNA = "ifany")
table(impol$`F`, useNA = "ifany")
#가족력 데이터 없으면 삭제
#impol = impol[!(is.na(F1)) & F1 != 0,]
#impol = impol[!(is.na(F2)) & F2 != 0,]
#impol = impol[!(is.na(F3)) & F3 != 0,]
#impol = impol[!(is.na(F4)) & F4 != 0,]
#impol = impol[!(is.na(F)),]
# 가족력 0: 없음 1: 있음으로 통일
impol[F1==2 ,F1 := 0]
impol[F2==2 ,F2 := 0]
impol[F3==2 ,F3 := 0]
impol[F4==2 ,F4 := 0]
table(impol$F1, useNA = "ifany")
table(impol$F2, useNA = "ifany")
table(impol$F3, useNA = "ifany")
table(impol$F4, useNA = "ifany")
table(impol$`F`, useNA = "ifany")


table(impol$drink, useNA = "ifany")
table(impol$howoftendrink, useNA = "ifany")
table(impol$howmuchdrink, useNA = "ifany")
table(impol$smoke, useNA = "ifany")
table(impol$packyear, useNA = "ifany")
table(impol$EGR, useNA = "ifany")

#약복용 체크
#뇌졸중/심장병/고혈압/당뇨병/고지혈증/기타(암포함)/폐결핵
impol$Hypertensiontrt = substr(impol$QUE012, 3, 3)
impol$DMtrt = substr(impol$QUE012, 4, 4)

impol[Hypertensiontrt==2 , Hypertensiontrt := 0]

impol[DMtrt==2 , DMtrt := 0]

tail(impol$QUE012)
tail(impol$Hypertensiontrt)
tail(impol$DMtrt)
#음주력 흡연력 근무기간 운동력 없으면 삭제
# impol = impol[!(is.na(drink)),]
# impol = impol[!(is.na(howmuchdrink)),]
# impol = impol[!(is.na(howoftendrink)),]
# impol = impol[!(is.na(smoke)),]
# impol = impol[!(is.na(packyear)),]
# impol = impol[!(is.na(EGR)),]




colSums(is.na(impol))
# 설문지항목 제거하기
rm_var = grep("^Q",names(impol), value = T)
impol[,(rm_var):=lapply(.SD, as.null), .SDcols=rm_var ]
impol




filter1 = duplicated(impol[,.(PATNUM, ORDDAT, EXMKND)])
filter2 = duplicated(impol[,.(PATNUM, ORDDAT, EXMKND)], fromLast = T)
filter = filter1 | filter2

#똑같은게 있다
impol = impol[!filter1, ]
fwrite(impol, "./data/preprocessing/impol_preprocessed.csv")




