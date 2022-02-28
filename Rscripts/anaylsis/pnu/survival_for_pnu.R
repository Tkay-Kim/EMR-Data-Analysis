rm(list=ls())

library(data.table)
library(patchwork)
library(survival)
library(epiR)
library(ggplot2)
library(dplyr)

dt = fread("diabetesforsurv.csv", encoding = "UTF-8", na.strings = c("", NA))

# 변수 설정
names(dt)
nox<- names(dt)[grep("^nox", names(dt))]
fact_var = c("PATNUM", "diabetes", "EXMKND", "gr", "SEXTYP", "smoke",
             "F1", "F2", "F3", "F4", "WAIST", "BMI",
             "HDL", "TRIGLY", "BP", "agegr", "drink",
             "Exercise", nox)

dt[,(fact_var):=lapply(.SD, as.factor), .SDcols = fact_var]


num_var = c("start", "stop", "Tend", "age") 
dt[,(num_var):=lapply(.SD, as.numeric), .SDcols = num_var]
str(dt, list.len = length(names(dt)))

###########
impat<-fread("data/preprocessing/impatbat_22.csv", encoding = "UTF-8")
impat = impat[,.(PATNUM, CUSTNM)]

setkey(dt, "PATNUM")
setkey(impat, "PATNUM")

impat$PATNUM = as.character(impat$PATNUM)
dt$PATNUM = as.character(dt$PATNUM)
dttest = merge.data.table(x = dt, y = impat, all.x = T)
dt = dttest[CUSTNM%in%c("부산대학교병원", "부산대학교병원*", "양산부산대학교병원", "양산부산대학교병원*"),]



# BASELINE STAT -----------------------------------------------------------

dtb = dt[,.SD[1], by = PATNUM]

vars = c("SEXTYP", "agegr", "drink", "Exercise", "smoke",
         "WAIST", "TRIGLY", "BP", "BMI", "HDL",
         "F1", "F2", "F3", "F4")

maketable<-function(dtb, vars){
  for (i in 1:length(vars)){
    if(i==1) {
      res <- list(0)
      a = table(dtb[,get(vars[i])])
      res[[i]] = cbind(paste0(vars[i], "/", names(a)),t(t(paste(a, " (", round(prop.table(a)*100,1), ")"))))
    } 
    else {
      a = table(dtb[,get(vars[i])])
      res[[i]] = cbind(paste0(vars[i], "/", names(a)),t(t(paste(a, " (", round(prop.table(a)*100,1), ")"))))
    }
  }
  return(res)
}
res = maketable(dtb, vars)
res = do.call(rbind,res)
res

fwrite(data.table(res), "./pnu/summary_at_baseline_pnu.csv")

############# RCtable

makerctable<-function(dtb, vars){
  for (i in 1:length(vars)){
    if(i==1) {
      res <- list(0)
      a = table(dtb[,.(get(vars[i]), gr)])
      res[[i]] = cbind(a, round(prop.table(a, margin = "V1")[,2],4))
    } 
    else {
      
      a = table(dtb[,.(get(vars[i]), gr)])
      res[[i]] = cbind(a, round(prop.table(a, margin = "V1")[,2],4))
    }
  }
  return(res)
}
res = makerctable(dtb, vars)
res = do.call(rbind,res)
res = cbind(rownames(res),res)
res
fwrite(data.table(res), "./pnu/summary_at_baseline_RCtable_pnu.csv")


# confounders -------------------------------------------------------------

vars = c("SEXTYP", "agegr", "drink", "Exercise","smoke", 
         "WAIST","TRIGLY", "BP", "BMI", "HDL", 
         "F1", "F2" ,"F3", "F4")

res = list()
# 단변량

for (i in 1:length(vars)){
  cox <- coxph(Surv(start,stop, diabetes)~get(vars[i]), id = PATNUM, data = dt)
  res[[i]] <- summary(cox)$conf.int
}

res1 = do.call("rbind", res)
res1 = round(res1, 4)

cox1<- coxph(Surv(start,stop, diabetes)~ SEXTYP+ agegr + drink + 
               Exercise + smoke + 
               WAIST + TRIGLY + BP + BMI + HDL + 
               F1 + F2 + F3 + F4, 
             id = PATNUM, data = dt)

res2 = summary(cox1)$conf.int
res2 = round(res2, 4)

RES = cbind(res1, res2)
rownames(RES) = rownames(res2)
RES = cbind(rownames(RES), RES)
RES
fwrite(data.table(RES), "res_confounders_pnu.csv")


# exposures ----------------------------------------------------------------------

nox <- grep("^nox", names(dt), value = T)
a <- cbind(nox,apply(dt[,..nox]==1,2,sum))
a <- data.table(a)
names(a) <-c("nox", "count")
a$count = as.numeric(a$count)
a = a[order(-count)]
a = a[count>=100,]
a
noxinfo <- copy(a)
noxinfo

# 단변량에서 야간노동 유무 변수
pat1 = dt[,sum(nox_P0601==1)>0,by = PATNUM][V1==T, PATNUM]
dt[PATNUM%in%pat1,sum(diabetes==1)]

pat0 = dt[,sum(nox_P0601==1)>0,by = PATNUM][V1==F,PATNUM]
dt[PATNUM%in%pat0,sum(diabetes==1)]

# 단변량 노출



for (i in 1: nrow(noxinfo)){
  try(cox<- coxph(Surv(start,stop, diabetes) ~ get(noxinfo[i, nox]), data = dt, id = PATNUM), silent = T)
  noxinfo[i,ecoef:=round(summary(cox)$conf.int[1],4)]
  noxinfo[i,lower:=round(summary(cox)$conf.int[3],4)]
  noxinfo[i,upper:=round(summary(cox)$conf.int[4],4)]
  noxinfo[i,`p-value`:=round(summary(cox)$coef[1,6],4)]
}  

# 수렴 안 하는것들 삭제
noxinfo = noxinfo[!is.infinite(upper),]
noxinfo = noxinfo[ecoef!=0,]
info <- fread("noxinfo.csv")
setkey(info, NOXCOD)
setkey(noxinfo, nox)
noxinfo = info[noxinfo]

noxinfo[NOXNME %in% c("Night work", "Noise", "Radiation", "Vibration"), NOXKND:= "A. Physical"]
noxinfo[NOXNME %in% c("Pb", "Fe", "Mn", "Welding fume", "Al",
                      "Mineral dust", "Cr", "Cu", "Ni", "Zn",
                      "Firbrous glass dust", "Wood dust",
                      "지르코니움과 그 화합물", "안티몬과 그 화합물"), NOXKND:= "C. Heavy metal, dust"]
noxinfo[is.na(NOXKND), NOXKND:= "B. Organic solvent"]

noxinfo = noxinfo[order(NOXKND)]
noxinfo
setcolorder(noxinfo, c("NOXCOD",  "NOXNME", "count", "ecoef", "lower", "upper", "p-value"))

# 다변량 노출

for (i in 1: nrow(noxinfo)){
  
  try(cox<- coxph(Surv(start,stop, diabetes)~ SEXTYP+ agegr+
                    drink + Exercise + smoke +
                    F1 + F2 + F3 + F4 +
                    HDL + WAIST + BMI + TRIGLY + BP +
                    get(noxinfo[i, NOXCOD]), data = dt, id = PATNUM), silent = T)
  cox
  noxinfo[i,`ecoef(multi)`:=round(summary(cox)$conf.int[23,1],4)]
  noxinfo[i,`lower(multi)`:=round(summary(cox)$conf.int[23,3],4)]
  noxinfo[i,`upper(multi)`:=round(summary(cox)$conf.int[23,4],4)]
  noxinfo[i,`p-value(multi)`:=round(summary(cox)$coef[23,6],4)]
}    
noxinfo
fwrite(noxinfo, "./Result_pnu/res_exposures_pnu.csv")



# 세개노출 --------------------------------------------------------------------

# 세개노출에 대해서 새롭게 정의
dt[,`nox_night+radiation`:=ifelse(nox_P0601==1 & nox_09==1, 1, 0)]
dt[,`nox_night+butoxi`:=ifelse(nox_P0601==1 & nox_N0041==1, 1, 0)]
dt[,`nox_butoxi+radiation`:=ifelse(nox_N0041==1 & nox_09==1, 1, 0)]
dt[,`nox_triple`:=ifelse(nox_P0601==1 & nox_N0041==1 & nox_09==1, 1, 0)]



#Rctable

vars = c("nox_P0601", "nox_09", "nox_N0041",
         "nox_night+radiation", "nox_night+butoxi", "nox_butoxi+radiation",
         "nox_triple")

makerctable<-function(dt, vars){
  
  for (i in 1:length(vars)){
    if(i==1) {
      res <- list(0)
      dtb = dt[,.(sum(gr==1)>0, sum(get(vars[i])==1)>0),by = PATNUM]
      colnames(dtb) = c("PATNUM", "diabetes", "nox")
      a = table(dtb[,.(nox, diabetes)])
      res[[i]] = cbind(a, round(prop.table(a, margin = "nox")[,2],4))
    } 
    else {
      dtb = dt[,.(sum(gr==1)>0, sum(get(vars[i])==1)>0),by = PATNUM]
      colnames(dtb) = c("PATNUM", "diabetes", "nox")
      a = table(dtb[,.(nox, diabetes)])
      res[[i]] = cbind(a, round(prop.table(a, margin = "nox")[,2],4))
    }
  }
  return(res)
}
res = makerctable(dt, vars)
res = do.call(rbind,res)
res = cbind(rownames(res),res)
res
fwrite(data.table(res), "./Result_pnu/summary_at_baseline_RCtable_nox_pnu.csv")



# 단변량 노출

nox <- grep("^nox", names(dt), value = T)
a <- cbind(nox,apply(dt[,..nox]==1,2,sum))
a <- data.table(a)
names(a) <-c("nox", "count")
a$count = as.numeric(a$count)
a = a[order(-count)]

noxinfo <- copy(a)
noxinfo = noxinfo[nox%in%vars,]


for (i in 1: nrow(noxinfo)){
  try(cox<- coxph(Surv(start,stop, diabetes) ~ get(noxinfo[i, nox]), data = dt, id = PATNUM), silent = T)
  noxinfo[i,ecoef:=round(summary(cox)$conf.int[1],4)]
  noxinfo[i,lower:=round(summary(cox)$conf.int[3],4)]
  noxinfo[i,upper:=round(summary(cox)$conf.int[4],4)]
  noxinfo[i,`p-value`:=round(summary(cox)$coef[1,6],4)]
}  

# 수렴 안 하는것들 삭제
noxinfo = noxinfo[!is.infinite(upper),]
noxinfo = noxinfo[ecoef!=0,]
dt
i=1
for (i in 1: nrow(noxinfo)){
  
  try(cox<- coxph(Surv(start,stop, diabetes)~ SEXTYP+ agegr+
                    drink + Exercise + smoke +
                    F1 + F2 + F3 + F4 +
                    HDL + WAIST + BMI + TRIGLY + BP +
                    get(noxinfo[i, nox]), data = dt, id = PATNUM), silent = T)
  
  noxinfo[i,`ecoef(multi)`:=round(summary(cox)$conf.int[23,1],4)]
  noxinfo[i,`lower(multi)`:=round(summary(cox)$conf.int[23,3],4)]
  noxinfo[i,`upper(multi)`:=round(summary(cox)$conf.int[23,4],4)]
  noxinfo[i,`p-value(multi)`:=round(summary(cox)$coef[23,6],4)]
}    
noxinfo

fwrite(noxinfo, "./Result_pnu/res_exposures_nox3_pnu.csv")











# 방사선 유기용제 제외 -------------------------------------------------------------
pat1 = dt[nox_09 == 1,PATNUM]
pat2 = dt[nox_N0041 == 1,PATNUM]
pat = union_all(pat1, pat2)
dt = dt[!(PATNUM%in% pat),]
dt[,sum(diabetes==1)]

# BASELINE STAT -----------------------------------------------------------

dtb = dt[,.SD[1], by = PATNUM]

vars = c("SEXTYP", "agegr", "drink", "Exercise", "smoke",
         "WAIST", "TRIGLY", "BP", "BMI", "HDL",
         "F1", "F2", "F3", "F4")

maketable<-function(dtb, vars){
  for (i in 1:length(vars)){
    if(i==1) {
      res <- list(0)
      a = table(dtb[,get(vars[i])])
      res[[i]] = cbind(paste0(vars[i], "/", names(a)),t(t(paste(a, " (", round(prop.table(a)*100,1), ")"))))
    } 
    else {
      a = table(dtb[,get(vars[i])])
      res[[i]] = cbind(paste0(vars[i], "/", names(a)),t(t(paste(a, " (", round(prop.table(a)*100,1), ")"))))
    }
  }
  return(res)
}
res = maketable(dtb, vars)
res = do.call(rbind,res)
res

fwrite(data.table(res), "summary_at_baseline_pnu_wo2nox.csv")

############# RCtable
i=1
maketable<-function(dtb, vars){
  for (i in 1:length(vars)){
    if(i==1) {
      res <- list(0)
      a = table(dtb[,.(get(vars[i]), gr)])
      res[[i]] = cbind(a, round(prop.table(a, margin = "V1")[,2],4))
    } 
    else {
      
      a = table(dtb[,.(get(vars[i]), gr)])
      res[[i]] = cbind(a, round(prop.table(a, margin = "V1")[,2],4))
    }
  }
  return(res)
}
res = maketable(dtb, vars)
res = do.call(rbind,res)
res = cbind(rownames(res),res)
res
fwrite(data.table(res), "summary_at_baseline_RCtable_pnu_wo2nox.csv")


# confounders -------------------------------------------------------------

vars = c("SEXTYP", "agegr", "drink", "Exercise","smoke", 
         "WAIST","TRIGLY", "BP", "BMI", "HDL", 
         "F1", "F2" ,"F3", "F4")

res = list()
# 단변량

for (i in 1:length(vars)){
  cox <- coxph(Surv(start,stop, diabetes)~get(vars[i]), id = PATNUM, data = dt)
  res[[i]] <- summary(cox)$conf.int
}

res1 = do.call("rbind", res)
res1 = round(res1, 4)

cox1<- coxph(Surv(start,stop, diabetes)~ SEXTYP+ agegr + drink + 
               Exercise + smoke + 
               WAIST + TRIGLY + BP + BMI + HDL + 
               F1 + F2 + F3 + F4, 
             id = PATNUM, data = dt)

res2 = summary(cox1)$conf.int
res2 = round(res2, 4)

RES = cbind(res1, res2)
rownames(RES) = rownames(res2)
RES = cbind(rownames(RES), RES)
RES
fwrite(data.table(RES), "res_confounders_pnu_wo2nox.csv")


# exposures ----------------------------------------------------------------------


# 단변량 노출

cox1<- coxph(Surv(start,stop, diabetes) ~ nox_P0601, data = dt, id = PATNUM)

# 다변량 노출
cox2<- coxph(Surv(start,stop, diabetes)~ SEXTYP+ agegr+
               drink + Exercise + smoke +
               F1 + F2 + F3 + F4 +
               HDL + WAIST + BMI + TRIGLY + BP +
               nox_P0601, data = dt, id = PATNUM)


noxinfo = c(round(summary(cox1)$conf.int,4), summary(cox1)$coef[6],
            round(summary(cox2)$conf.int[23,],4), summary(cox2)$coef[23,6])
fwrite(data.table(noxinfo), "res_exposures_pnu1_wo2nox.csv")




# 야간작업만노출 vs 모두노출없음  --------------------------------------------------------------------

# 정의
a = dt[,.(.N,sum(nox_P0601==1), sum(nox_09==1), sum(nox_N0041==1)),by= PATNUM]
# 비노출
a[V2==0 & V3==0 & V4==0,gr:=0]
# 야간작업
a[V2>0 & V3==0 & V4==0,gr:=1]

table(a$gr, useNA = "ifany")
a[is.na(gr),]

pat1 = a[gr==1, PATNUM]
pat0 = a[gr==0, PATNUM]

dt1 = dt[PATNUM%in% c(pat1, pat0), ]
dt1[PATNUM%in% pat1, label := 1]
dt1[PATNUM%in% pat0, label := 0]

b = dt1[,.SD[1,],by= PATNUM]
b[,.(label, gr)]
c = table(b[,.(label, gr)])
cbind(c,prop.table(c, margin = "label")[,2])


#Rctable









# 단변량 노출

noxinfo = data.table()
  cox<- coxph(Surv(start,stop, diabetes) ~ nox_P0601, data = dt1, id = PATNUM)
  noxinfo[,ecoef:=round(summary(cox)$conf.int[1],4)]
  noxinfo[,lower:=round(summary(cox)$conf.int[3],4)]
  noxinfo[,upper:=round(summary(cox)$conf.int[4],4)]
  noxinfo[,`p-value`:=round(summary(cox)$coef[1,6],4)]
noxinfo  


# 다변량 노출
cox2<- coxph(Surv(start,stop, diabetes) ~ SEXTYP + agegr+
               drink + Exercise + smoke +
               F1 + F2 + F3 + F4 +
               HDL + WAIST + BMI + TRIGLY + BP +
               nox_P0601, data = dt1, id = PATNUM)
cox2
noxinfo[,ecoefm:=round(summary(cox2)$conf.int[23,1],4)]
noxinfo[,lowerm:=round(summary(cox2)$conf.int[23,3],4)]
noxinfo[,upperm:=round(summary(cox2)$conf.int[23,4],4)]
noxinfo[,`p-valuem`:=round(summary(cox2)$coef[23,6],4)]

noxinfo


dt2 = dt1[,.SD[.N,],by=PATNUM]

a = dt2[diabetes==1,]
a[,diff:=Tend-start]
a[,DATE:=ORDDAT+diff]
a[,.(ORDDAT, start, stop, Tend, DATE)]
a[,year(DATE)]
table(a[,year(DATE)])







