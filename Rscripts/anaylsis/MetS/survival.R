library(data.table)
library(patchwork)
library(survival)
library(epiR)
library(ggplot2)
library(dplyr)

rm(list=ls())
dt = fread("MetSforsurv.csv", encoding = "UTF-8", na.strings = c("", NA))
dt[,sum(MetS==1)]
# 변수 설정
names(dt)
nox<- names(dt)[grep("^nox", names(dt))]
fact_var = c("PATNUM", "MetS", "EXMKND", "gr", "SEXTYP", "smoke",
             "F1", "F2", "F3", "F4", "WAIST", 
             "HDL", "TRIGLY", "BP", "agegr", "drink",
             "Exercise", nox)

dt[,(fact_var):=lapply(.SD, as.factor), .SDcols = fact_var]


num_var = c("start", "stop", "Tend", "age") 
dt[,(num_var):=lapply(.SD, as.numeric), .SDcols = num_var]
str(dt, list.len = length(names(dt)))


dt[,n_distinct(PATNUM)]

# BASELINE STAT -----------------------------------------------------------

dtb = dt[,.SD[1], by = PATNUM]

vars = c("SEXTYP", "agegr", "smoke", "drink", "Exercise", 
         "F1", "F2", "F3", "F4")

maketable<-function(dtb, vars){
  for (i in 1:length(vars)){
  if(i==1) {
  res <- list(0)
  a = table(dtb[,get(vars[i])])
  res[[i]] = cbind(paste0(vars[i], "/", names(a)),t(t(paste0(a, " (", round(prop.table(a)*100,1), ")"))))
  } 
  else {
  a = table(dtb[,get(vars[i])])
  res[[i]] = cbind(paste0(vars[i], "/", names(a)),t(t(paste0(a, " (", round(prop.table(a)*100,1), ")"))))
    }
  }
  return(res)
}
res = maketable(dtb, vars)
res = do.call(rbind,res)
res

fwrite(data.table(res), "./Result_MetS/summary_at_baseline.csv")

# confounders -------------------------------------------------------------

vars = c("SEXTYP", "agegr", "smoke", "drink", "Exercise",
        "F1", "F2" ,"F3", "F4")

res = list()
# 단변량

for (i in 1:length(vars)){
  cox <- coxph(Surv(start, stop, MetS) ~ get(vars[i]), id = PATNUM, data = dt)
  res[[i]] <- summary(cox)$conf.int
}

res1 = do.call("rbind", res)
res1 = round(res1, 4)


cox1 <- coxph(Surv(start, stop, MetS) ~ SEXTYP + agegr + drink + 
               Exercise + smoke + 
               F1 + F2 + F3 + F4, 
               id = PATNUM, data = dt)
cox1

res2 = summary(cox1)$conf.int
res2 = round(res2, 4)

RES = cbind(res1, res2)
rownames(RES) = rownames(res2)
RES
fwrite(data.table(RES), "./Result_MetS/res_confounders.csv")





# exposures ----------------------------------------------------------------------
rm(list=ls())
dt = fread("MetSforsurv.csv", encoding = "UTF-8", na.strings = c("", NA))

# 변수 설정
names(dt)
nox<- names(dt)[grep("^nox", names(dt))]
fact_var = c("PATNUM", "MetS", "EXMKND", "gr", "SEXTYP", "smoke",
             "F1", "F2", "F3", "F4",
             "agegr", "drink", "Exercise", nox)
dt[,(fact_var):=lapply(.SD, as.factor), .SDcols = fact_var]


num_var = c("start", "stop", "Tend", "age") 
dt[,(num_var):=lapply(.SD, as.numeric), .SDcols = num_var]
str(dt, list.len = length(names(dt)))


nox <- grep("^nox", names(dt), value = T)
a <- cbind(nox,apply(dt[,..nox]==1,2,sum))
a <- data.table(a)
names(a) <-c("nox", "count")
a$count = as.numeric(a$count)
a = a[order(-count)]
a = a[count>=100,]
noxinfo <- copy(a)
noxinfo

# 단변량 노출

for (i in 1: nrow(noxinfo)){
  try(cox<- coxph(Surv(start,stop, MetS) ~ get(noxinfo[i, nox]), data = dt, id = PATNUM), silent = T)
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

# 소음(배치전) 삭제
noxinfo = noxinfo[NOXNME!= '소음(배치전)',] 
noxinfo = noxinfo[order(-count)] 


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
i=1
for (i in 1: nrow(noxinfo)){
  
  try(cox<- coxph(Surv(start,stop, MetS)~ SEXTYP + agegr+
                    drink + Exercise + smoke +
                    F1 + F2 + F3 + F4 +
                    get(noxinfo[i, NOXCOD]), data = dt, id = PATNUM), silent = T)

  noxinfo[i,`ecoef(multi)`:=round(summary(cox)$conf.int[14,1],4)]
  noxinfo[i,`lower(multi)`:=round(summary(cox)$conf.int[14,3],4)]
  noxinfo[i,`upper(multi)`:=round(summary(cox)$conf.int[14,4],4)]
  noxinfo[i,`p-value(multi)`:=round(summary(cox)$coef[14,6],4)]
}    
noxinfo

fwrite(noxinfo, "./Result_MetS/res_exposures.csv")


# 복합노출 ----------------------------------------------------------------


noxinfo <- fread("res_exposures.csv", encoding = "UTF-8")
noxinfo <- noxinfo[,.(V1,NOXCOD, NOXKND, NOXNME)]
noxinfo
nox <-noxinfo[,NOXCOD]
nox
# 노출물질 한 변수로
res = list()

for (i in 1:36){
  for (j in (i+1):37){
    noxcodA = noxinfo[i, NOXCOD]
    noxcodB = noxinfo[j, NOXCOD]
    
    
    dt[get(noxcodA)==1, noxcom := "1"]
    dt[get(noxcodB)==1, noxcom := "2"]
    dt[get(noxcodA)==1 & get(noxcodB)==1, noxcom := "3"]
    dt[!(get(noxcodA)==1 | get(noxcodB) == 1), noxcom := "0"]
    
    dt$noxcom = as.factor(dt$noxcom)
    
    names(dt)
    if(sum(dt$noxcom %in% 3) !=0){
      cox1<- coxph(Surv(start,stop,diabetes)~ noxcom, data = dt)
      cox2<- coxph(Surv(start,stop,diabetes)~SEXTYP+agefac+
                     drinkfac + Efac + smoke +
                     F1 + F2 + F3 + F4 +
                     WAISTfac + BMIfac + HDLfac + TRIGLYfac + HBfac +
                     noxcom, data = dt)
      
      RERI<-round(epi.interaction(cox1, param = "dummy" , coef = 1:3, type = "RERI", conf.level = 0.95),4)
      S<-round(epi.interaction(cox1, param = "dummy" , coef = 1:3, type = "S", conf.level = 0.95),4)
      AP<-round(epi.interaction(cox1, param = "dummy" , coef = 1:3, type = "APAB", conf.level = 0.95)*100,1)
      
      RERIM<-round(epi.interaction(cox2, param = "dummy" , coef = 23:25, type = "RERI", conf.level = 0.95),4)
      SM<-round(epi.interaction(cox2, param = "dummy" , coef = 23:25, type = "S", conf.level = 0.95),4)
      APM<-round(epi.interaction(cox2, param = "dummy" , coef = 23:25, type = "APAB", conf.level = 0.95)*100,1)
      
      res[[paste( noxinfo[i, NOXNME],"/",noxinfo[j, NOXNME])]] <-c(noxinfo[i, NOXNME], noxinfo[j, NOXNME],
                                                                   table(dt$noxcom)[2:4],
                                                                   as.vector(t(round(cbind(summary(cox1)$conf.int[,c(1, 3, 4)],summary(cox1)$coef[,5]),4))),
                                                                   RERI, S, AP,
                                                                   as.vector(t(round(cbind(summary(cox2)$conf.int[23:25,c(1, 3, 4)],summary(cox2)$coef[23:25,5]),4))),
                                                                   RERIM, SM, APM)
      
      
  
      
      
    }
  }
}

a = do.call("rbind", res)
a<-data.table(a)
names(a) <-c("Aname", "Bname", "Acount", "Bcount", "Ccount",
             "e1", "l1", "u1", "p1",
             "e2", "l2", "u2", "p2",
             "e3", "l3", "u3", "p3",
             "RERI", "RERIu", "RERIl",
             "S", "Su", "Sl",
             "AP", "APu", "APl",
             "e4", "l4", "u4", "p4",
             "e5", "l5", "u5", "p5",
             "e6", "l6", "u6", "p6",
             "RERIM", "RERIMu", "RERIMl",
             "SM", "SMu", "SMl",
             "APM", "APMu", "APMl")

# 노출물질 따로 for multiplicative CI

res1 = list()
for (i in 1:36){
  for (j in (i+1):37){
    noxcodA = noxinfo[i, NOXCOD]
    noxcodB = noxinfo[j, NOXCOD]
    dt[,noxcom:=ifelse((get(noxcodA)==1 & get(noxcodB)==1), 1, 0)]
    
    if(sum(dt$noxcom == 1)!=0){
      cox1<- coxph(Surv(start,stop,diabetes)~ get(noxcodA) + get(noxcodB) + get(noxcodA) * get(noxcodB), data = dt)
      cox2<- coxph(Surv(start,stop,diabetes)~SEXTYP+agefac+ drinkfac + Efac +
                     smoke +
                     F1 + F2 + F3 + F4 +
                     WAISTfac + BMIfac + HDLfac + TRIGLYfac + HBfac +
                     get(noxcodA) + get(noxcodB) + get(noxcodA) * get(noxcodB), data = dt)
      res1[[paste0( noxinfo[i, NOXNME],"/",noxinfo[j, NOXNME])]] <-c(noxinfo[i, NOXNME], noxinfo[j, NOXNME],
                                                                    as.vector(t(round(c(summary(cox1)$conf.int[3,c(1, 3, 4)],summary(cox1)$coef[3,5]),4))),
                                                                    as.vector(t(round(c(summary(cox2)$conf.int[25,c(1, 3, 4)],summary(cox2)$coef[25,5]),4))))
      
    }
  }
}

b = do.call("rbind", res1)
b = data.table(cbind(rownames(b),b))
names(b)<-c("A", "B", "C", "mul", "mull", "mulu", "mulp",
            "Mmul", "Mmull", "Mmulu", "Mmulp")
b<-b[,-c(1,2,3)]
df = cbind(a,b)
df$name = paste(df$Aname, "/", df$Bname)

names(df)
df
# df[AP<0,`AP*`:=NA]
# df[APM<0,`APM*`:=NA]
fwrite(df,"Rdata/diabetes/rescom.csv")


# BEI ---------------------------------------------------------------------

nrow(dt) - colSums(is.na(dt))
str(dt)
a = dt[,.(PATNUM, ORDDAT, gr,C4514591U1)]
b = a[!is.na(C4514591U1),]
table(b[,sum(gr==1)>0,by=PATNUM][,V1])
occpat <- b[,sum(gr==1) > 0,by=PATNUM][V1==T,PATNUM]
occxpat <- b[,sum(gr==1) > 0,by=PATNUM][V1==F,PATNUM]
t.test(b[PATNUM%in% occpat,C4514591U1], b[PATNUM%in% occxpat,C4514591U1])


  