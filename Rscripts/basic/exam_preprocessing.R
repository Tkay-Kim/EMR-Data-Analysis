rm(list=ls())
#exam <- fread("C:/Users/404/Desktop/newdata/data/preprocessing/exam12_21.csv")
#exam
#exam1 = exam[,.(PATNUM, EXMKND, ORDDAT, EXMCOD, RESLT1, EXAMYY)]
#fwrite(exam1, "C:/Users/404/Desktop/newdata/data/preprocessing/processed_exam12_21.csv")

exam = fread("C:/Users/404/Desktop/newdata/data/preprocessing/processed_exam12_21.csv")

metscode = c("HA001", "HA002", "HB001", "HB002", "L0LC2443", "L0MC3711", "L0VC2420", "CIRABD", "C3825")
BEI<- fread("C:/USers/404/Desktop/MetS/Rdata/info/BEIinfo.csv")
BEIcode = BEI$code

exam1 = exam[EXMCOD %in% c(metscode, BEIcode)]

examwide = dcast.data.table(exam1, PATNUM + ORDDAT + EXMKND ~ EXMCOD, fill = NA, value.var = "RESLT1", fun.agg = function(x) {x})

examwide
setcolorder(examwide, c("PATNUM", "ORDDAT", "EXMKND", metscode, BEIcode))
fwrite(examwide, "examwide.csv")










# BEI 불러오기 ----------------------------------------------------------------
dt = fread("rawdata/imexammt.csv")
BEI <- fread("Rdata/info/BEIinfo.csv")
BEI2 <- dt[EXMCOD %in% BEI[,code]]
BEI2
BEI2[RESLT1=="<0.01",RESLT1 := 0.005] # <0.01 => 0.005

BEI2[EXMCOD==BEI[1,code],RESLT1]
BEI2[EXMCOD==BEI[1,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=0.165/2]

BEI2[EXMCOD==BEI[2,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=0.007/2]
BEI2[EXMCOD==BEI[3,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=14.360/2]
BEI2[EXMCOD==BEI[4,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=0.134/2]
BEI2[EXMCOD==BEI[5,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=0.134/2]
BEI2[EXMCOD==BEI[6,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=2.415/2]
BEI2[EXMCOD==BEI[7,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=1.167/2]
BEI2[EXMCOD==BEI[8,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=0.425/2]
BEI2[EXMCOD==BEI[9,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=1.738/2]
BEI2[EXMCOD==BEI[10,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=0.005]
BEI2[EXMCOD==BEI[11,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=0.895/2]
BEI2[EXMCOD==BEI[12,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=0.005]
BEI2[EXMCOD==BEI[13,code] & (RESLT1 == 0|is.na(RESLT1)), RESLT1:=0.017/2]

setkey(BEI, code)
setkey(BEI2, EXMCOD)
BEI3<- merge(BEI2, BEI, all.x = TRUE, by.x = "EXMCOD", by.y = "code")

BEI<- BEI3[,.(PATNUM, EXMKND, ORDDAT, BEI, RESLT1)]

BEI$PATNUM <-as.integer(BEI$PATNUM)
BEI$ORDDAT <-ymd(BEI$ORDDAT)
BEI$EXMKND <-as.integer(BEI$EXMKND)

BEI <- reshape(BEI, idvar = c("PATNUM", "ORDDAT", "EXMKND"), timevar = "BEI", direction = "wide")

exam_impol1 = merge(exam_impol1, BEI, all.x = TRUE, by = c("PATNUM", "EXMKND", "ORDDAT"))

nox = grep("^nox",names(exam_impol), value = T)
nox <- c(nox,"sum")
a = exam_impol1[,..nox]
a[is.na(a)]<-0
exam_impol1[,(nox):=a] 
apply(exam_impol1[,..nox],2,sum)

BEIname = c("BEI1","BEI2","BEI3","BEI4","BEI5","BEI6","BEI7","BEI8","BEI9","BEI10","BEI11", "BEI12", "BEI13")
names(exam_impol1)[grep("^RESLT",names(exam_impol1))] <-BEIname
names(exam_impol1)
str(exam_impol1, list.len = nrow(exam_impol1))


exam_impol1[,(BEIname):=lapply(.SD,as.numeric),.SDcols = BEIname]

exam_impol1 = exam_impol1[order(PATNUM, ORDDAT)]



