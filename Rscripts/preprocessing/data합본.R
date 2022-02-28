rm(list=ls())
library(data.table)
library(readxl)


getwd()
setwd("./data/요청자료_2020_2021/IMEXAMMT")

i=8
lists = list(0)
for (i in 1:length(list.files())){
  dt = readxl::read_xlsx(list.files()[i])
  dt = data.table(dt)
  dt = dt[!is.na(EXMDAT),]
  dt = dt[!is.na(EXMSBK),]
  if(ncol(dt)==69) dt = dt[,-"EMRTYP"]
  lists[[i]] = dt
}

data = do.call("rbind", lists)

data = data[!is.na(PATNUM),]

fwrite(data, "exam2021.csv")

data$PATNUM = as.numeric(data$PATNUM)

exam <- fread("C:/Users/404/Desktop/newdata/data/imexammt.csv")
delete = setdiff(colnames(data),colnames(exam))
data1 = data[,-..delete]


str(data1)
str(exam)
data1$SPCNO1
exam$SPCNO1 = as.character(exam$SPCNO1)

data1
exam
data2 = rbind(data1, exam)
str(data2)

fwrite(data2, "exam12_21.csv")


## judgnt(유해인자)
rm(list=ls())
getwd()
setwd("C:/Users/404/Desktop/newdata/data/요청자료_2020_2021")
list.files()
jud1 = readxl::read_xlsx("IMJUDGNT_2020.xlsx")
jud2 = readxl::read_xlsx("IMJUDGNT_2021.xlsx")
jud = rbind(jud1, jud2)
jud$PATNUM = as.numeric(jud$PATNUM)

judor<- fread("C:/Users/404/Desktop/newdata/data/imjudgnt.csv", encoding = "UTF-8")
colnames(judor)
colnames(jud)

judge = rbind(judor, jud)

fwrite(judge, "judgnt12_21.csv")


# impol(문진)


rm(list=ls())
impol12_19 <- fread("C:/Users/404/Desktop/newdata/data/impol_12_19.csv", encoding = "UTF-8")
impol20 <- read_xlsx("C:/Users/404/Desktop/newdata/data/요청자료_2020_2021/IMPOL00T_2020.xlsx")
impol21 <- read_xlsx("C:/Users/404/Desktop/newdata/data/요청자료_2020_2021/IMPOL00T_2021.xlsx")
impol20$PATNUM = as.numeric(impol20$PATNUM)
impol21$PATNUM = as.numeric(impol21$PATNUM)

names = colnames(impol12_19)
colnames(impol20) 
library(dplyr)
impol20 = impol20 %>% data.table
impol21 = impol21 %>% data.table

setdiff(colnames(impol20), names)


impol20 = impol20[,..names]
impol21 = impol21[,..names]

impol = rbind(impol12_19, impol20, impol21)
fwrite(impol, "impol12_21.csv")
impol


#impat
impat<- read_xlsx("C:/Users/404/Desktop/newdata/data/IMPATBAT_2022.xlsx")
fwrite(impat,"impatbat_22.csv")
