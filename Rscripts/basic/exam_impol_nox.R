rm(list=ls())
library(data.table)
library(dplyr)
library(lubridate)
exam_impol <- fread("./data/preprocessing/exam_impol.csv", encoding = "UTF-8", na.strings = c("", NA))

colSums(is.na(exam_impol))

n_distinct(exam_impol$PATNUM)

nox<- fread("./data/preprocessing/nox_preprocessed.csv", encoding = "UTF-8")

nox$ORDDAT = ymd(nox$ORDDAT)
setkeyv(exam_impol, c("PATNUM", "ORDDAT", "EXMKND"))
setkeyv(nox, c("PATNUM", "ORDDAT", "EXMKND"))


str(nox)
exam_impol_nox = merge.data.table(exam_impol, nox, all.x = T)

noxnames = grep("^nox", colnames(exam_impol_nox), value = T)
other = setdiff(names(exam_impol_nox), noxnames)
mask = is.na(exam_impol_nox)
mask[,other]<-FALSE
exam_impol_nox[mask]<-0

fwrite(exam_impol_nox, "./data/preprocessing/exam_impol_nox.csv")




