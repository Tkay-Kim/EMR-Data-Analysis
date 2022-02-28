rm(list=ls())
library(data.table)
exam <- fread("./data/preprocessing/examwide.csv", encoding = "UTF-8", na.strings = c("", NA))
impol <- fread("./data/preprocessing/impol_preprocessed.csv", encoding = "UTF-8")

colSums(is.na(impol))
colSums(is.na(exam))






library(lubridate)
exam$ORDDAT <-ymd(exam$ORDDAT)
str(exam)
str(impol)
setkeyv(exam, c("PATNUM", "ORDDAT", "EXMKND"))
setkeyv(impol, c("PATNUM", "ORDDAT", "EXMKND"))

exam_impol = merge.data.table(impol, exam)

colSums(is.na(exam_impol))

fwrite(exam_impol, "./data/preprocessing/exam_impol.csv")
