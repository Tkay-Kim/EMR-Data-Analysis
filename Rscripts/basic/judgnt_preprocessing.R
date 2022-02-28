rm(list=ls())

nox = fread("C:/Users/404/Desktop/newdata/data/preprocessing/judgnt12_21.csv", encoding = "UTF-8")
nox = nox[NOXTYP == "1", .(PATNUM, ORDDAT, EXMKND, NOXCOD, NOXNME)]

?dcast.data.table
#nox[NOXCOD == "노말헥",]
noxwide = dcast.data.table(nox, PATNUM + ORDDAT + EXMKND ~ NOXCOD, fill = "0", value.var = "NOXNME", fun.agg = function(x) x, )
noxwide$노말헥 = NULL



noxwide
names = colnames(noxwide)
names = setdiff(names, c("PATNUM", "ORDDAT", "EXMKND"))
noxnames = paste0("nox_", names)
finalnames = c("PATNUM", "ORDDAT", "EXMKND", noxnames)
colnames(noxwide) = finalnames

mask = noxwide!=0
mask[,1:3]<-FALSE
mask
noxwide[mask] = "1"
noxwide

noxnames = grep("^nox", colnames(noxwide), value = T)
onlynox = noxwide[,..noxnames]
onlynox[,(noxnames):=lapply(.SD, as.numeric), .SDcols =noxnames]
str(onlynox)

nox_order = names(sort(colSums(onlynox), decreasing = T))
setcolorder(noxwide, c("PATNUM", "ORDDAT", "EXMKND", nox_order))


fwrite(noxwide, "./data/preprocessing/nox_preprocessed.csv")

