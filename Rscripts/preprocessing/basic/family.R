
# family<- function(dt, varname){
# 
# a = dt[,.(.(get(varname))), by=PATNUM]
# a[,V2:=lapply(a$V1, unique)]
# a[,V3:=as.character(V2)]
# table(a$V3)
# 
# pat10<- a[V3 %in% c("1:0", "0:1"), PATNUM]
# pat1na<- a[V3 %in% c("c(1, NA)", "c(NA, 1)"),PATNUM]
# pat0na<- a[V3 %in% c("c(0, NA)", "c(NA, 0)"),PATNUM]
# pat10na<- a[V3 %in% c("c(1, 0, NA)", "c(1, NA, 0)",
#                       "c(0, 1, NA)", "c(0, NA, 1)",
#                       "c(NA, 1, 0)", "c(NA, 0, 1)"), PATNUM]
# dttmp <-copy(dt)
# eval(parse(text= paste("dttmp[PATNUM %in% c(pat10, pat1na, pat10na),", varname, ":= 1]")))
# eval(parse(text= paste("dttmp[PATNUM %in% pat0na,", varname, ":= 0]")))
# print(dttmp[,table(get(varname), useNA = "always")])
# return(dttmp)
# }