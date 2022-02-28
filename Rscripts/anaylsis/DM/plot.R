library(data.table)
library(patchwork)
library(survival)
library(epiR)
library(ggplot2)
library(dplyr)

# 그림
rm(list=ls())
RES<-fread("res_confounders.csv")
names(RES)<-c("e1", "e-", "l1", "u1", "e2", "e--", "l2", "u2")
str(RES)
num_var = names(RES)
RES[,(num_var):=lapply(.SD, as.numeric), .SDcols = num_var]
RES $ factor = c("Male",
                 "Age1", "Age2",
                 "drink1", "drink2",
                 "E1", "E2",
                 "Current", "Former",
                 "Waist1",
                 "Trigly1", "Trigly2",
                 "BP1", "BP2",
                 "BMI1", "BMI2",
                 "HDL1", "HDL2",
                 "F11", "F21", "F31", "F41")




names = c("Female", "Male",
          "Age0", "Age1", "Age2",
          "drink0", "drink1", "drink2",
          "E0", "E1", "E2",
          "Never", "Current", "Former",
          "Waist0", "Waist1",
          "Trigly0", "Trigly1", "Trigly2",
          "BP0", "BP1", "BP2",
          "BMI0", "BMI1", "BMI2",
          "HDL0", "HDL1", "HDL2",
          "F10" ,"F11", "F20", "F21",
          "F30", "F31", "F40", "F41")

res = data.table(names)

res = res %>% mutate(id = row_number())
setkey(res, "names")
setkey(RES, "factor")
a = RES[res]
a = a[order(id)]
a[l1>1, label0 := "*" ]
a[l2>1, label1 := "*" ]

psole = ggplot(a, aes(x = e1, y = factor, xmin = l1, xmax = u1 )) +
  geom_point()+ geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 1, color = "red") +
  scale_y_discrete(limits = rev(a$factor)) +
  geom_text(size = 5, aes(label = label0), vjust = 0.1, col = "red") +
  labs(x = "A. Simple", y="") +
  xlim(0,10) +## x축의 눈금표시 간격을 설정
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

pmulti = ggplot(a, aes(x = e2, y = factor, xmin = l2, xmax = u2 )) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 1, color = "red") +
  scale_y_discrete(limits = rev(a$factor))+
  geom_text(size = 5, aes(label = label1), vjust = 0.1, col = "red") +
  labs(x = "B. Multiple") +
  xlim(0,10) + ## x축의 눈금표시 간격을 설정 
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
a
psole + pmulti


# exposures ---------------------------------------------------------------

noxinfo <-fread("res_exposures.csv")
noxinfo = noxinfo[count >= 100,]
noxinfo[`p-value`<0.05, label0 := "*"]
noxinfo[`p-value(multi)`<0.05, label1 := "*"]

psole = ggplot(noxinfo, aes(x = ecoef, y = NOXNME, xmin = lower, xmax = upper )) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 1, color = "red") +
  scale_y_discrete(limits = rev(noxinfo$NOXNME))+
  geom_text(size = 5, aes(label = label0), vjust = 0.1, col = "red") +
  labs(x = "A. Simple (Exposure-only)", y="") +
  scale_x_continuous(breaks=seq(0, 10, by = 1))+ ## x축의 눈금표시 간격을 설정
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  coord_cartesian(xlim = c(0, 10)) 


pmulti = ggplot(noxinfo, aes(x = `ecoef(multi)`, y = NOXNME, xmin = `lower(multi)`,
                             xmax = `upper(multi)` )) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  geom_vline(xintercept = 1, color = "red") +
  scale_y_discrete(limits = rev(noxinfo$NOXNME))+
  geom_text(size = 5, aes(label = label1), vjust = 0.1, col = "red") +
  labs(x = "B. Mutiple (Exposure + Confonunders)", y="") +
  scale_x_continuous(breaks=seq(0, 10, by = 1))+ ## x축의 눈금표시 간격을 설정
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  coord_cartesian(xlim = c(0, 10)) 


psole + pmulti


# 복합노출 그래프 ----------------------------------------------------------------
rm(list=ls())
library(data.table)

dt <-fread("Rdata/diabetes/rescom.csv")
b<-dt[ l3>1 |l6>1,]
b<- b[e1!=0&e2!=0&e3!=0&e4!=0&e5!=0&e6!=0,]
names(b)

b[RERI>0, ADD:="†"]
b[mul>1, MUL:="‡"]
b[RERIM>0, `ADD(M)`:="†"]
b[Mmul>1, `MUL(M)`:="‡"]


# log scale plot

library(ggplot2)
plotCI<-function(i){
  p = ggplot(b, aes(x = get(paste0("e", i)), y = name,
                    xmin = get(paste0("l", i)), xmax = get(paste0("u", i))))+
    geom_point() +
    geom_errorbarh(height = 0.2) +
    geom_vline(xintercept = 1, color = "red") +
    scale_y_discrete(limits = rev(b$name))+
    labs(x = "", y="") +
    theme_classic() +      
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  if(i==1){
    p = p + labs(x = "Simple, Risk factor A") + 
      coord_cartesian(xlim = c(0.04, 25)) +
      scale_x_continuous(breaks = c(1,5,10,15,20,25), trans = "log")
  }  
  
  if(i==4){
    p = p + labs(x = "Multiple, Risk factor A") +
      coord_cartesian(xlim = c(0.04, 25)) +
      scale_x_continuous(breaks = c(1,5,10,15,20,25), trans = "log")
    
  }  
  
  if(i==2){
    p = p +  labs(x = "Simple, Risk factor B") +
      coord_cartesian(xlim = c(0.04, 25)) +
      scale_x_continuous(breaks = c(1,5,10,15,20,25), trans = "log")
  }
  if(i == 5){
    p = p +  labs(x = "Multiple, Risk factor B") +
      coord_cartesian(xlim = c(0.04, 25)) +
      scale_x_continuous(breaks = c(1,5,10,15,20,25), trans = "log")
  }
  
  if(i ==3){
    p = p + labs(x="Simple, Risk factor A & B")+
      coord_cartesian(xlim = c(0.04, 360)) +
      scale_x_continuous(breaks = c(1,5,10, 50, 100,300), trans = "log") +
      geom_text(size = 2, aes(label = ADD), vjust = -0.35, hjust =  -0.3) +
      geom_text(size = 2, aes(label = MUL), vjust = -0.35, hjust =  -1.5) 
  }  
  
  if(i == 6){
    p = p + labs(x="Multiple, Risk factor A & B") + 
      coord_cartesian(xlim = c(0.04, 360)) +
      scale_x_continuous(breaks = c(1,5,10, 50, 100, 300), trans = "log") +
      geom_text(size = 2, aes(label = `ADD(M)`), vjust = -0.35, hjust = -0.3) +
      geom_text(size = 2, aes(label = `MUL(M)`), vjust = -0.35, hjust = -1.5) 
  }
  
  return(p)
}
b[,min(l1)]
b[,min(l2)]
b[,min(l3)]
b[,min(l4)]
b[,min(l5)]
b[,min(l6)]


b[,max(u1)]
b[,max(u2)]
b[,max(u3)]
b[,max(u4)]
b[,max(u5)]
b[,max(u6)]

fwrite(b, "Rdata/diabetes/rescomsummary.csv")



p1 = plotCI(1);p1
p2 = plotCI(2);p2
p3 = plotCI(3);p3
p4 = plotCI(4);p4
p5 = plotCI(5);p5
p6 = plotCI(6);p6


library(patchwork)
p1+p2+p3
p4+p5+p6

