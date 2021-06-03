
library(microbenchmark)
library(mice)
library(misle)
library(missRanger)
library(addhazard)
data(nwtsco)
str(nwtsco)
fac.var<-c("relaps","dead","study","stage","histol","instit")
nwtsco[,fac.var]<-lapply(nwtsco[,fac.var],factor)
str(nwtsco)

M=5
s=2021
set.seed(s)

withNA.df<-createNA(nwtsco,p=0.3)


nwts.time<-microbenchmark("mice.default"={imp<-mice(withNA.df,m=M,seed=s,print=T)},
                     "mice.cart"={imp<-mice(withNA.df,m=M,seed=s,method="cart",print=T)},
                     "mice.rf"={imp<-mice(withNA.df,m=M,seed=s,method="rf",print=T)},
                     "missranger"={imp<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4),simplify=FALSE)},
                     "missranger.pmm"={imp<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4,pmm.k=5),simplify=FALSE)},
                     "mixgb"={
                       MIXGB<-Mixgb$new(withNA.df)
                       mixgb.data<-MIXGB$impute(m=M)},
                     "mixgb.pmm1"={
                       MIXGB1<-Mixgb$new(withNA.df,pmm.type=1,pmm.k = 5)
                       mixgb.data1<-MIXGB1$impute(m=M)},
                     "mixgb.pmm2"={
                       MIXGB2<-Mixgb$new(withNA.df,pmm.type=2,pmm.k = 5)
                       mixgb.data2<-MIXGB2$impute(m=M)},
                     "mixgb.auto"={
                       MIXGB3<-Mixgb$new(withNA.df,pmm.type="auto",pmm.k = 5)
                       mixgb.data3<-MIXGB3$impute(m=M)},
                     times = 10,
                     unit="s")

nwts.time
save(nwts.time,file="/Users/agnes/Desktop/MIXGB-jcgs/Supplement/time/Table3/result/result/nwts.RData")

