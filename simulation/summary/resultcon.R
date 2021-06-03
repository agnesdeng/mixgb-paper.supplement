source("/Users/agnes/Desktop/MIXGB-jcgs/Supplement/simulation/summary/functions/reportbeta.R")
source("/Users/agnes/Desktop/MIXGB-jcgs/Supplement/simulation/summary/functions/plotbias.R")
source("/Users/agnes/Desktop/MIXGB-jcgs/Supplement/simulation/summary/functions/bootstrap.R")
source("/Users/agnes/Desktop/MIXGB-jcgs/Supplement/simulation/summary/functions/plotbootvariance.R")
setwd("~/Desktop/MIXGB-jcgs/Supplement/simulation/result")
load("con31.RData")
load("con32.RData")
load("con33.RData")




# result table for continuous data generation model 1 ---------------------------


result=con31
betaresults=reportbeta(sim.result=result,model=1)
betaresults


sub.col=1:11
#beta1: intercept
beta=1
b1<-betaresults[[beta]][,sub.col]
beta=2
b2<-betaresults[[beta]][,sub.col]
beta=3
b3<-betaresults[[beta]][,sub.col]
#xtable(betaresults[[beta]][,sub.col])
beta=4
b4<-betaresults[[beta]][,sub.col]
beta=5
b5<-betaresults[[beta]][,sub.col]
beta=6
b6<-betaresults[[beta]][,sub.col]
beta=7
b7<-betaresults[[beta]][,sub.col]


multi_xtable(b1,b2,b3,b4,b5,b6,b7)


mcerrors1<-mcerrors(sim.result=con31,show.max = T)
mcerrors1
con31booterror=booterror(sim.result=con31,model=1,runs=100,seedset=SEED)
con31booterror


# result table for continuous data generation model 2 ---------------------------


result=con32
betaresults=reportbeta(sim.result=result,model=2)
betaresults


sub.col=1:11
#beta1: intercept
beta=1
b1<-betaresults[[beta]][,sub.col]
beta=2
b2<-betaresults[[beta]][,sub.col]
beta=3
b3<-betaresults[[beta]][,sub.col]
#xtable(betaresults[[beta]][,sub.col])
beta=4
b4<-betaresults[[beta]][,sub.col]
beta=5
b5<-betaresults[[beta]][,sub.col]
beta=6
b6<-betaresults[[beta]][,sub.col]
beta=7
b7<-betaresults[[beta]][,sub.col]


multi_xtable(b1,b2,b3,b4,b5,b6,b7)

mcerrors2<-mcerrors(sim.result=con32,show.max = T)
mcerrors2
con32booterror=booterror(sim.result=con32,model=2,runs=100,seedset=SEED)
con32booterror
# result table for continuous data generation model 3 ---------------------------


result=con33
betaresults=reportbeta(sim.result=result,model=3)
betaresults


sub.col=1:11
#beta1: intercept
beta=1
b1<-betaresults[[beta]][,sub.col]
beta=2
b2<-betaresults[[beta]][,sub.col]
beta=3
b3<-betaresults[[beta]][,sub.col]
#xtable(betaresults[[beta]][,sub.col])
beta=4
b4<-betaresults[[beta]][,sub.col]
beta=5
b5<-betaresults[[beta]][,sub.col]
beta=6
b6<-betaresults[[beta]][,sub.col]
beta=7
b7<-betaresults[[beta]][,sub.col]


multi_xtable(b1,b2,b3,b4,b5,b6,b7)


mcerrors3<-mcerrors(sim.result=con33,show.max = T)
mcerrors3
con33booterror=booterror(sim.result=con33,model=3,runs=100,seedset=SEED)
con33booterror


# plot result for continuous data generation model 1 ----------------------


p=plotbias(sim.result=con31,model=1,colscheme = 2)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/bias1.jpeg",
     width = 12,height=5,units = "in",res=600,pointsize=1)
print(p)
dev.off()


###
set.seed(2021, kind = "L'Ecuyer-CMRG")
SEED=round(runif(1000,min=0,max=1000000))

M.sum0=reportboot(sim.result=con31,model=1,runs=100,seedset=SEED)


p=plotbootvariance(bootresult=M.sum0,bootruns=100,model=1,pal.col = Greens[-1])
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/var1.jpeg",
     width = 12,height=6,units = "in",res=600,pointsize=1)
print(p)
dev.off()




# plot result for continuous data generation model 2 ----------------------


p=plotbias(sim.result=con32,model=2,colscheme = 2)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/bias2.jpeg",
     width = 12,height=5,units = "in",res=600,pointsize=1)
print(p)
dev.off()




M.sum2=reportboot(sim.result=con32,model=2,runs=100,seedset=SEED)

p=plotbootvariance(bootresult=M.sum2,bootruns=100,model=2,pal.col = Greens[-1])
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/var2.jpeg",
     width = 12,height=6,units = "in",res=600,pointsize=1)
print(p)
dev.off()


# plot result for continuous data generation model 3 ----------------------


p=plotbias(sim.result=con33,model=3,colscheme = 2)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/bias3.jpeg",
     width = 12,height=5,units = "in",res=600,pointsize=1)
print(p)
dev.off()



M.sum3=reportboot(sim.result=con33,model=3,runs=100,seedset=SEED)

p=plotbootvariance(bootresult=M.sum3,bootruns=100,model=3,pal.col = Greens[-1])
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/var3.jpeg",
     width = 12,height=6,units = "in",res=600,pointsize=1)
print(p)
dev.off()
