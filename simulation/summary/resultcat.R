source("/Users/agnes/Desktop/MIXGB-jcgs/Supplement/simulation/summary/functions/reportalpha.R")
source("/Users/agnes/Desktop/MIXGB-jcgs/Supplement/simulation/summary/functions/plotbias.R")
source("/Users/agnes/Desktop/MIXGB-jcgs/Supplement/simulation/summary/functions/bootstrapcat.R")
source("/Users/agnes/Desktop/MIXGB-jcgs/Supplement/simulation/summary/functions/plotbootvariance.R")
setwd("~/Desktop/MIXGB-jcgs/Supplement/simulation/result")

#load simulation results for categorical data generation model 3.4-3.6
load("cat34.RData")
load("cat35.RData")
load("cat36.RData")


# result for categorical data generation model 1 ---------------------------


result=cat34
alpharesults=reportalpha(sim.result=result,model=1)
alpharesults

sub.col=1:11
#alpha1: intercept
alpha=1
b1<-alpharesults[[alpha]][,sub.col]
alpha=2
b2<-alpharesults[[alpha]][,sub.col]
alpha=3
b3<-alpharesults[[alpha]][,sub.col]
#xtable(alpharesults[[alpha]][,sub.col])
alpha=4
b4<-alpharesults[[alpha]][,sub.col]
alpha=5
b5<-alpharesults[[alpha]][,sub.col]
alpha=6
b6<-alpharesults[[alpha]][,sub.col]
alpha=7
b7<-alpharesults[[alpha]][,sub.col]

alpha=8
b8<-alpharesults[[alpha]][,sub.col]

multi_xtable(b1,b2,b3,b4,b5,b6,b7,b8)

mcerrors1<-mcerrors.cat(sim.result=cat34,show.max = T)
mcerrors1
cat34booterror=booterror.cat(sim.result=cat34,runs=100,seedset=SEED)
cat34booterror



# result for continuous data generation model 2 ---------------------------


result=cat35
alpharesults=reportalpha(sim.result=result,model=2)
alpharesults


sub.col=1:11
#alpha1: intercept
alpha=1
b1<-alpharesults[[alpha]][,sub.col]
alpha=2
b2<-alpharesults[[alpha]][,sub.col]
alpha=3
b3<-alpharesults[[alpha]][,sub.col]
#xtable(alpharesults[[alpha]][,sub.col])
alpha=4
b4<-alpharesults[[alpha]][,sub.col]
alpha=5
b5<-alpharesults[[alpha]][,sub.col]
alpha=6
b6<-alpharesults[[alpha]][,sub.col]
alpha=7
b7<-alpharesults[[alpha]][,sub.col]
alpha=8
b8<-alpharesults[[alpha]][,sub.col]

multi_xtable(b1,b2,b3,b4,b5,b6,b7,b8)

mcerrors2<-mcerrors.cat(sim.result=cat35,show.max = T)
mcerrors2
cat35booterror=booterror.cat(sim.result=cat35,runs=100,seedset=SEED)
cat35booterror




# result for continuous data generation model 3 ---------------------------
result=cat36
alpharesults=reportalpha(sim.result=result,model=3)
alpharesults


sub.col=1:11
#alpha1: intercept
alpha=1
b1<-alpharesults[[alpha]][,sub.col]
alpha=2
b2<-alpharesults[[alpha]][,sub.col]
alpha=3
b3<-alpharesults[[alpha]][,sub.col]
#xtable(alpharesults[[alpha]][,sub.col])
alpha=4
b4<-alpharesults[[alpha]][,sub.col]
alpha=5
b5<-alpharesults[[alpha]][,sub.col]
alpha=6
b6<-alpharesults[[alpha]][,sub.col]
alpha=7
b7<-alpharesults[[alpha]][,sub.col]
alpha=8
b8<-alpharesults[[alpha]][,sub.col]

multi_xtable(b1,b2,b3,b4,b5,b6,b7,b8)

mcerrors3<-mcerrors.cat(sim.result=cat36,show.max = T)
mcerrors3
cat36booterror=booterror.cat(sim.result=cat36,runs=100,seedset=SEED)
cat36booterror





##########bias

p=plotbias.cat(sim.result=cat34,model=1,effect = "l",colscheme = 2)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/biascat1.jpeg",
     width = 12,height=5,units = "in",res=600,pointsize=1)
print(p)
dev.off()

p=plotbias.cat(sim.result=cat35,model=2,effect = "l",colscheme = 2)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/biascat2.jpeg",
     width = 12,height=5,units = "in",res=600,pointsize=1)
print(p)
dev.off()


p=plotbias.cat(sim.result=cat36,model=3,effect = "l",colscheme = 2)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/biascat3.jpeg",
     width = 12,height=5,units = "in",res=600,pointsize=1)
print(p)
dev.off()



#plot variance with bootstrap SE

M.sum34=reportboot.cat(sim.result=cat34,runs=100,seedset=SEED)
p=plotbootvariance.cat(bootresult=M.sum34,bootruns=100,model=1,pal.col = Greens)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/varcat1.jpeg",
     width = 12,height=6,units = "in",res=600,pointsize=1)
print(p)
dev.off()



M.sum35=reportboot.cat(sim.result=cat35,runs=100,seedset=SEED)
p=plotbootvariance.cat(bootresult=M.sum35,bootruns=100,model=2,pal.col = Greens)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/varcat2.jpeg",
     width = 12,height=6,units = "in",res=600,pointsize=1)
print(p)
dev.off()


M.sum36=reportboot.cat(sim.result=cat36,runs=100,seedset=SEED)
p=plotbootvariance.cat(bootresult=M.sum36,bootruns=100,model=3,pal.col = Greens)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/varcat3.jpeg",
     width = 12,height=6,units = "in",res=600,pointsize=1)
print(p)
dev.off()




#######report
result=bin12
betaresults=reportbeta.bin(sim.result=result,model=1,effect="l")
betaresults

result=bin22
betaresults=reportbeta.bin(sim.result=result,model=2,effect="l")
betaresults


result=bin32s
betaresults1=reportbeta.bin(sim.result=result,model=3,effect="s")
betaresults1

result=bin32s
betaresults2=reportbeta.bin(sim.result=result,model=3,effect="l")
betaresults2

betaresults1[[8]]
betaresults2[[8]]
