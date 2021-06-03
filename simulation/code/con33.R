setwd("/home/simulation/code")
source("micombine.R")

library(MASS)
library(mitools)
library(mice)
library(missRanger)
##To properly install "misle", please check https://github.com/agnesdeng/misle 
#library(devtools)
#install_github("agnesdeng/misle")
library(misle)


#3rd continuous generation model; medium effect size; create 20% MCAR in x2 and x3 and create 20% MAR in Y (depends on x9 and x10)

# SEED set for reproducible results-------------------------------------------------------------
set.seed(2020, kind = "L'Ecuyer-CMRG")
SEED=round(runif(1000,min=0,max=1000000))


# Data generation ---------------------------------------------------------
cov.m<-matrix(0,ncol = 10,nrow = 10)
cov.m[1:4,1:4]<-0.5
cov.m[5:10,5:10]<-0.3
diag(cov.m)<-1
cov.m

create.data <- function(Nrow = 1000,cov.matrix=cov.m,sd=1) {
  sim.mat<-mvrnorm(n=Nrow, mu=rep(0, 10), Sigma=cov.matrix)
  colnames(sim.mat)<-paste0(rep("x",10),1:10)
  sim.df=data.frame(sim.mat)
  sim.df$y<-with(sim.df,0.31*x1+0.31*x2+0.31*x3+0.31*x8+0.31*x9+0.37*x8*x9+rnorm(Nrow,0,sd))
  return(sim.df)
}



# Create 20% MAR data ---------------------------------------------------------


mar.data<-function(complete.df){
  #log odds of missing
  prop=function(beta0){
    mean(1/(1+exp(-beta0-complete.df$x9-complete.df$x10)))
  }
  propdiff=function(beta0){
    mean(1/(1+exp(-beta0-complete.df$x9-complete.df$x10)))-0.2
  }
  #we want to fine beta0 such that prop=0.2
  beta0<-uniroot(propdiff,lower=-3,upper=-1)$root
  prop(beta0)

  logit.na<-with(complete.df,beta0+x9+x10)
  #prob of missing
  prob.na<-exp(logit.na)/(exp(logit.na)+1)
  #we want prop=sum(prob.na)/1000=0.2
  #create missing at random in y

  y.miss<-rbinom(nrow(complete.df),1,prob.na)
  complete.df$y[y.miss==1]<-NA
  complete.df
}


# Simulation --------------------------------------------------------------
#M: number of imputation
sim<-function(Sd=1,runs=10,M=20,seedset=SEED){


  p<-7
  hbeta<-numeric(p*runs)
  hbeta.var<-numeric(p*runs)
  mice.PMM<-matrix(NA,nrow = p*runs,ncol=9)
  mice.MIDASTOUCH<-matrix(NA,nrow = p*runs,ncol=9)
  mice.CART<-matrix(NA,nrow = p*runs,ncol=9)
  mice.RF<-matrix(NA,nrow = p*runs,ncol=9)
  MISSRANGER<-matrix(NA,nrow = p*runs,ncol=9)
  mixgb.PMM0<-matrix(NA,nrow = p*runs,ncol=9)
  mixgb.PMM1<-matrix(NA,nrow = p*runs,ncol=9)
  mixgb.PMM2<-matrix(NA,nrow = p*runs,ncol=9)

  for(r in 1:runs){
    from=p*(r-1)+1
    to=p*r

    s<-seedset[r]
    set.seed(s)
    complete.df<-create.data(Nrow=1000,cov.matrix = cov.m,sd=Sd)
    lm.fit<-lm(y ~ x1+x2+x3+x8+x9+x8:x9,data=complete.df)
    #coef of the lm model
    hbeta[from:to]<-coef(lm.fit)
    #variance of hbeta
    hbeta.var[from:to]<-diag(vcov(lm.fit))
    #create 20% MAR in Y (depends on x9 and x10)
    withNA.df<-mar.data(complete.df)
    #create 20% MCAR in x2 and x3
    withNA.df<-createNA(withNA.df,names=c("x2","x3"),p=c(0.2,0.2))

    #mice default (pmm)
    imp<-mice(withNA.df,m=M,print=FALSE)
    a<-complete(imp,action="all")
    a<-as.list(a)
    imputed.list<-imputationList(a)
    mice.pmm<-with(imputed.list,lm(y ~ x1+x2+x3+x8+x9+x8:x9))
    mice.PMM[from:to,]<-summary(micombine(mice.pmm))


    #mice (midastouch)
    imp<-mice(withNA.df,method="midastouch",m=M,print=FALSE)
    a<-complete(imp,action="all")
    a<-as.list(a)
    imputed.list<-imputationList(a)
    mice.midastouch<-with(imputed.list,lm(y ~ x1+x2+x3+x8+x9+x8:x9))
    mice.MIDASTOUCH[from:to,]<-summary(micombine(mice.midastouch))


    #mice (cart)
    imp<-mice(withNA.df,method="cart",m=M,print=FALSE)
    a<-complete(imp,action="all")
    a<-as.list(a)
    imputed.list<-imputationList(a)
    mice.cart<-with(imputed.list,lm(y ~ x1+x2+x3+x8+x9+x8:x9))
    mice.CART[from:to,]<-summary(micombine(mice.cart))

    #mice (rf)
    imp<-mice(withNA.df,method="rf",m=M,print=FALSE)
    a<-complete(imp,action="all")
    a<-as.list(a)
    imputed.list<-imputationList(a)
    mice.rf<-with(imputed.list,lm(y ~ x1+x2+x3+x8+x9+x8:x9))
    mice.RF[from:to,]<-summary(micombine(mice.rf))

    #missranger
    ranger.data<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4,pmm.k=5),simplify=FALSE)
    imputed.list<-imputationList(ranger.data)
    missranger<-with(imputed.list,lm(y ~ x1+x2+x3+x8+x9+x8:x9))
    MISSRANGER[from:to,]<-summary(micombine(missranger))

    #mixgb
    MIXGB<-Mixgb$new(withNA.df)
    mixgb.data<-MIXGB$impute(m=M)
    imputed.list<-imputationList(mixgb.data)
    mixgb.pmm0<-with(imputed.list,lm(y ~ x1+x2+x3+x8+x9+x8:x9))
    mixgb.PMM0[from:to,]<-summary(micombine(mixgb.pmm0))

    #mixgb1
    MIXGB1<-Mixgb$new(withNA.df,pmm.type=1,pmm.k = 5)
    mixgb.data1<-MIXGB1$impute(m=M)
    imputed.list<-imputationList(mixgb.data1)
    mixgb.pmm1<-with(imputed.list,lm(y ~ x1+x2+x3+x8+x9+x8:x9))
    mixgb.PMM1[from:to,]<-summary(micombine(mixgb.pmm1))

    #mixgb2
    MIXGB2<-Mixgb$new(withNA.df,pmm.type=2,pmm.k = 5)
    mixgb.data2<-MIXGB2$impute(m=M)
    imputed.list<-imputationList(mixgb.data2)
    mixgb.pmm2<-with(imputed.list,lm(y ~ x1+x2+x3+x8+x9+x8:x9))
    mixgb.PMM2[from:to,]<-summary(micombine(mixgb.pmm2))

  }

  Names<-c("comb.coef","total.var","t.se","(t.lower","t.upper)","within","between","missinfo","lambda")
  colnames(mice.PMM)<-Names
  colnames(mice.MIDASTOUCH)<-Names
  colnames(mice.CART)<-Names
  colnames(mice.RF)<-Names
  colnames(MISSRANGER)<-Names
  colnames(mixgb.PMM0)<-Names
  colnames(mixgb.PMM1)<-Names
  colnames(mixgb.PMM2)<-Names
  L<-list(mice.PMM=mice.PMM,mice.MIDASTOUCH=mice.MIDASTOUCH,mice.CART=mice.CART,mice.RF=mice.RF,missranger=MISSRANGER,mixgb.PMM0=mixgb.PMM0,mixgb.PMM1=mixgb.PMM1,mixgb.PMM2=mixgb.PMM2)
  L<-lapply(1:length(L),function(idx){cbind(L[[idx]],hbeta,hbeta.var)})
  return(L)
}





# Final results -----------------------------------------------------------

#####multiple imputation M=20

con33<-sim(Sd=1,runs=1000,M=20,seedset=SEED)
save(con33,file="/home/simulation/result/con33.RData")




