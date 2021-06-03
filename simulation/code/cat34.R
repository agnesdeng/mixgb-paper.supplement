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




#1st categorical generation model; large effect size; create 20% MCAR in d3 and d4 and create 20% MAR in Y (depends on x9 and x10)

# SEED set for reproducible results-------------------------------------------------------------
set.seed(2020, kind = "L'Ecuyer-CMRG")
SEED=round(runif(1000,min=0,max=1000000))


################Large effect size

# Data generation ---------------------------------------------------------


#seed=SEED[1]
create.data <- function(Nrow = 1000) {
  #set.seed(seed = seed)
  sim.mat1<-matrix(rbinom(n=Nrow*6,size=1,p=0.5),nrow=Nrow,ncol=6)
  colnames(sim.mat1)<-paste0(rep("d",6),c(1,2,3,4,8,9))
  sim.mat2<-matrix(rbinom(n=Nrow*4,size=2,p=0.5),nrow=Nrow,ncol=4)
  colnames(sim.mat2)<-paste0(rep("d",4),c(5,6,7,10))
  sim.mat<-cbind(sim.mat1,sim.mat2)
  sim.mat<-sim.mat[,c(1,2,3,4,7,8,9,5,6,10)]
  sim.df=data.frame(sim.mat)
  logit<-with(sim.df,0.5*d1+1*d2+0.5*d3-1*d4-1*d8+0.5*d9+1.1*d1*d2)
  p<-1/(1+exp(-logit))
  sim.df$y<-rbinom(n=Nrow,size=1,prob = p)
  return(sim.df)
}




# Create 20% MAR data ---------------------------------------------------------

mar.data<-function(complete.df){
  #log odds of missing
  prop=function(alpha0){
    mean(1/(1+exp(-alpha0-complete.df$d9-complete.df$d10)))
  }
  propdiff=function(alpha0){
    mean(1/(1+exp(-alpha0-complete.df$d9-complete.df$d10)))-0.2
  }
  #we want to find alpha0 such that prop=0.2
  alpha0<-uniroot(propdiff,lower=-4,upper=-2)$root
  prop(alpha0)

  logit.na<-with(complete.df,alpha0+d9+d10)
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
sim<-function(runs=10,M=20,seedset=SEED){

  p<-8
  hbeta<-numeric(p*runs)
  hbeta.var<-numeric(p*runs)

  mice.LOGREG<-matrix(NA,nrow = p*runs,ncol=9)
  mice.LOGREGBOOT<-matrix(NA,nrow = p*runs,ncol=9)
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
    complete.df<-create.data(Nrow=1000)
    #create 20% MAR in Y (depends on d9 and d10)
    withNA.df<-mar.data(complete.df)
    #create 20% MCAR in d3 and d4
    withNA.df<-createNA(withNA.df,names=c("d3","d4"),p=c(0.2,0.2))


    all.col<-names(complete.df)
    complete.df[,all.col]<-lapply(complete.df[,all.col],factor)

    glm.fit<-glm(y ~ d1+d2+d3+d4+d8+d9+d1:d2,data=complete.df,family=binomial(link = "logit"))
    #coef of the lm model
    hbeta[from:to]<-coef(glm.fit)
    #variance of hbeta
    hbeta.var[from:to]<-diag(vcov(glm.fit))

    withNA.df[,all.col]<-lapply(withNA.df[,all.col],factor)




    #mice default (pmm)
    imp<-mice(withNA.df,m=M,print=FALSE,method = "logreg")
    a<-complete(imp,action="all")
    a<-as.list(a)
    imputed.list<-imputationList(a)
    mice.logreg<-with(imputed.list,glm(y~d1+d2+d3+d4+d8+d9+d1:d2,family=binomial()))
    mice.LOGREG[from:to,]<-summary(micombine(mice.logreg))

    #mice (logreg.boot)
    imp<-mice(withNA.df,method="logreg.boot",m=M,print=FALSE)
    a<-complete(imp,action="all")
    a<-as.list(a)
    imputed.list<-imputationList(a)
    mice.logregboot<-with(imputed.list,glm(y~d1+d2+d3+d4+d8+d9+d1:d2,family=binomial()))
    mice.LOGREGBOOT[from:to,]<-summary(micombine(mice.logregboot))


    #mice (cart)
    imp<-mice(withNA.df,method="cart",m=M,print=FALSE)
    a<-complete(imp,action="all")
    a<-as.list(a)
    imputed.list<-imputationList(a)
    mice.cart<-with(imputed.list,glm(y~d1+d2+d3+d4+d8+d9+d1:d2,family=binomial()))
    mice.CART[from:to,]<-summary(micombine(mice.cart))

    #mice (rf)
    imp<-mice(withNA.df,method="rf",m=M,print=FALSE)
    a<-complete(imp,action="all")
    a<-as.list(a)
    imputed.list<-imputationList(a)
    mice.rf<-with(imputed.list,glm(y~d1+d2+d3+d4+d8+d9+d1:d2,family=binomial()))
    mice.RF[from:to,]<-summary(micombine(mice.rf))

    #missranger
    ranger.data<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4,pmm.k=5),simplify=FALSE)
    imputed.list<-imputationList(ranger.data)
    missranger<-with(imputed.list,glm(y~d1+d2+d3+d4+d8+d9+d1:d2,family=binomial()))
    MISSRANGER[from:to,]<-summary(micombine(missranger))


    #mixgb
    MIXGB<-Mixgb$new(withNA.df)
    mixgb.data<-MIXGB$impute(m=M)
    imputed.list<-imputationList(mixgb.data)
    mixgb.pmm0<-with(imputed.list,glm(y~d1+d2+d3+d4+d8+d9+d1:d2,family=binomial()))
    mixgb.PMM0[from:to,]<-summary(micombine(mixgb.pmm0))

    #mixgb1
    MIXGB1<-Mixgb$new(withNA.df,pmm.type=1,pmm.k = 5)
    mixgb.data1<-MIXGB1$impute(m=M)
    imputed.list<-imputationList(mixgb.data1)
    mixgb.pmm1<-with(imputed.list,glm(y~d1+d2+d3+d4+d8+d9+d1:d2,family=binomial()))
    mixgb.PMM1[from:to,]<-summary(micombine(mixgb.pmm1))

    #mixgb2
    MIXGB2<-Mixgb$new(withNA.df,pmm.type=2,pmm.k = 5)
    mixgb.data2<-MIXGB2$impute(m=M)
    imputed.list<-imputationList(mixgb.data2)
    mixgb.pmm2<-with(imputed.list,glm(y~d1+d2+d3+d4+d8+d9+d1:d2,family=binomial()))
    mixgb.PMM2[from:to,]<-summary(micombine(mixgb.pmm2))

  }

  Names<-c("comb.coef","total.var","t.se","(t.lower","t.upper)","within","between","missinfo","lambda")
  colnames(mice.LOGREG)<-Names
  colnames(mice.LOGREGBOOT)<-Names
  colnames(mice.CART)<-Names
  colnames(mice.RF)<-Names
  colnames(MISSRANGER)<-Names
  colnames(mixgb.PMM0)<-Names
  colnames(mixgb.PMM1)<-Names
  colnames(mixgb.PMM2)<-Names
  L<-list(mice.LOGREG=mice.LOGREG,mice.LOGREGBOOT=mice.LOGREGBOOT,mice.CART=mice.CART,mice.RF=mice.RF,missranger=MISSRANGER,mixgb.PMM0=mixgb.PMM0,mixgb.PMM1=mixgb.PMM1,mixgb.PMM2=mixgb.PMM2)
  L<-lapply(1:length(L),function(idx){cbind(L[[idx]],hbeta,hbeta.var)})
  return(L)

}




#####multiple imputation M=20
cat34<-sim(runs=1000,M=20,seedset=SEED)
save(cat34,file="/home/simulation/result/cat34.RData")



