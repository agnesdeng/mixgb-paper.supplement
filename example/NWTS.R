

# Load the NWTS dataset from the "addhazard" package ----------------------

library(addhazard)
data(nwtsco)
str(nwtsco)
fac.var<-c("relaps","dead","study","stage","histol","instit")
nwtsco[,fac.var]<-lapply(nwtsco[,fac.var],factor)
str(nwtsco)

# Create 20% MAR data in "histol" and "tumdiam" that depends on trel -----------------------------

mar.data<-function(complete.df=nwtsco,depend.on="trel"){
  #log odds of missing
  prop=function(beta0){
    mean(1/(1+exp(-beta0-complete.df[,depend.on])))
  }
  propdiff=function(beta0){
    mean(1/(1+exp(-beta0-complete.df[,depend.on])))-0.2
  }
  #we want to fine beta0 such that prop=0.2
  beta0<-uniroot(propdiff,lower=-100,upper=100)$root
  prop(beta0)

  logit.na<-with(complete.df,beta0+complete.df[,depend.on])
  #prob of missing
  prob.na<-exp(logit.na)/(exp(logit.na)+1)

    y.miss<-rbinom(nrow(complete.df),1,prob.na)
    complete.df[,"histol"][y.miss==1]<-NA

    prop=function(beta0){
      mean(1/(1+exp(beta0-complete.df[,depend.on])))
    }
    propdiff=function(beta0){
      mean(1/(1+exp(beta0-complete.df[,depend.on])))-0.2
    }
    #we want to find beta0 such that prop=0.2
    beta0<-uniroot(propdiff,lower=-100,upper=100)$root
    prop(beta0)

    logit.na<-with(complete.df,beta0-complete.df[,depend.on])
    #prob of missing
    prob.na<-exp(logit.na)/(exp(logit.na)+1)

    y.miss<-rbinom(nrow(complete.df),1,prob.na)
    complete.df[,"tumdiam"][y.miss==1]<-NA

  complete.df
}


# NWTS dataset with missing values ---------------------------------------


withNA.df=mar.data()


#If users only want to use multiple imputation through XGBoost, we recommend to install R package "mixgb" instead of "misle"
#devtools::install_github("agnesdeng/mixgb")


#library(misle)
library(mixgb)
MIXGB=Mixgb$new(withNA.df,pmm.type="auto")
mixgb.data=MIXGB$impute(m=5)



# Generate some plots -----------------------------------------------------

source("/Users/agnes/Desktop/MIXGB-jcgs/Supplement/example/plotfun.R")

data.df<-nwtsco

#for factor variable
p1=plotfac(var.name="histol",data=data.df,imp = mixgb.data)
grid.draw(p1,recording = T)

#save it as jpeg
p1=plotfac(var.name="histol",data=data.df,imp = mixgb.data)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/nwts1.jpeg",
     width = 15,height=3,units = "in",res=600,pointsize=1)
grid.draw(p1,recording = T)
dev.off()

#for numeric variable
p2=plotnum(var.name="tumdiam",data=data.df,imp = mixgb.data)
grid.draw(p2,recording = T)

#save it as jpeg
p2=plotnum(var.name="tumdiam",data=data.df,imp = mixgb.data)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/nwts2.jpeg",
    width = 15,height=3,units = "in",res=600,pointsize=1)
grid.draw(p2,recording = T)
dev.off()


#conditional plot  (numeric vs numeric)
p3=plotconnum(var.name1="specwgt",var.name2="tumdiam",data=data.df,imp=mixgb.data,alpha=0.4)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/nwts3.jpeg",
    width = 15,height=3,units = "in",res=600,pointsize=1)
grid.draw(p3,recording = T)
dev.off()

#conditional plot  (numeric vs factor)
p4=plotcon1fac1num(num.name="tumdiam",fac.name="histol",data=data.df,imp=mixgb.data)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/nwts4.jpeg",
     width = 15,height=3,units = "in",res=600,pointsize=1)
grid.draw(p4,recording = T)
dev.off()

#conditional plot  (factor vs factor)
p5=plotconfac(var.name1="stage",var.name2="histol",data=data.df,imp=mixgb.data)
jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/nwts5.jpeg",
    width = 15,height=3,units = "in",res=600,pointsize=1)
grid.draw(p5,recording = T)
dev.off()


