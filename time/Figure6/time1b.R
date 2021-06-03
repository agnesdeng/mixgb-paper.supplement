
library(MASS)
library(microbenchmark)
library(misle)
library(missRanger)





# Data generation ---------------------------------------------------------

create.data1 <- function(Nrow = 1000) {
  #set.seed(seed = seed)
  sim.mat<-matrix(rbinom(n=Nrow*10,size=1,p=0.5),nrow=Nrow,ncol=10)
  colnames(sim.mat)<-paste0(rep("d",10),1:10)
  sim.df=data.frame(sim.mat)
  logit<-with(sim.df,0.5*d1+1*d2+0.5*d3-1*d4-1*d8+0.5*d9+1.1*d1*d2)
  p<-1/(1+exp(-logit))
  sim.df$y<-rbinom(n=Nrow,size=1,prob = p)
  all.col<-names(sim.df)
  sim.df[,all.col]<-lapply(sim.df[,all.col],factor)
  return(sim.df)
}

create.data2 <- function(Nrow = 1000) {
  #set.seed(seed = seed)
  sim.mat<-matrix(rbinom(n=Nrow*10,size=1,p=0.5),nrow=Nrow,ncol=10)
  colnames(sim.mat)<-paste0(rep("d",10),1:10)
  sim.df=data.frame(sim.mat)
  logit<-with(sim.df,0.7*d1+0.5*d2+0.7*d3-0.2*d4-1.1*d8-1.1*d9+1.1*d3*d4)
  p<-1/(1+exp(-logit))
  sim.df$y<-rbinom(n=Nrow,size=1,prob = p)

  all.col<-names(sim.df)
  sim.df[,all.col]<-lapply(sim.df[,all.col],factor)
  return(sim.df)
}

create.data3 <- function(Nrow = 1000) {
  #set.seed(seed = seed)
  sim.mat<-matrix(rbinom(n=Nrow*10,size=1,p=0.5),nrow=Nrow,ncol=10)
  colnames(sim.mat)<-paste0(rep("d",10),1:10)
  sim.df=data.frame(sim.mat)
  logit<-with(sim.df,0.4*d1+0.4*d2+0.4*d3-1.3*d4-0.2*d8-0.2*d9+1.1*d8*d9)
  p<-1/(1+exp(-logit))
  sim.df$y<-rbinom(n=Nrow,size=1,prob = p)
  all.col<-names(sim.df)
  sim.df[,all.col]<-lapply(sim.df[,all.col],factor)
  return(sim.df)
}


set.seed(2021)
data1<-create.data1(Nrow=100)
data2<-create.data2(Nrow=100)
data3<-create.data3(Nrow=100)
s1b.df<-data1
s2b.df<-cbind(d1=data1,d2=data2)
s3b.df<-cbind(d1=data1,d2=data2,d3=data3)


########



M=5

withNA.df<-createNA(s1b.df,p=0.3)


s1b<-microbenchmark("missranger100"={imp<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4),simplify=FALSE)},
                    "mixgb.auto"={
                      MIXGB3<-Mixgb$new(withNA.df,pmm.type="auto",pmm.k = 5)
                      mixgb.data3<-MIXGB3$impute(m=M)},
                   times=10,
                   unit = "s")
s1b


####################
withNA.df<-createNA(s2b.df,p=0.3)


s2b<-microbenchmark("missranger100"={imp<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4),simplify=FALSE)},
                    "mixgb.auto"={
                      MIXGB3<-Mixgb$new(withNA.df,pmm.type="auto",pmm.k = 5)
                      mixgb.data3<-MIXGB3$impute(m=M)},
                   times=10,
                   unit = "s")
s2b



############
withNA.df<-createNA(s3b.df,p=0.3)


s3b<-microbenchmark("missranger100"={imp<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4),simplify=FALSE)},
                    "mixgb.auto"={
                      MIXGB3<-Mixgb$new(withNA.df,pmm.type="auto",pmm.k = 5)
                      mixgb.data3<-MIXGB3$impute(m=M)},
                    times=10,
                    unit = "s")
s3b




#save results to the "result" folder. Please change the directory accordingly.

save(s1b,file="/home/Figure6/result/s1b.RData")
save(s2b,file="/home/Figure6/result/s2b.RData")
save(s3b,file="/home/Figure6/result/s3b.RData")


