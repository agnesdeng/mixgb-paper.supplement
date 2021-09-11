
library(MASS)
library(microbenchmark)
#library(misle)
#If users only want to use multiple imputation through XGBoost, we recommend to install R package "mixgb" instead of "misle"
#devtools::install_github("agnesdeng/mixgb")
library(mixgb)
library(missRanger)





# Data generation ---------------------------------------------------------

create.data1 <- function(Nrow = 1000) {
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


create.data2 <- function(Nrow = 1000) {
  #set.seed(seed = seed)
  sim.mat1<-matrix(rbinom(n=Nrow*6,size=1,p=0.5),nrow=Nrow,ncol=6)
  colnames(sim.mat1)<-paste0(rep("d",6),c(1,2,3,4,8,9))
  sim.mat2<-matrix(rbinom(n=Nrow*4,size=2,p=0.5),nrow=Nrow,ncol=4)
  colnames(sim.mat2)<-paste0(rep("d",4),c(5,6,7,10))
  sim.mat<-cbind(sim.mat1,sim.mat2)
  sim.mat<-sim.mat[,c(1,2,3,4,7,8,9,5,6,10)]
  sim.df=data.frame(sim.mat)
  logit<-with(sim.df,0.7*d1+0.5*d2+0.7*d3-0.2*d4-1.1*d8-1.1*d9+1.1*d3*d4)
  p<-1/(1+exp(-logit))
  sim.df$y<-rbinom(n=Nrow,size=1,prob = p)

  return(sim.df)
}


create.data3 <- function(Nrow = 1000) {
  #set.seed(seed = seed)
  sim.mat1<-matrix(rbinom(n=Nrow*6,size=1,p=0.5),nrow=Nrow,ncol=6)
  colnames(sim.mat1)<-paste0(rep("d",6),c(1,2,3,4,8,9))
  sim.mat2<-matrix(rbinom(n=Nrow*4,size=2,p=0.5),nrow=Nrow,ncol=4)
  colnames(sim.mat2)<-paste0(rep("d",4),c(5,6,7,10))
  sim.mat<-cbind(sim.mat1,sim.mat2)
  sim.mat<-sim.mat[,c(1,2,3,4,7,8,9,5,6,10)]
  sim.df=data.frame(sim.mat)
  logit<-with(sim.df,0.4*d1+0.4*d2+0.4*d3-1.3*d4-0.2*d8-0.2*d9+1.1*d8*d9)
  p<-1/(1+exp(-logit))
  sim.df$y<-rbinom(n=Nrow,size=1,prob = p)
  return(sim.df)
}

set.seed(2021)
data1<-create.data1(Nrow=100000)
data2<-create.data2(Nrow=100000)
data3<-create.data3(Nrow=100000)
xl1c.df<-data1
xl2c.df<-cbind(d1=data1,d2=data2)
xl3c.df<-cbind(d1=data1,d2=data2,d3=data3)


########



M=5

withNA.df<-createNA(xl1c.df,p=0.3)


xl1c<-microbenchmark("missranger100"={imp<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4),simplify=FALSE)},
                     "mixgb.auto"={
                       MIXGB3<-Mixgb$new(withNA.df,pmm.type="auto",pmm.k = 5)
                       mixgb.data3<-MIXGB3$impute(m=M)},
                     times=10,
                     unit = "s")
xl1c




####################
withNA.df<-createNA(xl2c.df,p=0.3)


xl2c<-microbenchmark("missranger100"={imp<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4),simplify=FALSE)},
                     "mixgb.auto"={
                       MIXGB3<-Mixgb$new(withNA.df,pmm.type="auto",pmm.k = 5)
                       mixgb.data3<-MIXGB3$impute(m=M)},
                     times=10,
                     unit = "s")
xl2c


############
withNA.df<-createNA(xl3c.df,p=0.3)


xl3c<-microbenchmark("missranger100"={imp<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4),simplify=FALSE)},
                     "mixgb.auto"={
                       MIXGB3<-Mixgb$new(withNA.df,pmm.type="auto",pmm.k = 5)
                       mixgb.data3<-MIXGB3$impute(m=M)},
                     times=10,
                     unit = "s")
xl3c





#save results to the "result" folder. Please change the directory accordingly.

save(xl1c,file="/home/Figure6/result/xl1c.RData")
save(xl2c,file="/home/Figure6/result/xl2c.RData")
save(xl3c,file="/home/Figure6/result/xl3c.RData")


