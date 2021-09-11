
library(MASS)
library(microbenchmark)
#library(misle)
#If users only want to use multiple imputation through XGBoost, we recommend to install R package "mixgb" instead of "misle"
#devtools::install_github("agnesdeng/mixgb")
library(mixgb)
library(missRanger)





# Data generation ---------------------------------------------------------
cov.m<-matrix(0,ncol = 10,nrow = 10)
cov.m[1:4,1:4]<-0.5
cov.m[5:10,5:10]<-0.3
diag(cov.m)<-1
cov.m


create.data1 <- function(Nrow = 1000,cov.matrix=cov.m,sd=1) {
  sim.mat<-mvrnorm(n=Nrow, mu=rep(0, 10), Sigma=cov.matrix)
  colnames(sim.mat)<-paste0(rep("x",10),1:10)
  sim.df=data.frame(sim.mat)
  sim.df$y<-with(sim.df,0.31*x1+0.31*x2+0.31*x3+0.31*x8+0.31*x9+0.28*x3^2+rnorm(Nrow,0,sd))
  return(sim.df)
}


create.data2 <- function(Nrow = 1000,cov.matrix=cov.m,sd=1) {

  sim.mat<-mvrnorm(n=Nrow, mu=rep(0, 10), Sigma=cov.matrix)
  colnames(sim.mat)<-paste0(rep("x",10),1:10)
  sim.df=data.frame(sim.mat)
  sim.df$y<-with(sim.df,0.31*x1+0.31*x2+0.31*x3+0.31*x8+0.31*x9+0.35*x1*x2+rnorm(Nrow,0,sd))
  return(sim.df)
}

create.data3 <- function(Nrow = 1000,cov.matrix=cov.m,sd=1) {
  sim.mat<-mvrnorm(n=Nrow, mu=rep(0, 10), Sigma=cov.matrix)
  colnames(sim.mat)<-paste0(rep("x",10),1:10)
  sim.df=data.frame(sim.mat)
  sim.df$y<-with(sim.df,0.31*x1+0.31*x2+0.31*x3+0.31*x8+0.31*x9+0.37*x8*x9+rnorm(Nrow,0,sd))
  return(sim.df)
}


set.seed(2021)
data1<-create.data1(Nrow=100000,cov.matrix = cov.m)
data2<-create.data2(Nrow=100000,cov.matrix = cov.m)
data3<-create.data3(Nrow=100000,cov.matrix = cov.m)
xl1.df<-data1
xl2.df<-cbind(d1=data1,d2=data2)
xl3.df<-cbind(d1=data1,d2=data2,d3=data3)


########



M=5

withNA.df<-createNA(xl1.df,p=0.3)


xl1<-microbenchmark("missranger100"={imp<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4,pmm.k=5),simplify=FALSE)},
                    "mixgb.auto"={
                      MIXGB3<-Mixgb$new(withNA.df,pmm.type="auto",pmm.k = 5)
                      mixgb.data3<-MIXGB3$impute(m=M)},
                    times=10,
                    unit = "s")
xl1


####################
withNA.df<-createNA(xl2.df,p=0.3)


xl2<-microbenchmark("missranger100"={imp<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4,pmm.k=5),simplify=FALSE)},
                    "mixgb.auto"={
                      MIXGB3<-Mixgb$new(withNA.df,pmm.type="auto",pmm.k = 5)
                      mixgb.data3<-MIXGB3$impute(m=M)},
                    times=10,
                    unit = "s")
xl2




############
withNA.df<-createNA(xl3.df,p=0.3)

xl3<-microbenchmark("missranger100"={imp<-replicate(n=M,missRanger(data=withNA.df,num.trees=100,num.threads=4,pmm.k=5),simplify=FALSE)},
                    "mixgb.auto"={
                      MIXGB3<-Mixgb$new(withNA.df,pmm.type="auto",pmm.k = 5)
                      mixgb.data3<-MIXGB3$impute(m=M)},
                    times=10,
                    unit = "s")
xl3






#save results to the "result" folder. Please change the directory accordingly.

save(xl1,file="/home/Figure6/result/xl1.RData")
save(xl2,file="/home/Figure6/result/xl2.RData")
save(xl3,file="/home/Figure6/result/xl3.RData")


