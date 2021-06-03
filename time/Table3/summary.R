library(dplyr)
library(microbenchmark)

setwd("/Users/agnes/Desktop/MIXGB-jcgs/Supplement/time/Table3/result")

load("soybean.RData")
load("nwts.RData")
load("nhanes.RData")
load("credit.RData")
load("adult.RData")


# soybean -----------------------------------------------------------------

soybean.time
print(soybean.time)


ns_s=1000000000

time<-soybean.time$time/ns_s
methods<-soybean.time$expr
result.df<-data.frame(methods=methods,time=time)

library(dplyr)
soybean.sum<-result.df %>%
  dplyr::group_by(methods) %>%
  dplyr::summarise(t.mean=mean(time),t.sd=sd(time),count=n()) %>%
  dplyr::mutate(t.se=t.sd/sqrt(count),
                t.ci=qt(p=0.975,df=count-1)*t.se)
soybean.sum

print(soybean.time)


# nwts -----------------------------------------------------------------

nwts.time
print(nwts.time)


ns_s=1000000000

time<-nwts.time$time/ns_s
methods<-nwts.time$expr
result.df<-data.frame(methods=methods,time=time)


nwts.sum<-result.df %>%
  dplyr::group_by(methods) %>%
  dplyr::summarise(t.mean=mean(time),t.sd=sd(time),count=n()) %>%
  dplyr::mutate(t.se=t.sd/sqrt(count),
                t.ci=qt(p=0.975,df=count-1)*t.se)
nwts.sum

print(nwts.time)


# nhanes -----------------------------------------------------------------

nhanes.time
print(nhanes.time)

ns_s=1000000000

time<-nhanes.time$time/ns_s
methods<-nhanes.time$expr
result.df<-data.frame(methods=methods,time=time)


nhanes.sum<-result.df %>%
  dplyr::group_by(methods) %>%
  dplyr::summarise(t.mean=mean(time),t.sd=sd(time),count=n()) %>%
  dplyr::mutate(t.se=t.sd/sqrt(count),
                t.ci=qt(p=0.975,df=count-1)*t.se)
nhanes.sum

print(nhanes.time)



# credit -----------------------------------------------------------------

credit.time
print(credit.time)


ns_s=1000000000

time<-credit.time$time/ns_s
methods<-credit.time$expr
result.df<-data.frame(methods=methods,time=time)


credit.sum<-result.df %>%
  dplyr::group_by(methods) %>%
  dplyr::summarise(t.mean=mean(time),t.sd=sd(time),count=n()) %>%
  dplyr::mutate(t.se=t.sd/sqrt(count),
                t.ci=qt(p=0.975,df=count-1)*t.se)
credit.sum

print(credit.time)


# adult -----------------------------------------------------------------

adult.time
print(adult.time)


ns_s=1000000000

time<-adult.time$time/ns_s
methods<-adult.time$expr
result.df<-data.frame(methods=methods,time=time)


adult.sum<-result.df %>%
  dplyr::group_by(methods) %>%
  dplyr::summarise(t.mean=mean(time),t.sd=sd(time),count=n()) %>%
  dplyr::mutate(t.se=t.sd/sqrt(count),
                t.ci=qt(p=0.975,df=count-1)*t.se)
adult.sum

print(adult.time)







