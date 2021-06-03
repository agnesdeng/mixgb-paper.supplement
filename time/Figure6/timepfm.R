setwd("/Users/agnes/Desktop/MIXGB-jcgs/Supplement/time/Figure6/result")


# continuous --------------------------------------------------------------


load("s1.RData")
load("s2.RData")
load("s3.RData")
load("m1.RData")
load("m2.RData")
load("m3.RData")
load("l1.RData")
load("l2.RData")
load("l3.RData")
load("xl1.RData")
load("xl2.RData")
load("xl3.RData")
#######

library(dplyr)
#############
ns_s=1000000000
combinecon.df<-rbind(s1,s2,s3,m1,m2,m3,l1,l2,l3,xl1,xl2,xl3)
combinecon.df$time<-log(combinecon.df$time/ns_s)
combinecon.df$num.obs<-factor(rep(c(100,1000,10000,100000),each=60))
combinecon.df$num.fea<-factor(rep(rep(c("11 features","21 features","31 features"),each=20),times=4))


colnames(combinecon.df)[1]<-c("Methods")
levels(combinecon.df$Methods)<-c("missRanger","mixgb.auto")

combinecon.sum<-combinecon.df %>%
  dplyr::group_by(Methods,num.obs,num.fea) %>%
  dplyr::summarise(t.mean=mean(time),t.sd=sd(time),count=n()) %>%
  dplyr::mutate(t.se=t.sd/sqrt(count),
                t.ci=qt(p=0.975,df=count-1)*t.se)



#########binary

# binary ------------------------------------------------------------------


load("s1b.RData")
load("s2b.RData")
load("s3b.RData")
load("m1b.RData")
load("m2b.RData")
load("m3b.RData")
load("l1b.RData")
load("l2b.RData")
load("l3b.RData")
load("xl1b.RData")
load("xl2b.RData")
load("xl3b.RData")

#use dplyr:: to avoid conflicts
#############
ns_s=1000000000

combinebin.df<-rbind(s1b,s2b,s3b,m1b,m2b,m3b,l1b,l2b,l3b,xl1b,xl2b,xl3b)
combinebin.df$time<-log(combinebin.df$time/ns_s)
combinebin.df$num.obs<-factor(rep(c(100,1000,10000,100000),each=60))
combinebin.df$num.fea<-factor(rep(rep(c("11 features","21 features","31 features"),each=20),times=4))


colnames(combinebin.df)[1]<-c("Methods")

levels(combinebin.df$Methods)<-c("missRanger","mixgb.auto")

combinebin.sum<-combinebin.df %>%
  dplyr::group_by(Methods,num.obs,num.fea) %>%
  dplyr::summarise(t.mean=mean(time),t.sd=sd(time),count=n()) %>%
  dplyr::mutate(t.se=t.sd/sqrt(count),
                t.ci=qt(p=0.975,df=count-1)*t.se)


# a mix of binary and 3-levels categorical variables -------------------------------------------------------------------


load("s1c.RData")
load("s2c.RData")
load("s3c.RData")
load("m1c.RData")
load("m2c.RData")
load("m3c.RData")
load("l1c.RData")
load("l2c.RData")
load("l3c.RData")
load("xl1c.RData")
load("xl2c.RData")
load("xl3c.RData")

#use dplyr:: to avoid conflicts
#############
ns_s=1000000000

combinebin.df<-rbind(s1c,s2c,s3c,m1c,m2c,m3c,l1c,l2c,l3c,xl1c,xl2c,xl3c)
combinebin.df$time<-log(combinebin.df$time/ns_s)
combinebin.df$num.obs<-factor(rep(c(100,1000,10000,100000),each=60))
combinebin.df$num.fea<-factor(rep(rep(c("11 features","21 features","31 features"),each=20),times=4))


colnames(combinebin.df)[1]<-c("Methods")

levels(combinebin.df$Methods)<-c("missRanger","mixgb.auto")

combinebin.sum2<-combinebin.df %>%
  dplyr::group_by(Methods,num.obs,num.fea) %>%
  dplyr::summarise(t.mean=mean(time),t.sd=sd(time),count=n()) %>%
  dplyr::mutate(t.se=t.sd/sqrt(count),
                t.ci=qt(p=0.975,df=count-1)*t.se)

combine.sum<-rbind(combinecon.sum,combinebin.sum,combinebin.sum2)
combine.sum$var.type<-rep(c("continuous","binary","mixed"),each=24)
combine.sum$var.type<-factor(combine.sum$var.type,levels=c("continuous","binary","mixed"))



# plot results (Figure 6)  ------------------------------------------------


library(ggplot2)
p=ggplot(combine.sum,aes(x=num.obs,y=t.mean,group=Methods,shape=Methods,linetype=Methods))+
  geom_errorbar(aes(ymin=t.mean-t.ci,ymax=t.mean+t.ci,colour=Methods),width=0.5)+
  geom_line(aes(colour=Methods,linetype=Methods))+
  geom_point(aes(colour=Methods,shape=Methods,size=Methods))+
  ylab("Log time (seconds)")+
  xlab("Number of observations")+
  scale_x_discrete(labels=c(expression(10^2),expression(10^3),expression(10^4),expression(10^5)))+
  #facet_wrap(~num.fea,nrow=1)+
  facet_grid(var.type~num.fea)+
  scale_color_manual(values=c('#999999','#E69F00'))+
  scale_shape_manual(values=c(16,17))+
  scale_size_manual(values=c(4,4))+
  scale_linetype_manual(values=c("dashed","solid"))+
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18),
        axis.text=element_text(size=13),
        axis.title.x = element_text(size=18,margin=margin(t=10,r=0,b=0,l=0)),
        axis.title.y = element_text(size=18,margin=margin(0,r=10,0,l=10)),
        legend.key.width=unit(0.5,"in"),
        legend.title = element_text(size = 18,face="bold"),
        legend.text = element_text(size = 15),
        legend.key = element_rect(size = 15),
        legend.key.height = unit(0.5, "in"))

jpeg(filename = "/Users/agnes/Desktop/xgboost/jcgs/timepfm.jpeg",
     width = 12,height=12,units = "in",res=600,pointsize=1)
print(p)
dev.off()

