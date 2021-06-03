library(ggplot2)
library(grid)
##########
plotnum=function(var.name="age",data=insurance,imp=mixgb.data2){
  idx=which(is.na(withNA.df[,var.name]))
  true=data[,var.name][idx]
  observed=data[,var.name][-idx]
  na.num=length(idx)
  obs.num=length(observed)
  #dim(sapply(mixgb.data2,"[[",var.name ))
  #lapply(mixgb.data2,"[",var.name )[[1]]
  L=lapply(imp,"[[",var.name)
  L1=lapply(L,function(x){x[idx]})
  imputed=unlist(L1)
  imputed=c(observed,true,imputed)
  set=c(rep("Observed",obs.num),rep("Masked true",na.num),rep(paste("Imputed-",1:5,sep=""),each=na.num))
  set=factor(set,levels=c("Observed","Masked true",paste("Imputed-",1:5,sep="")))
  
  var.df=data.frame(imputed=imputed,set=set)
  #cbind(var.df$imputed[var.df$set=="Masked true"],var.df$imputed[var.df$set=="Imputed-1"])
  #r=range(var.df$imputed[var.df$set=="Masked true"])
  #hist(var.df$imputed[var.df$set=="Masked true"],breaks = 100,xlim=r)
  #hist(var.df$imputed[var.df$set=="Imputed-4"],breaks = 100,xlim=r)

  plot1=ggplot(var.df,aes(x=imputed))+
    geom_histogram(aes(y=..density..),bins=20,color="white",fill="orange2")+
    geom_density(alpha=.2,fill="orange")+
    #scale_fill_brewer(palette="Paired",name ="DEFAULT")+
    xlab(paste("variable:",var.name))+
    facet_wrap(~set,nrow=1)+
    theme(strip.text.x = element_text(size = 18),
          axis.text=element_text(size=13),
          axis.title = element_text(size=18))

  #theme(strip.background = element_rect(fill=c("orange",rep("gray",5))))
  #facet_wrap(~set,nrow=1)
  plot1

  g <- ggplot_gtable(ggplot_build(plot1))

  stripr<- which(grepl('strip-t', g$layout$name))
  stripr
  #fills="gray60"
  #k=1
  #i=stripr[1]
  #j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  #g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  #k <- k+1
  fills<-c("gray70","gray70",rep("gray90",5))
  k <- 1
  for (i in stripr){
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  g

}


library(RColorBrewer)
f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
blues<- f("Blues")[c(5,6,7,8)]

plotfac=function(var.name="sex",data=insurance,imp=mixgb.data2){
  idx=which(is.na(withNA.df[,var.name]))
  true=data[,var.name][idx]
  observed=data[,var.name][-idx]
  na.num=length(idx)
  obs.num=length(observed)
  #dim(sapply(mixgb.data2,"[[",var.name ))
  #lapply(mixgb.data2,"[",var.name )[[1]]
  L=lapply(imp,"[[",var.name)
  L1=lapply(L,function(x){x[idx]})
  imputed=unlist(L1)
  imputed=as.factor(c(observed,true,imputed))
  set=c(rep("Observed",obs.num),rep("Masked true",na.num),rep(paste("Imputed-",1:5,sep=""),each=na.num))
  set=factor(set,levels=c("Observed","Masked true",paste("Imputed-",1:5,sep="")))
  
  var.df=data.frame(imputed=imputed,set=set)
  #cbind(var.df$imputed[var.df$set=="Masked true"],var.df$imputed[var.df$set=="Imputed-1"])
  #r=range(var.df$imputed[var.df$set=="Masked true"])
  #hist(var.df$imputed[var.df$set=="Masked true"],breaks = 100,xlim=r)
  #hist(var.df$imputed[var.df$set=="Imputed-4"],breaks = 100,xlim=r)
  
  plot1=ggplot(var.df,aes(imputed,fill=imputed))+
    geom_bar()+
    #scale_fill_manual(values=pal)+
    scale_fill_brewer(palette="Paired",name =var.name)+
    #scale_fill_brewer(palette="Paired",name =var.name)+
    xlab(paste("variable:",var.name))+
    coord_flip()+
    facet_wrap(~set,nrow=1,scales="free_x")+
    theme(strip.text.x = element_text(size = 18),
          axis.text=element_text(size=13),
          axis.title = element_text(size=18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 15))
  #theme(strip.background = element_rect(fill=c("orange",rep("gray",5))))
  #facet_wrap(~set,nrow=1)
  plot1

  g <- ggplot_gtable(ggplot_build(plot1))

  stripr<- which(grepl('strip-t', g$layout$name))
  stripr
  #fills="gray60"
  #k=1
  #i=stripr[1]
  #j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  #g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  #k <- k+1
  fills<-c("gray70","gray70",rep("gray90",5))
  k <- 1
  for (i in stripr){
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  g

}


#
####conditional two factor  (only "histol" has missing values )

plotconfac=function(var.name1="stage",var.name2="histol",data=nwts,imp=mixgb.data2){
  idx=which(is.na(withNA.df[,var.name2]))
  true=data[,var.name2][idx]
  observed2=data[,var.name2][-idx]
  observed1=data[,var.name1][-idx]
  na.num=length(idx)
  obs.num=length(observed2)
  #dim(sapply(mixgb.data2,"[[",var.name ))
  #lapply(mixgb.data2,"[",var.name )[[1]]
  L=lapply(imp,"[[",var.name2)
  L1=lapply(L,function(x){x[idx]})
  imputed=unlist(L1)
  imputed=as.factor(c(observed2,true,imputed))
  set=c(rep("Observed",obs.num),rep("Masked true",na.num),rep(paste("Imputed-",1:5,sep=""),each=na.num))
  set=factor(set,levels=c("Observed","Masked true",paste("Imputed-",1:5,sep="")))
  condon=c(as.factor(observed1),rep(as.factor(data[,var.name1][idx]),6))
  var.df=data.frame(impupted=imputed,condon=condon,set=set)
  #cbind(var.df$imputed[var.df$set=="Masked true"],var.df$imputed[var.df$set=="Imputed-1"])
  #r=range(var.df$imputed[var.df$set=="Masked true"])
  #hist(var.df$imputed[var.df$set=="Masked true"],breaks = 100,xlim=r)
  #hist(var.df$imputed[var.df$set=="Imputed-4"],breaks = 100,xlim=r)
  #true.df=data.frame(imputed=as.factor(true),condon=as.factor(data[,var.name1][idx]))
  #ggplot(true.df,aes(condon,fill=imputed))+
    #geom_bar()
  plot1=ggplot(var.df,aes(condon,fill=imputed))+
    geom_bar(alpha=1)+
    #scale_fill_brewer(palette="Set2",name =var.name2)+
    #scale_fill_manual(values = c("#F0E442","#56B4E9"),name=var.name2)+
    scale_fill_manual(values = c("#f1a340","#998ec3"),name=var.name2)+
    #scale_fill_manual(values = c("#edf8b1","#2c7fb8"),name=var.name2)+
    #values = c("#F0E442","#56B4E9")
    xlab(paste("variable:",var.name1))+
    coord_flip()+
    facet_wrap(~set,nrow=1,scales="free_x")+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+
    theme(strip.text.x = element_text(size = 18),
          axis.text=element_text(size=13),
          axis.title = element_text(size=18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 15))
  #theme(strip.background = element_rect(fill=c("orange",rep("gray",5))))
  #facet_wrap(~set,nrow=1)

  plot2=ggplot(var.df,aes(condon,fill=imputed))+
    geom_bar(alpha=1)+
    #scale_fill_brewer(palette="Set2",name =var.name2)+
    #scale_fill_manual(values = c("#F0E442","#56B4E9"),name=var.name2)+
    scale_fill_manual(values = c("#f1a340","#998ec3"),name=var.name2)+
    #scale_fill_manual(values = c("#edf8b1","#2c7fb8"),name=var.name2)+
    #values = c("#F0E442","#56B4E9")
    xlab(paste("variable:",var.name1))+
    facet_wrap(~set,nrow=1,scales="free_y")+
    #scale_y_continuous(labels = scales::percent)+
    theme(strip.text.x = element_text(size = 12),
          axis.text=element_text(size=10),
          axis.title = element_text(size=14))
  #theme(strip.background = element_rect(fill=c("orange",rep("gray",5))))
  #facet_wrap(~set,nrow=1)
  plot1

  g <- ggplot_gtable(ggplot_build(plot1))

  stripr<- which(grepl('strip-t', g$layout$name))
  stripr
  #fills="gray60"
  #k=1
  #i=stripr[1]
  #j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  #g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  #k <- k+1
  fills<-c("gray70","gray70",rep("gray90",5))
  k <- 1
  for (i in stripr){
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  g

}

####conditional two numeric
var.name1="specwgt"
var.name2="tumdiam"
plotconnum=function(var.name1="specwgt",var.name2="tumdiam",data=data.df,imp=mixgb.data2,alpha=0.5){
  idx=which(is.na(withNA.df[,var.name2]))
  true=data[,var.name2][idx]
  observed2=data[,var.name2][-idx]
  observed1=data[,var.name1][-idx]
  na.num=length(idx)
  obs.num=length(observed2)
  
  #dim(sapply(mixgb.data2,"[[",var.name ))
  #lapply(mixgb.data2,"[",var.name )[[1]]
  L=lapply(imp,"[[",var.name2)
  L1=lapply(L,function(x){x[idx]})
  imputed=unlist(L1)
  imputed=c(observed2,true,imputed)
  set=c(rep("Observed",obs.num),rep("Masked true",na.num),rep(paste("Imputed-",1:5,sep=""),each=na.num))
  set=factor(set,levels=c("Observed","Masked true",paste("Imputed-",1:5,sep="")))
  condon=c(observed1,rep(data[,var.name1][idx],6))
  var.df=data.frame(impupted=imputed,condon=condon,set=set)


  plot1=ggplot(var.df,aes(y=imputed,x=condon))+
    #red "#FC4E07"
    geom_point(colour="#e66101",alpha=alpha)+
    #scale_fill_brewer(palette="Set2",name =var.name2)+
    ylab(paste("variable:",var.name2))+
    xlab(paste("variable:",var.name1))+
    facet_wrap(~set,nrow=1)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3))+
    theme(strip.text.x = element_text(size = 18),
          axis.text=element_text(size=13),
          axis.title = element_text(size=18))
  #theme(strip.background = element_rect(fill=c("orange",rep("gray",5))))
  #facet_wrap(~set,nrow=1)
  plot1

  g <- ggplot_gtable(ggplot_build(plot1))

  stripr<- which(grepl('strip-t', g$layout$name))
  stripr
  #fills="gray60"
  #k=1
  #i=stripr[1]
  #j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  #g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  #k <- k+1
  fills<-c("gray70","gray70",rep("gray90",5))
  k <- 1
  for (i in stripr){
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  g

}



####conditional one factor two numeric
num.namey="charges"
num.namex="bmi"
fac.name="smoker"
plotcon1fac2num=function(num.namey="charges",num.namex="bmi",fac.name="smoker",data=insurance,imp=mixgb.data2,alpha=0.5){
  idx=which(is.na(withNA.df[,num.namey]))
  true=data[,num.namey][idx]
  observed=data[,num.namey][-idx]
  na.num=length(idx)

  L=lapply(imp,"[[",num.namey)
  L1=lapply(L,function(x){x[idx]})
  imputed=unlist(L1)
  imputed=c(true,imputed)
  set=c(rep("Masked true",na.num),rep(paste("Imputed-",1:5,sep=""),each=na.num))
  set=factor(set,levels=c("Masked true",paste("Imputed-",1:5,sep="")))
  X=rep(data[,num.namex][idx],6)
  condon=rep(as.factor(data[,fac.name][idx]),6)
  var.df=data.frame(impupted=imputed,X=X,condon=condon,set=set)
  #cbind(var.df$imputed[var.df$set=="Masked true"],var.df$imputed[var.df$set=="Imputed-1"])
  #r=range(var.df$imputed[var.df$set=="Masked true"])
  #hist(var.df$imputed[var.df$set=="Masked true"],breaks = 100,xlim=r)
  #hist(var.df$imputed[var.df$set=="Imputed-4"],breaks = 100,xlim=r)
  #true.df=data.frame(imputed=true,condon=as.factor(data[,var.name2][idx]))
  #ggplot(true.df,aes(imputed,fill=condon))+
  #geom_bar()

  plot1=ggplot(var.df,aes(x=X,y=imputed,colour=condon))+
    geom_point(alpha=alpha)+
    geom_smooth(method="lm")+
    labs(colour=fac.name)+
    ylab(paste("variable:",num.namey))+
    xlab(paste("variable:",num.namex))+
    facet_wrap(~set,nrow=1)+
    theme(strip.text.x = element_text(size = 12),
          axis.text=element_text(size=10),
          axis.title = element_text(size=14))
  #theme(strip.background = element_rect(fill=c("orange",rep("gray",5))))
  #facet_wrap(~set,nrow=1)
  plot1

  g <- ggplot_gtable(ggplot_build(plot1))

  stripr<- which(grepl('strip-t', g$layout$name))
  stripr
  #fills="gray60"
  #k=1
  #i=stripr[1]
  #j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  #g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  #k <- k+1
  fills<-c("gray70",rep("gray90",5))
  k <- 1
  for (i in stripr){
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  g

}

####conditional one factor two numeric
num.namey="charges"
num.namex="bmi"
fac.name="smoker"
plotcon1fac2num=function(num.namey="charges",num.namex="bmi",fac.name="smoker",data=insurance,imp=mixgb.data2){
  idx=which(is.na(withNA.df[,num.namey]))
  true=data[,num.namey][idx]
  observed=data[,num.namey][-idx]
  na.num=length(idx)

  L=lapply(imp,"[[",num.namey)
  L1=lapply(L,function(x){x[idx]})
  imputed=unlist(L1)
  imputed=c(true,imputed)
  set=c(rep("Masked true",na.num),rep(paste("Imputed-",1:5,sep=""),each=na.num))
  set=factor(set,levels=c("Masked true",paste("Imputed-",1:5,sep="")))
  X=rep(data[,num.namex][idx],6)
  condon=rep(as.factor(data[,fac.name][idx]),6)
  var.df=data.frame(impupted=imputed,X=X,condon=condon,set=set)
  #cbind(var.df$imputed[var.df$set=="Masked true"],var.df$imputed[var.df$set=="Imputed-1"])
  #r=range(var.df$imputed[var.df$set=="Masked true"])
  #hist(var.df$imputed[var.df$set=="Masked true"],breaks = 100,xlim=r)
  #hist(var.df$imputed[var.df$set=="Imputed-4"],breaks = 100,xlim=r)
  #true.df=data.frame(imputed=true,condon=as.factor(data[,var.name2][idx]))
  #ggplot(true.df,aes(imputed,fill=condon))+
  #geom_bar()

  plot1=ggplot(var.df,aes(x=X,y=imputed,colour=condon))+
    geom_point()+
    geom_smooth(method="lm")+
    labs(colour=fac.name)+
    ylab(paste("variable:",num.namey))+
    xlab(paste("variable:",num.namex))+
    facet_wrap(~set,nrow=1)+
    theme(strip.text.x = element_text(size = 12),
          axis.text=element_text(size=10),
          axis.title = element_text(size=14))
  #theme(strip.background = element_rect(fill=c("orange",rep("gray",5))))
  #facet_wrap(~set,nrow=1)
  plot1

  g <- ggplot_gtable(ggplot_build(plot1))

  stripr<- which(grepl('strip-t', g$layout$name))
  stripr
  #fills="gray60"
  #k=1
  #i=stripr[1]
  #j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  #g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  #k <- k+1
  fills<-c("gray70",rep("gray90",5))
  k <- 1
  for (i in stripr){
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  g

}


####conditional one factor one numeric (only missing in fac.name)

plotcon1fac1num=function(num.name="trel",fac.name="histol",data=data.df,imp=mixgb.data2){
  idx=which(is.na(withNA.df[,fac.name]))
  true=data[,fac.name][idx]
  obs.fac=data[,fac.name][-idx]
  obs.numeric=data[,num.name][-idx]
  na.num=length(idx)
  obs.num=length(obs.fac)

  L=lapply(imp,"[[",fac.name)
  L1=lapply(L,function(x){x[idx]})
  imputed=unlist(L1)
  imputed=c(obs.fac,true,imputed)
  set=c(rep("Observed",obs.num),rep("Masked true",na.num),rep(paste("Imputed-",1:5,sep=""),each=na.num))
  set=factor(set,levels=c("Observed","Masked true",paste("Imputed-",1:5,sep="")))
  condon=c(obs.numeric,rep(data[,num.name][idx],6))
  var.df=data.frame(imputed=as.factor(imputed),condon=condon,set=set)
  #cbind(var.df$imputed[var.df$set=="Masked true"],var.df$imputed[var.df$set=="Imputed-1"])
  #r=range(var.df$imputed[var.df$set=="Masked true"])
  #hist(var.df$imputed[var.df$set=="Masked true"],breaks = 100,xlim=r)
  #hist(var.df$imputed[var.df$set=="Imputed-4"],breaks = 100,xlim=r)
  #true.df=data.frame(imputed=true,condon=as.factor(data[,var.name2][idx]))
  #ggplot(true.df,aes(imputed,fill=condon))+
  #geom_bar()

  if(length(levels(data[,fac.name]))==2){
    plot1=ggplot(var.df,aes(x=imputed,y=condon,fill=imputed,color=imputed))+
      #geom_point(position=position_jitterdodge(jitter.width=0.1,jitter.height = 0.2))+
      #geom_boxplot(outlier.size=0,alpha=0.6)+
      geom_boxplot(outlier.size=0.7,alpha=0.6)+
      geom_jitter(position=position_jitter(0.2),size=0.2)+
      #yellow and blue "#F0E442","#56B4E9"
      scale_color_manual(values = c("#f1a340","#998ec3"),name=fac.name)+
      scale_fill_manual(values = c("#f1a340","#998ec3"),name=fac.name)+
      #scale_fill_brewer(palette="Set2",name =fac.name)+
      ylab(paste("variable:",num.name))+
      xlab(paste("variable:",fac.name))+
      facet_wrap(~set,nrow=1)+
      theme(strip.text.x = element_text(size = 18),
            axis.text=element_text(size=13),
            axis.title = element_text(size=18),
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 15),
            legend.key.size = unit(1, "cm"),
            legend.key.width = unit(1,"cm"))
    #theme(strip.background = element_rect(fill=c("orange",rep("gray",5))))
  }else{
    plot1=ggplot(var.df,aes(x=imputed,y=condon,fill=imputed))+
      #geom_point(position=position_jitterdodge(jitter.width=0.1,jitter.height = 0.2))+
      #geom_boxplot(outlier.size=0,alpha=0.6)+
      geom_boxplot(outlier.size=0.7,alpha=0.6)+
      #scale_fill_manual(values = c("#F0E442","#56B4E9"),name=fac.name)+
      scale_fill_brewer(palette="Set2",name =fac.name)+
      ylab(paste("variable:",num.name))+
      xlab(paste("variable:",fac.name))+
      facet_wrap(~set,nrow=1)+
      theme(strip.text.x = element_text(size = 18),
            axis.text=element_text(size=13),
            axis.title = element_text(size=18),
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 15))
    #theme(strip.background = element_rect(fill=c("orange",rep("gray",5))))
  }


  #facet_wrap(~set,nrow=1)
  plot1

  g <- ggplot_gtable(ggplot_build(plot1))

  stripr<- which(grepl('strip-t', g$layout$name))
  stripr
  #fills="gray60"
  #k=1
  #i=stripr[1]
  #j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  #g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  #k <- k+1
  fills<-c("gray70","gray70",rep("gray90",5))
  k <- 1
  for (i in stripr){
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  g

}



####conditional one factor two numeric
num.namey="charges"
num.namex="bmi"
fac.name="smoker"
plotcon1fac2num=function(num.namey="charges",num.namex="bmi",fac.name="smoker",data=insurance,imp=mixgb.data2){
  idx=which(is.na(withNA.df[,num.namey]))
  true=data[,num.namey][idx]
  observed=data[,num.namey][-idx]
  na.num=length(idx)

  L=lapply(imp,"[[",num.namey)
  L1=lapply(L,function(x){x[idx]})
  imputed=unlist(L1)
  imputed=c(true,imputed)
  set=c(rep("Masked true",na.num),rep(paste("Imputed-",1:5,sep=""),each=na.num))
  set=factor(set,levels=c("Masked true",paste("Imputed-",1:5,sep="")))
  X=rep(data[,num.namex][idx],6)
  condon=rep(as.factor(data[,fac.name][idx]),6)
  var.df=data.frame(impupted=imputed,X=X,condon=condon,set=set)
  #cbind(var.df$imputed[var.df$set=="Masked true"],var.df$imputed[var.df$set=="Imputed-1"])
  #r=range(var.df$imputed[var.df$set=="Masked true"])
  #hist(var.df$imputed[var.df$set=="Masked true"],breaks = 100,xlim=r)
  #hist(var.df$imputed[var.df$set=="Imputed-4"],breaks = 100,xlim=r)
  #true.df=data.frame(imputed=true,condon=as.factor(data[,var.name2][idx]))
  #ggplot(true.df,aes(imputed,fill=condon))+
  #geom_bar()

  plot1=ggplot(var.df,aes(x=X,y=imputed,colour=condon))+
    geom_point()+
    geom_smooth(method="lm")+
    labs(colour=fac.name)+
    ylab(paste("variable:",num.namey))+
    xlab(paste("variable:",num.namex))+
    facet_wrap(~set,nrow=1)+
    theme(strip.text.x = element_text(size = 12),
          axis.text=element_text(size=10),
          axis.title = element_text(size=14))
  #theme(strip.background = element_rect(fill=c("orange",rep("gray",5))))
  #facet_wrap(~set,nrow=1)
  plot1

  g <- ggplot_gtable(ggplot_build(plot1))

  stripr<- which(grepl('strip-t', g$layout$name))
  stripr
  #fills="gray60"
  #k=1
  #i=stripr[1]
  #j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  #g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  #k <- k+1
  fills<-c("gray70",rep("gray90",5))
  k <- 1
  for (i in stripr){
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  g

}

