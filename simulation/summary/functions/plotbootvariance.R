
library(RColorBrewer)
f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
reds<- f("YlOrRd")[-1]
oranges<-f("Oranges")[-1]
greens<-f("YlGn")[-1]
Greens<-c(greens[1:5],"#389451",greens[6:7])
purples<-f("Purples")[-1]



plotbootvariance<-function(bootresult=M.sum,bootruns=100,model=1,pal.col=Greens){


  if(model==1){
    m=c(expression(beta[0]), expression(beta[1]),expression(beta[2]),
        expression(beta[3]),expression(beta[4]),
        expression(beta[5]),expression(beta[6])
    )
  }else if(model==2){
    m=c(expression(beta[0]), expression(beta[7]),expression(beta[8]),
        expression(beta[9]),expression(beta[10]),
        expression(beta[11]),expression(beta[12])
    )
  }else{
    m=c(expression(beta[0]), expression(beta[13]),expression(beta[14]),
        expression(beta[15]),expression(beta[16]),
        expression(beta[17]),expression(beta[18])
    )
  }

  #all.df$Estimates<-factor(rep(paste("beta",0:6,sep=""),8),levels=c("beta0","beta1","beta2","beta3","beta4","beta5","beta6"),labels=m)

  #all.df$Methods<-factor(rep(c("mice.pmm","mice.midastouch","mice.cart","mice.rf","missranger","mixgb","mixgb.pmm1","mixgb.pmm2"),each=7),levels=c("mice.pmm","mice.midastouch","mice.cart","mice.rf","missranger","mixgb","mixgb.pmm1","mixgb.pmm2"))

  All.df<-data.frame("value"=c(bootresult$diff_within.mean,bootresult$diff_between.mean),
                     "Se"=c(bootresult$diff_within.se,bootresult$diff_between.se),
                     "Ci"=qt(0.975,df=bootruns)*c(bootresult$diff_within.se,bootresult$diff_between.se),
                     "quantity.name"=factor(rep(c("diff.within","diff.between"),each=56),
                                            levels=c("diff.within","diff.between"),
                                            labels=c('"[" ~Var[within]-Var[within]({true})~ "]" %*%10000',
                                                     '"[" ~Var[between]-Var[between]({true})~ "]" %*%10000')),
                     "Estimates"=rep(bootresult$beta.group,2),
                     "Methods"=rep(bootresult$method,2))

  #colour blind friendly palette
  #cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  # The palette with black:
  #cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  ##6 gradient blues and 1 yellow
  #pal<-c("#0073E6","#1A8CFF","#4DA6FF","#66B3FF","#99CCFF","#CCE6FF","#FFCC00")
  pal<-pal.col


  ggplot(data=All.df,aes(x=Estimates,y=value,fill=Estimates),labeller=label_parsed)+
    geom_bar(stat = "identity",position=position_dodge())+
    geom_errorbar(aes(ymin=value-Ci,ymax=value+Ci),width=0.6,colour="gray30")+
    ylab("Difference")+
    scale_x_discrete(labels=m)+
    scale_fill_manual(values=pal,labels=m)+
    #scale_fill_discrete(labels = m)+
    #scale_fill_brewer(palette="Set2")+
    #facet_wrap(~quantity.name,scales="free",nrow=2,labeller=label_parsed)+
    # facet_grid(quantity.name~Methods,scales="free",labeller=label_parsed,switch = "y")+
    facet_grid(quantity.name~Methods,labeller=label_parsed,switch = "y")+
    theme(plot.margin=unit(c(0.2,0.2,0.2,0),"in"),
          axis.text.x = element_text(angle = 0),
          axis.title.x = element_text(size=12,margin=margin(t=10,r=0,b=0,l=0)),
          axis.title.y = element_text(size=12,margin=margin(0,r=10,0,l=10)),
          strip.text.x = element_text(size = 11),
          panel.spacing.y = unit(0.1, "in"),
          panel.spacing.x = unit(0.1, "in"),
          legend.title = element_text(size=12),
          legend.text = element_text(size=12),
          legend.key.width=unit(0.2,"in"),
          legend.key.height=unit(0.3,"in"),
          legend.text.align = 0)
  


}



# plot categorical --------------------------------------------------------

plotbootvariance.cat<-function(bootresult=M.sum,bootruns=100,model=1,pal.col=Greens){


  if(model==1){
    m=c(expression(alpha[0]), expression(alpha[1]),expression(alpha[2]),
        expression(alpha[3]),expression(alpha[4]),
        expression(alpha[5]),expression(alpha[6]),expression(alpha[7])
    )
  }else if(model==2){
    m=c(expression(alpha[0]),expression(alpha[8]),
        expression(alpha[9]),expression(alpha[10]),
        expression(alpha[11]),expression(alpha[12]),expression(alpha[13]),expression(alpha[14])
    )
  }else{
    m=c(expression(alpha[0]),
        expression(alpha[15]),expression(alpha[16]),
        expression(alpha[17]),expression(alpha[18]),
        expression(alpha[19]),expression(alpha[20]),
        expression(alpha[21])
    )
  }

  #all.df$Estimates<-factor(rep(paste("beta",0:6,sep=""),8),levels=c("beta0","beta1","beta2","beta3","beta4","beta5","beta6"),labels=m)

  #all.df$Methods<-factor(rep(c("mice.pmm","mice.midastouch","mice.cart","mice.rf","missranger","mixgb","mixgb.pmm1","mixgb.pmm2"),each=7),levels=c("mice.pmm","mice.midastouch","mice.cart","mice.rf","missranger","mixgb","mixgb.pmm1","mixgb.pmm2"))

  All.df<-data.frame("value"=c(bootresult$diff_within.mean,bootresult$diff_between.mean),
                     "Se"=c(bootresult$diff_within.se,bootresult$diff_between.se),
                     "Ci"=qt(0.975,df=bootruns)*c(bootresult$diff_within.se,bootresult$diff_between.se),
                     "quantity.name"=factor(rep(c("diff.within","diff.between"),each=64),
                                            levels=c("diff.within","diff.between"),
                                            labels=c('"[" ~Var[within]-Var[within]({true})~ "]" %*%100',
                                                     '"[" ~Var[between]-Var[between]({true})~ "]" %*%100')),
                     "Estimates"=rep(bootresult$beta.group,2),
                     "Methods"=rep(bootresult$method,2))

  #colour blind friendly palette
  #cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  # The palette with black:
  #cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  ##6 gradient blues and 1 yellow
  #pal<-c("#0073E6","#1A8CFF","#4DA6FF","#66B3FF","#99CCFF","#CCE6FF","#FFCC00")
  pal<-pal.col


  ggplot(data=All.df,aes(x=Estimates,y=value,fill=Estimates),labeller=label_parsed)+
    geom_bar(stat = "identity",position=position_dodge())+
    geom_errorbar(aes(ymin=value-Ci,ymax=value+Ci),width=0.6,colour="gray30")+
    ylab("Difference")+
    scale_x_discrete(labels=m)+
    scale_fill_manual(values=pal,labels=m)+
    #scale_fill_discrete(labels = m)+
    #scale_fill_brewer(palette="Set2")+
    #facet_wrap(~quantity.name,scales="free",nrow=2,labeller=label_parsed)+
    # facet_grid(quantity.name~Methods,scales="free",labeller=label_parsed,switch = "y")+
    facet_grid(quantity.name~Methods,labeller=label_parsed,switch = "y")+
    theme(plot.margin=unit(c(0.2,0.2,0.2,0),"in"),
          axis.text.x = element_text(size=7,angle = 0),
          axis.title.x = element_text(size=12,margin=margin(t=10,r=0,b=0,l=0)),
          axis.title.y = element_text(size=12,margin=margin(0,r=10,0,l=10)),
          strip.text.x = element_text(size = 11),
          panel.spacing.y = unit(0.1, "in"),
          panel.spacing.x = unit(0.1, "in"),
          legend.title = element_text(size=12),
          legend.text = element_text(size=12),
          legend.key.width=unit(0.2,"in"),
          legend.key.height=unit(0.3,"in"),
          legend.text.align = 0)
          

}
