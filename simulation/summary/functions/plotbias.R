

plotbias<-function(sim.result=result,model=1,colscheme=2){
  #pal<-c("#0066CC","#0073E6","#1A8CFF","#4DA6FF","#66B3FF","#99CCFF","#CCE6FF","#FFCC00")
  ##pal<-c("#B3D9FF","#99CCFF","#66B3FF","#4DA6FF","#1A8CFF","#0073E6","#0066CC","#E6B800")

  ##6 gradient blues and 1 yellow
  pal<-c("#0073E6","#1A8CFF","#4DA6FF","#66B3FF","#99CCFF","#CCE6FF","#FFCC00")

 # blue<-c("#0073E6","#1A8CFF","#4DA6FF","#66B3FF","#99CCFF")
  #blue<-rev(blue)
  blue<-rev(pal[1:6])
  yellow<-c("#FEFE33","#FDDC22","#FFCC00")

  if(model==1){
    #fit<-lm(y ~ x1+x2+x3+x8+x9+I(x3^2),data=complete.df)
    #true.coef<-coef(fit)
    true.coef<-c(0,rep(0.31,5),0.28)
    if(colscheme==2){
      pal<-rep(NA,7)
      pal[c(4,7)]<-yellow[c(1,3)]
      pal[-c(4,7)]<-blue[2:6]
      colors<-pal
      colors[c(4,7)]<-"#333333"
    }

  }else if(model==2){
    #fit<-lm(y ~ x1+x2+x3+x8+x9+x1:x2,data=complete.df)
    #true.coef<-coef(fit)
    true.coef<-c(0,rep(0.31,5),0.35)

    if(colscheme==2){
      pal<-rep(NA,7)
      pal[c(2,3,7)]<-yellow
      pal[-c(2,3,7)]<-blue[2:5]
      colors<-pal
      colors[c(2,3,7)]<-"#333333"
    }

  }else{
    #fit<-lm(y ~ x1+x2+x3+x8+x9+x8:x9,data=complete.df)
    #true.coef<-coef(fit)
    true.coef<-c(0,rep(0.31,5),0.37)
    if(colscheme==2){
      pal<-rep(NA,7)
      pal[c(5,6,7)]<-yellow
      pal[-c(5,6,7)]<-blue[2:5]
      colors<-pal
      colors[c(5,6,7)]<-"#333333"
    }
  }

  #true.within<-diag(vcov(fit))
  #L: number of betas
  L<-7
  runs=nrow(sim.result[[1]])/L
  #N: number of methods
  N=length(sim.result)
  sum.list=list()
  for(i in 1:N){
    sim.result[[i]]=as.data.frame(sim.result[[i]])
    sim.result[[i]]$bias=sim.result[[i]]$comb.coef-sim.result[[i]]$hbeta

    sim.result[[i]]$t.ciwidth=sim.result[[i]]$`t.upper)`-sim.result[[i]]$`(t.lower`
    #coverage percentage
    sim.result[[i]]$t.coverage= sim.result[[i]]$`(t.lower`<sim.result[[i]]$hbeta & sim.result[[i]]$hbeta<sim.result[[i]]$`t.upper)`
    sim.result[[i]]$beta.group=rep(1:L,runs)


    summary.df<-sim.result[[i]] %>% group_by(beta.group) %>%
      dplyr::summarise(across(c("comb.coef","hbeta"), list(mean=mean, var = var),.names = "{col}_{fn}"),
                       across(c("bias"),list(mean=mean,sd=sd),.names="{col}_{fn}"),
                       across(-c("comb.coef","hbeta","bias"),mean), .groups = 'drop')
    summary.df$true.coef<-true.coef
    summary.df$bias_se<-summary.df$bias_sd/sqrt(runs)

    summary.df <-summary.df%>%
      dplyr::rename(MI.coef=comb.coef_mean,
                    true.total=comb.coef_var,
                    hat.coef=hbeta_mean,
                    true.within1=hbeta_var,
                    total=total.var,
                    true.within2=hbeta.var)
    summary.df$true.between1=summary.df$true.total-summary.df$true.within1
    summary.df$true.between=summary.df$true.total-summary.df$true.within2

    ##scale
    summary.df$bias_se=100*summary.df$bias_se
    summary.df$bias_mean=100*summary.df$bias_mean
    summary.df$total=10000*summary.df$total
    summary.df$true.total=10000*summary.df$true.total
    summary.df$within=10000*summary.df$within
    summary.df$true.within2=10000*summary.df$true.within2
    summary.df$between=10000*summary.df$between
    summary.df$true.between=10000*summary.df$true.between
    summary.df$t.coverage=100*summary.df$t.coverage
    summary.df$missinfo=100*summary.df$missinfo
    summary.df$diff.total=summary.df$total- summary.df$true.total
    summary.df$diff.within=summary.df$within- summary.df$true.within2
    summary.df$diff.between=summary.df$between- summary.df$true.between


    #note: between here is after adjustment for small m, ie. between=(1+1/m)*between
    #rubin paper: multiple imputation after 18 years  page 482

    sum.list[[i]]<-summary.df[,c("bias_mean","bias_se")]

  }

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


  #Linear.m=sapply(lapply(lapply(sum.list,function(x) x[l.terms,]),abs),colMeans)
  #Interact.m=sapply(lapply(lapply(sum.list,function(x) x[i.terms,]),abs),colMeans)
  all.df<-plyr::ldply(sum.list,data.frame)
  all.df$Estimates<-factor(rep(paste("beta",0:6,sep=""),8),levels=c("beta0","beta1","beta2","beta3","beta4","beta5","beta6"),labels=m)

  all.df$Estimates<-as.factor(all.df$Estimates)

  all.df$Methods<-factor(rep(c("mice.pmm","mice.midastouch","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"),each=7),levels=c("mice.pmm","mice.midastouch","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"))

  All.df<-data.frame("Bias"=all.df$bias_mean,
                     "Se"=all.df$bias_se,
                     "Ci"=qt(0.975,df=1000)*all.df$bias_se,
                     "Estimates"=all.df$Estimates,
                     "Methods"=all.df$Methods)
  #colour blind friendly palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  # The palette with black:
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  #pal<-c(rev(blues)[1:6],"hotpink")
  # pal<-c("#0066CC","#0080FF","#3399FF","#66B3FF","#99CCFF","#CCE6FF","#FF9900")


  ggplot(data=All.df,aes(x=Estimates,y=Bias,fill=Estimates,color=Estimates),labeller=label_parsed)+
    geom_bar(stat = "identity")+
    geom_errorbar(aes(ymin=Bias-Ci,ymax=Bias+Ci),width=0.6,colour="gray30")+
    ylab(expression(paste("Average ",{Bias}%*% 100)))+
    scale_fill_manual(values=pal,labels=m)+
    scale_color_manual(values=colors,labels=m)+
    #scale_fill_discrete(labels = m)+
    scale_x_discrete(labels=m)+
    #scale_fill_brewer(palette="Set2")+
    facet_wrap(~Methods,nrow=1,labeller=label_parsed)+
    theme(axis.text.x = element_text(angle =0),
          axis.title.x = element_text(size=12,margin=margin(t=10,r=0,b=0,l=0)),
          axis.title.y = element_text(size=12,margin=margin(0,r=10,0,l=10)),
          strip.text.x = element_text(size = 11),
          panel.spacing.y = unit(0.1, "in"),
          #legend.key=element_rect(size=3,colour=NA),
          legend.title = element_text(size=12),
          legend.text = element_text(size=12),
          legend.key.width=unit(0.2,"in"),
          legend.key.height=unit(0.3,"in"),
          legend.text.align = 0
          #legend.spacing = unit(0.2,"in")
    )


}


# plot categorical --------------------------------------------------------
#colour scheme 1: interaction term yellow, other terms blue
#colour scheme 2: terms related to interaction yellow, other blue


plotbias.cat<-function(sim.result=result,model=1,effect="s",colscheme=2){

  pal<-c("#0066CC","#0073E6","#1A8CFF","#4DA6FF","#66B3FF","#99CCFF","#CCE6FF","#FFCC00")
  #pal<-c("#B3D9FF","#99CCFF","#66B3FF","#4DA6FF","#1A8CFF","#0073E6","#0066CC","#E6B800")


  blue<-c("#0073E6","#1A8CFF","#4DA6FF","#66B3FF","#99CCFF")
  blue<-rev(blue)
    #from light to dark
  #yellow<-c("#FEFE33","#FDED2A","#FDDC22","#FCCB1A","#FCBA12","#FBA90A","#FB9902","#FB8604","#FC7307")
  yellow<-c("#FEFE33","#FDDC22","#FFCC00")
  if(model==1){

   if(effect=="s"){
     true.coef<-c(0,0.5,1,0.4,-0.5,-1.1,-0.5,0.4)
   }else{
     true.coef<-c(0,0.5,1,0.5,-1,-1,-0.5,1.1)
   }

    if(colscheme==2){
      pal<-rep(NA,8)
      pal[c(2,3,8)]<-yellow
      pal[-c(2,3,8)]<-blue
      colors<-pal
      colors[c(2,3,8)]<-"#333333"
    }

  }else if(model==2){

    if(effect=="s"){
      true.coef<-c(0,0.7,0.4,0.7,-0.2,-1.1,-0.7,0.4)
    }else{
      true.coef<-c(0,0.7,0.5,0.7,-0.2,-1.1,-1.1,1.1)
    }

    if(colscheme==2){
      pal<-rep(NA,8)
      pal[c(4,5,8)]<-yellow
      pal[-c(4,5,8)]<-blue
      colors<-pal
      colors[c(4,5,8)]<-"#333333"
    }

  }else{

    if(effect=="s"){
      true.coef<-c(0,0.4,0.5,0.4,-1.1,-0.2,-0.2,0.4)
    }else{
      true.coef<-c(0,0.4,0.4,0.4,-1.3,-0.2,-0.2,1.1)
    }
    if(colscheme==2){
      pal<-rep(NA,8)
      pal[c(6,7,8)]<-yellow
      pal[-c(6,7,8)]<-blue
      colors<-pal
      colors[c(6,7,8)]<-"#333333"

    }
  }

  #true.within<-diag(vcov(fit))
  #L: number of alphas
  L<-8
  runs=nrow(sim.result[[1]])/L
  #N: number of methods
  N=length(sim.result)
  sum.list=list()

  for(i in 1:N){
    sim.result[[i]]=as.data.frame(sim.result[[i]])
    sim.result[[i]]$bias=sim.result[[i]]$comb.coef-sim.result[[i]]$hbeta

    sim.result[[i]]$t.ciwidth=sim.result[[i]]$`t.upper)`-sim.result[[i]]$`(t.lower`
    #coverage percentage
    sim.result[[i]]$t.coverage= sim.result[[i]]$`(t.lower`<sim.result[[i]]$hbeta & sim.result[[i]]$hbeta<sim.result[[i]]$`t.upper)`
    sim.result[[i]]$alpha.group=rep(1:L,runs)


    summary.df<-sim.result[[i]] %>% group_by(alpha.group) %>%
      dplyr::summarise(across(c("comb.coef","hbeta"), list(mean=mean, var = var),.names = "{col}_{fn}"),
                       across(c("bias"),list(mean=mean,sd=sd),.names="{col}_{fn}"),
                       across(-c("comb.coef","hbeta","bias"),mean), .groups = 'drop')
    summary.df$true.coef<-true.coef
    summary.df$bias_se<-summary.df$bias_sd/sqrt(runs)

    summary.df <-summary.df%>%
      dplyr::rename(MI.coef=comb.coef_mean,
                    true.total=comb.coef_var,
                    hat.coef=hbeta_mean,
                    true.within1=hbeta_var,
                    total=total.var,
                    true.within2=hbeta.var)
    summary.df$true.between1=summary.df$true.total-summary.df$true.within1
    summary.df$true.between=summary.df$true.total-summary.df$true.within2

    ##scale
    summary.df$bias_se=100*summary.df$bias_se
    summary.df$bias_mean=100*summary.df$bias_mean
    summary.df$total=10000*summary.df$total
    summary.df$true.total=10000*summary.df$true.total
    summary.df$within=10000*summary.df$within
    summary.df$true.within2=10000*summary.df$true.within2
    summary.df$between=10000*summary.df$between
    summary.df$true.between=10000*summary.df$true.between
    summary.df$t.coverage=100*summary.df$t.coverage
    summary.df$missinfo=100*summary.df$missinfo
    summary.df$diff.total=summary.df$total- summary.df$true.total
    summary.df$diff.within=summary.df$within- summary.df$true.within2
    summary.df$diff.between=summary.df$between- summary.df$true.between


    #note: between here is after adjustment for small m, ie. between=(1+1/m)*between
    #rubin paper: multiple imputation after 18 years  page 482

    sum.list[[i]]<-summary.df[,c("bias_mean","bias_se")]

  }

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
    m=c(expression(alpha[0]),expression(alpha[15]),expression(alpha[16]),
        expression(alpha[17]),expression(alpha[18]),expression(alpha[19]),
        expression(alpha[20]),expression(alpha[21])
    )
  }


  #Linear.m=sapply(lapply(lapply(sum.list,function(x) x[l.terms,]),abs),colMeans)
  #Interact.m=sapply(lapply(lapply(sum.list,function(x) x[i.terms,]),abs),colMeans)
  all.df<-plyr::ldply(sum.list,data.frame)
  all.df$Estimates<-factor(rep(paste("alpha",0:7,sep=""),8),levels=c("alpha0","alpha1","alpha2","alpha3","alpha4","alpha5","alpha6","alpha7"),labels=m)

  all.df$Estimates<-as.factor(all.df$Estimates)

  all.df$Methods<-factor(rep(c("mice.logreg","mice.logregboot","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"),each=8),levels=c("mice.logreg","mice.logregboot","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"))

  All.df<-data.frame("Bias"=all.df$bias_mean,
                     "Se"=all.df$bias_se,
                     "Ci"=qt(0.975,df=1000)*all.df$bias_se,
                     "Estimates"=all.df$Estimates,
                     "Methods"=all.df$Methods)
  #colour blind friendly palette
  #cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  # The palette with black:
  #cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  #pal<-c(rev(blues)[1:6],"hotpink")
  # pal<-c("#0066CC","#0080FF","#3399FF","#66B3FF","#99CCFF","#CCE6FF","#FF9900")

  ggplot(data=All.df,aes(x=Estimates,y=Bias,fill=Estimates,color=Estimates),labeller=label_parsed)+
    geom_bar(stat = "identity")+
    geom_errorbar(aes(ymin=Bias-Ci,ymax=Bias+Ci),width=0.6,colour="gray30")+
    ylab(expression(paste("Average ",{Bias}%*% 100)))+
    ylim(c(-40,40))+
    scale_fill_manual(values=pal,labels=m)+
    scale_color_manual(values=colors,labels=m)+
    #scale_fill_discrete(labels = m)+
    scale_x_discrete(labels=m)+
    #scale_fill_brewer(palette="Set2")+
    facet_wrap(~Methods,nrow=1,labeller=label_parsed)+
    #plot.margin=unit(c(0.2,0.2,0.2,0.3),"in"),
    theme(axis.text.x = element_text(size=7,angle =0),
          axis.title.x = element_text(size=12,margin=margin(t=10,r=0,b=0,l=0)),
          axis.title.y = element_text(size=12,margin=margin(0,r=10,0,l=10)),
          strip.text.x = element_text(size = 11),
          panel.spacing.y = unit(0.1, "in"),
          #legend.key=element_rect(size=3,colour=NA),
          legend.title = element_text(size=12),
          legend.text = element_text(size=12),
          legend.key.width=unit(0.2,"in"),
          legend.key.height=unit(0.3,"in"),
          legend.text.align = 0
          #legend.spacing = unit(0.2,"in")
    )


}


