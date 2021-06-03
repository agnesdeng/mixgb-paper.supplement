library(dplyr)
library(purrr)
library(xtable)
library(tidyverse)
library(ggplot2)
library(plyr)

reportbeta<-function(sim.result=result,model=1){

  if(model==1){
    #fit<-lm(y ~ x1+x2+x3+x8+x9+I(x3^2),data=complete.df)
    #true.coef<-coef(fit)
    true.coef<-c(0,rep(0.31,5),0.28)
  }else if(model==2){
    #fit<-lm(y ~ x1+x2+x3+x8+x9+x1:x2,data=complete.df)
    #true.coef<-coef(fit)
    true.coef<-c(0,rep(0.31,5),0.35)
  }else{
    #fit<-lm(y ~ x1+x2+x3+x8+x9+x8:x9,data=complete.df)
    #true.coef<-coef(fit)
    true.coef<-c(0,rep(0.31,5),0.37)
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
    #sim.result[[i]]$bias=sim.result[[i]]$comb.coef-sim.result[[i]]$hbeta

    sim.result[[i]]$bias=sim.result[[i]]$comb.coef-sim.result[[i]]$hbeta
    #sim.result[[i]]$biaspct=100*(sim.result[[i]]$comb.coef-sim.result[[i]]$hbeta)/sim.result[[i]]$hbeta

    sim.result[[i]]$t.ciwidth=sim.result[[i]]$`t.upper)`-sim.result[[i]]$`(t.lower`
    #coverage percentage
    sim.result[[i]]$t.coverage= sim.result[[i]]$`(t.lower`<sim.result[[i]]$hbeta & sim.result[[i]]$hbeta<sim.result[[i]]$`t.upper)`
    sim.result[[i]]$beta.group=rep(1:L,runs)
    sim.result[[i]]$diff.MIhat=sim.result[[i]]$comb.coef-sim.result[[i]]$hbeta


    summary.df<-sim.result[[i]] %>% group_by(beta.group) %>%
      dplyr::summarise(across(c("comb.coef","hbeta"), list(mean=mean, var = var),.names = "{col}_{fn}"),
                       across(c("diff.MIhat"),list(var=var),.names="{fn}_{col}"),
                       across(-c("comb.coef","hbeta","diff.MIhat"),mean), .groups = 'drop')
    summary.df$true.coef<-true.coef

    summary.df <-summary.df%>%
      dplyr::rename(MI.coef=comb.coef_mean,
                    true.total=comb.coef_var,
                    hat.coef=hbeta_mean,
                    true.within1=hbeta_var,
                    total=total.var,
                    true.within2=hbeta.var,
                    true.between2=var_diff.MIhat)
    summary.df$true.between1=summary.df$true.total-summary.df$true.within1
    summary.df$true.between=summary.df$true.total-summary.df$true.within2

    ##scale
    summary.df$bias=100*summary.df$bias
    summary.df$total=10000*summary.df$total
    summary.df$true.total=10000*summary.df$true.total
    summary.df$within=10000*summary.df$within
    summary.df$true.within2=10000*summary.df$true.within2
    summary.df$between=10000*summary.df$between
    summary.df$true.between=10000*summary.df$true.between
    summary.df$t.coverage=100*summary.df$t.coverage
    summary.df$missinfo=100*summary.df$missinfo




    #note: between here is after adjustment for small m, ie. between=(1+1/m)*between
    #rubin paper: multiple imputation after 18 years  page 482

    sum.list[[i]]<-summary.df[,c("bias",
                                 "total","true.total",
                                 "within","true.within2",
                                 "between","true.between",
                                 "t.coverage","t.ciwidth",
                                 "missinfo")]

  }

  #sum.list %>% map(~.x[1,])
  out=list()
  for(i in 1:L){
    ll<-lapply(sum.list,function(x) x[i,])
    ll<-do.call(rbind,ll)
    ll<-ll %>% add_column(method=c("mice.pmm","mice.midastouch","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"),.before = "bias")
    out[[i]]<-ll
  }
  out
}



# Xtable ------------------------------------------------------------------




multi_xtable <- function(...)
{
  vars <- as.list(match.call(expand.dots = TRUE))[-1]
  df_list <- lapply(vars, eval)
  num_cols <- sapply(df_list, length)
  if (!all(num_cols == num_cols[1]))
    stop("All data frames must have equal number of columns")
  xtables <- lapply(df_list, function(x)
    #digits=c(0,4,4,4,4,4,4,4,4,4,4,4)
    capture.output(xtable::print.xtable(xtable::xtable(x,digits=c(0,0,2,2,2,2,2,2,2,0,2,0)), table.placement="H")))
  if (length(xtables) == 1)
    return(xtables[[1]])
  header <- xtables[[1]][1:6]
  tail <- xtables[[1]][length(xtables[[1]]) + (-1:0)]
  xtables <- lapply(xtables, function(x) x[7:(length(x) - 2)])
  xtables <- do.call("c", xtables)
  cat(header, xtables, tail, sep = "\n")
}



# Monte Carlo SEs ---------------------------------------------------------


mcerrors<-function(sim.result=result,show.max=T){
  
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
                       across(c("bias","t.coverage","t.ciwidth","missinfo"),list(mean=mean,sd=sd),.names="{col}_{fn}"),
                       across(-c("comb.coef","hbeta","bias","t.coverage","t.ciwidth","missinfo"),mean), .groups = 'drop')
    
    summary.df$bias_se<-summary.df$bias_sd/sqrt(runs)
    summary.df$t.coverage_se<-summary.df$t.coverage_sd/sqrt(runs)
    summary.df$t.ciwidth_se<-summary.df$t.ciwidth_sd/sqrt(runs)
    summary.df$missinfo_se<-summary.df$missinfo_sd/sqrt(runs)
    
    
    
    ##scale
    summary.df$bias_se=100*summary.df$bias_se
    summary.df$t.coverage_se=100*summary.df$t.coverage_se
    summary.df$missinfo_se=100*summary.df$missinfo_se
    
    
    summary.df$bias_mean=100*summary.df$bias_mean
    
    summary.df$t.coverage_mean=100*summary.df$t.coverage_mean
    summary.df$missinfo_mean=100*summary.df$missinfo_mean
    
    
    
    #note: between here is after adjustment for small m, ie. between=(1+1/m)*between
    #rubin paper: multiple imputation after 18 years  page 482
    if(show.max==T){
      sum.list[[i]]<-summary.df[,c("bias_se",
                                   "t.coverage_se",
                                   "t.ciwidth_se",
                                   "missinfo_se")]
    }else{
      sum.list[[i]]<-summary.df[,c("bias_mean","bias_se",
                                   "t.coverage_mean","t.coverage_se",
                                   "t.ciwidth_mean","t.ciwidth_se",
                                   "missinfo_mean","missinfo_se")]
    }
    
    
  }
  
  
  if(show.max==T){
    out=list()
    for(i in 1:L){
      ll<-lapply(sum.list,function(x) x[i,])
      ll<-do.call(rbind,ll)
      out[[i]]<-ll
    }
    out
    S=sapply(out,function(x) apply(x,2,max))
    apply(S,1,max)
    
    
  }else{
    out=list()
    for(i in 1:L){
      ll<-lapply(sum.list,function(x) x[i,])
      ll<-do.call(rbind,ll)
      ll<-ll %>% add_column(method=c("mice.pmm","mice.midastouch","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"),.before = "bias_mean")
      out[[i]]<-ll
    }
    out
    
  }
  
  
}

# Maximum bootstrap SEs ---------------------------------------------------------
bootbeta<-function(sim.result=result,model=1,seed=1){
  
  if(model==1){
    
    true.coef<-c(0,rep(0.31,5),0.28)
  }else if(model==2){
    
    true.coef<-c(0,rep(0.31,5),0.35)
  }else{
    
    true.coef<-c(0,rep(0.31,5),0.37)
  }
  
  
  #L: number of betas
  L<-7
  runs=nrow(sim.result[[1]])/L
  #N: number of methods
  N=length(sim.result)
  
  set.seed(seed)
  idx=sample(1000,size=1000,replace=TRUE)
  index=(idx-1)*L+1
  boot.index=rep(index,each=L)+0:(L-1)
  
  boot.result=lapply(sim.result,function(x) x[boot.index,])
  
  sum.list=list()
  for(i in 1:N){
    boot.result[[i]]=as.data.frame(boot.result[[i]])
    #boot.result[[i]]$bias=boot.result[[i]]$comb.coef-boot.result[[i]]$hbeta
    
    boot.result[[i]]$bias=boot.result[[i]]$comb.coef-boot.result[[i]]$hbeta
    #boot.result[[i]]$biaspct=100*(boot.result[[i]]$comb.coef-boot.result[[i]]$hbeta)/boot.result[[i]]$hbeta
    
    boot.result[[i]]$t.ciwidth=boot.result[[i]]$`t.upper)`-boot.result[[i]]$`(t.lower`
    #coverage percentage
    boot.result[[i]]$t.coverage= boot.result[[i]]$`(t.lower`<boot.result[[i]]$hbeta & boot.result[[i]]$hbeta<boot.result[[i]]$`t.upper)`
    boot.result[[i]]$beta.group=rep(1:L,runs)
    boot.result[[i]]$diff.MIhat=boot.result[[i]]$comb.coef-boot.result[[i]]$hbeta
    
    
    summary.df<-boot.result[[i]] %>% group_by(beta.group) %>%
      dplyr::summarise(across(c("comb.coef","hbeta"), list(mean=mean, var = var),.names = "{col}_{fn}"),
                       across(c("diff.MIhat"),list(var=var),.names="{fn}_{col}"),
                       across(-c("comb.coef","hbeta","diff.MIhat"),mean), .groups = 'drop')
    summary.df$true.coef<-true.coef
    
    summary.df <-summary.df%>%
      dplyr::rename(MI.coef=comb.coef_mean,
                    true.total=comb.coef_var,
                    hat.coef=hbeta_mean,
                    true.within1=hbeta_var,
                    total=total.var,
                    true.within2=hbeta.var,
                    true.between2=var_diff.MIhat)
    summary.df$true.between1=summary.df$true.total-summary.df$true.within1
    summary.df$true.between=summary.df$true.total-summary.df$true.within2
    
    ##scale
    summary.df$bias=100*summary.df$bias
    summary.df$total=10000*summary.df$total
    summary.df$true.total=10000*summary.df$true.total
    summary.df$within=10000*summary.df$within
    summary.df$true.within2=10000*summary.df$true.within2
    summary.df$between=10000*summary.df$between
    summary.df$true.between=10000*summary.df$true.between
    summary.df$t.coverage=100*summary.df$t.coverage
    summary.df$missinfo=100*summary.df$missinfo
    
    
    
    
    #note: between here is after adjustment for small m, ie. between=(1+1/m)*between
    #rubin paper: multiple imputation after 18 years  page 482
    
    sum.list[[i]]<-summary.df[,c("bias",
                                 "total","true.total",
                                 "within","true.within2",
                                 "between","true.between",
                                 "t.coverage","t.ciwidth",
                                 "missinfo")]
    
  }
  
  #sum.list %>% map(~.x[1,])
  out=list()
  for(i in 1:L){
    ll<-lapply(sum.list,function(x) x[i,])
    ll<-do.call(rbind,ll)
    ll<-ll %>% add_column(method=c("mice.pmm","mice.midastouch","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"),.before = "bias")
    ll$beta.group=i
    out[[i]]<-ll
  }
  out
  all.df=do.call(rbind,out)
  all.df$beta.group=as.factor(all.df$beta.group)
  as.matrix(all.df[,c("total","true.total","within","true.within2","between","true.between")])
  
}



booterror<-function(sim.result=sim12,model=1,runs=100,seedset=SEED){
  num.methods=8
  L=7
  d=num.methods*L
  M=matrix(NA,nrow=d*runs,ncol=6)
  for(r in 1:runs){
    from=(r-1)*d+1
    to=r*d
    M[from:to,]=bootbeta(sim.result=sim.result,model=model,seed=seedset[r])
  }
  M.df=as.data.frame(M)
  M.df$method=factor(rep(c("mice.pmm","mice.midastouch","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"),L*runs),levels=c("mice.pmm","mice.midastouch","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"))
  M.df$beta.group=factor(rep(rep(1:L,each=8),runs))
  colnames(M.df)[1:6]=c("total","true.total","within","true.within","between","true.between")
  
  M.sumsd<-M.df %>% group_by(beta.group,method) %>%
    dplyr::summarise_all(sd)
  M.sum<-M.df %>% group_by(beta.group,method) %>%
    dplyr::summarise_all(list(sd = ~sd(.),se = ~sd(.x/sqrt(runs))))
  apply(M.sum[,9:14],2,max)
}




