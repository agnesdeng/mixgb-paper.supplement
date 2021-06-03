


set.seed(2021, kind = "L'Ecuyer-CMRG")
SEED=round(runif(1000,min=0,max=1000000))




reportboot.cat<-function(sim.result=cat34,runs=100,seedset=SEED){
  num.methods=8
  #num of parameters
  L=8
  d=num.methods*L
  M=matrix(NA,nrow=d*runs,ncol=6)
  for(r in 1:runs){
    from=(r-1)*d+1
    to=r*d
    M[from:to,]=bootalpha(sim.result=sim.result,seed=seedset[r])
  }
  M.df=as.data.frame(M)
  M.df$method=factor(rep(c("mice.logreg","mice.logregboot","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"),L*runs),levels=c("mice.logreg","mice.logregboot","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"))
  M.df$beta.group=factor(rep(rep(1:L,each=8),runs))
  colnames(M.df)[1:6]=c("total","true.total","within","true.within","between","true.between")
  M.df$diff_total=M.df$total-M.df$true.total
  M.df$diff_within=M.df$within-M.df$true.within
  M.df$diff_between=M.df$between-M.df$true.between
  M.sum<-M.df %>% group_by(beta.group,method) %>%
    dplyr::summarise(across(c("diff_total","diff_within","diff_between"), list(mean=mean, sd = sd),.names = "{col}.{fn}"))
  M.sum$diff_total.se<-M.sum$diff_total.sd/sqrt(runs)
  M.sum$diff_within.se<-M.sum$diff_within.sd/sqrt(runs)
  M.sum$diff_between.se<-M.sum$diff_between.sd/sqrt(runs)
  M.sum
}




bootalpha<-function(sim.result=result,seed=1){
  #L: number of alphas
  L<-8
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
    #summary.df$true.coef<-true.coef

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
    summary.df$total=100*summary.df$total
    summary.df$true.total=100*summary.df$true.total
    summary.df$within=100*summary.df$within
    summary.df$true.within2=100*summary.df$true.within2
    summary.df$between=100*summary.df$between
    summary.df$true.between=100*summary.df$true.between
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
    ll<-ll %>% tibble::add_column(method=c("mice.logreg","mice.logregboot","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"),.before = "bias")
    ll$beta.group=i
    out[[i]]<-ll
  }
  out
  all.df=do.call(rbind,out)
  all.df$beta.group=as.factor(all.df$beta.group)
  as.matrix(all.df[,c("total","true.total","within","true.within2","between","true.between")])

}



booterror.cat<-function(sim.result=cat34,runs=100,seedset=SEED){
  num.methods=8
  L=8
  d=num.methods*L
  M=matrix(NA,nrow=d*runs,ncol=6)
  for(r in 1:runs){
    from=(r-1)*d+1
    to=r*d
    M[from:to,]=bootalpha(sim.result=sim.result,seed=seedset[r])
  }
  M.df=as.data.frame(M)
  M.df$method=factor(rep(c("mice.logreg","mice.logregboot","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"),L*runs),levels=c("mice.pmm","mice.midastouch","mice.cart","mice.rf","missRanger","mixgb.null","mixgb.pmm1","mixgb.pmm2"))
  M.df$beta.group=factor(rep(rep(1:L,each=8),runs))
  colnames(M.df)[1:6]=c("total","true.total","within","true.within","between","true.between")

  M.sumsd<-M.df %>% group_by(beta.group,method) %>%
    dplyr::summarise_all(sd)
  M.sum<-M.df %>% group_by(beta.group,method) %>%
    dplyr::summarise_all(list(sd = ~sd(.),se = ~sd(.x/sqrt(runs))))
  apply(M.sum[,9:14],2,max)
}
