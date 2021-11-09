
if(hoc == "immunotox" & !origALI){
    load(file=paste('fit_revised',hoc,probmassoutside,'.Rdata'))
    for(g in 1:14){
      df_group[[g]][[1]]$Q1 = Q1_ALI/100
    }
  }else{
load(file=paste('fit_',hoc,probmassoutside,'.Rdata'))
}

## ----------------------------
df.pq <- make_distr_dataframe(df_group) 

## ----------------------------
pfunctions <- get_pfunctions(df_group) 
pfunctions_histo <- get_pfunctions_histo(df_group) 


## ----------------------------
info <- do.call('rbind',lapply(1:length(pfunctions),function(i){
  do.call('rbind',lapply(1:length(pfunctions[[i]]),function(j){data.frame(i=i,j=j,expert = pfunctions[[i]][[j]]$expert, cluster = pfunctions[[i]][[j]]$cluster)}))
  }))

## ----------------------------
## adjust curve for the ranges
dose_curve = seq(-4,4,length.out = 300)

df_env <- get_env(info,dose_curve,pfunctions)
df_env$type = 'parametric'

df_env_histo <- get_env(info,dose_curve,pfunctions_histo)

df_env_histo$type = 'histogram'

list_env <- get_env_list(info,dose_curve,pfunctions)

df_Q1 <- cbind(info,do.call('rbind',lapply(1:nrow(info),function(rr){
  temp = df_group[[info[rr,"i"]]][[info[rr,"j"]]]$Q1
  data.frame(lower = temp[1], upper = temp[2],category = hoc)
})))

df_Q2 <- cbind(info,do.call('rbind',lapply(1:nrow(info),function(rr){
  temp1 = df_group[[info[rr,"i"]]][[info[rr,"j"]]]$fit
  if(temp1$dist == "normal"){
    temp = qnorm(c(0.05,0.95),temp1$param$mean,temp1$param$sd)
  }
    
  if(temp1$dist == "t"){
    temp = qt(c(0.05,0.95),temp1$param$df)*temp1$param$scale + temp1$param$location
  }
  if(temp1$dist == "sn"){
    temp = qsn(c(0.05,0.95),temp1$param$xi,temp1$param$omega,temp1$param$alpha)
  }
  if(temp1$dist=="st"){
    temp = qst(c(0.05,0.95),temp1$param$xi,temp1$param$omega,temp1$param$alpha,temp1$param$nu)
  }
  if(temp1$dist=="normal_mix"){
    sam =   temp1$param$pmix*rnorm(10^4,temp1$param$mean1,temp1$param$sd1)+
      (1-temp1$param$pmix)*rnorm(10^4,temp1$param$mean2,temp1$param$sd2)
    temp = quantile(sam,c(0.05,0.95))
  }
  if(temp1$dist == "sn_mix"){
    sam = temp1$param$pmix*rsn(10^4,temp1$param$xi1,temp1$param$omega1,temp1$param$alpha1)+
      (1-temp1$param$pmix)*rsn(10^4,temp1$param$xi2,temp1$param$omega2,temp1$param$alpha2)
    temp = quantile(sam,c(0.05,0.95))
  }
  if(temp1$dist == "beta"){
    lowup = range(temp1$df_fit$log10dd)
    temp = qbeta(c(0.05,0.95),shape1=temp1$param$shape1,shape2=temp1$param$shape2)*(lowup[2]-lowup[1]) + lowup[1]
  }

  data.frame(lower = temp[1], upper = temp[2],category = hoc)
})))


