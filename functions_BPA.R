
nano.unit = "ng/kg.bw/day"
mikro.unit = "μg/kg.bw/day"
milli.unit = "mg/kg.bw/day"

#load("fit_distributions.Rdata")

do.one.data.extration <- function(expert,clid){
reason <- NULL
reason$Q1 <- read.xlsx(xlsxFile=expert_xlsx[expert],sheet= sheet, rows = 3, cols = clid+1, colNames = FALSE)$X1

if(read.xlsx(xlsxFile=expert_xlsx[expert],sheet = sheet, rows = 13, 
             cols = clid+1, colNames = FALSE)$X1){
  Q1 <- read.xlsx(xlsxFile=expert_xlsx[expert],sheet= sheet, rows = 14:15, cols = clid+1, colNames = FALSE)$X1/100
  Q1 <- range(Q1) #just in case they happen to be the other way around
}else{
  Q1 <- read.xlsx(xlsxFile=expert_xlsx[expert],sheet= sheet, rows = 4:12, cols = clid+1, colNames = FALSE)$X1
  #approx probability scale 
  aps <- c(0,1,5,10,33,66,90,95,99,100)/100
  Q1 <- range(aps[min(length(aps) - which(Q1)):max(length(aps) - which(Q1) + 1)])
}

reason$Q2 <- read.xlsx(xlsxFile=expert_xlsx[expert],sheet= sheet, rows = 16, cols = clid+1, colNames = FALSE)$X1

unit <- colnames(read.xlsx(xlsxFile=expert_xlsx[expert],sheet= sheet, rows = 17, cols = clid+1)) 

Q2 <- NULL
Q2$d <-  c(read.xlsx(xlsxFile=expert_xlsx[expert],sheet= sheet, rows = 20:34, cols = clid+1 , colNames = FALSE)$X1,
           read.xlsx(xlsxFile=expert_xlsx[expert],sheet= sheet, rows = 19, cols = clid+1 , colNames = FALSE)$X1)

Q2$log10d <- log10(Q2$d)

chips <- c(0,read.xlsx(xlsxFile=expert_xlsx[expert],sheet= sheet, rows = 35:49, cols = clid+1 , colNames = FALSE)$X1[1:(length(Q2$d)-1)])

if(FALSE){
  # when bins are given no chips, adjust the hard limits 
  nozeroprob = which(chips>0)
  nozeroprob = c(min(nozeroprob)-1,nozeroprob,max(nozeroprob)+1)[1:length(Q2$d)]
  Q2$d = Q2$d[nozeroprob]
  Q2$log10d = Q2$log10d[nozeroprob]
  chips = chips[nozeroprob] 
}

## turn data into cdfs 

#load("probmassoutside.Rdata")
# get probability for each bin
Q2$p_org <- chips/sum(chips) #probability in bin
# turn into points from the cdf
Q2$cdf_org <- cumsum(Q2$p_org)
# adjust for probability mass outside the histogram
Q2$p_adj <- Q2$p_org*(1-2*probmassoutside)
# add 0.01 to lowest value
Q2$p_adj[which(Q2$p_adj==0)] <- probmassoutside/sum(length(Q2$p_adj==0)) ## the sum should be 0.99 sum(Q2$p_adj)
# turn into points from the cdf
Q2$cdf_adj <- cumsum(Q2$p_adj)

# transform to ng 
if(unit == "mg/kg.bw/day"){
  Q2$log10d = Q2$log10d+6
  Q2$d = Q2$d*10^6
  unit = "ng/kg.bw/day"
}else if(unit == "ng/kg.bw/day"){
  
}else{
  Q2$log10d = Q2$log10d+3
  Q2$d = Q2$d*10^3
  unit = "ng/kg.bw/day"
}

return(list(Q1=Q1,Q2=Q2,reason=reason,unit=unit,expert = expert,clid = clid))
}

do.one.data.extration_12july <- function(expert,Q1){
  reason <- NULL
  
  #Q1 are consensus 
  
  reason$Q2 <- NULL
  
  clid = 1
  
  unit <- colnames(read.xlsx(xlsxFile=expertJuly12th_xlsx,sheet = LETTERS[expert], rows = 17, cols = clid+1)) 
  
  Q2 <- NULL
  Q2$d <-  c(read.xlsx(xlsxFile=expertJuly12th_xlsx,sheet = LETTERS[expert], rows = 20:34, cols = clid+1 , colNames = FALSE)$X1,
             read.xlsx(xlsxFile=expertJuly12th_xlsx,sheet = LETTERS[expert], rows = 19, cols = clid+1 , colNames = FALSE)$X1)
  
  Q2$log10d <- log10(Q2$d)
  
  chips <- c(0,read.xlsx(xlsxFile=expertJuly12th_xlsx,sheet = LETTERS[expert], rows = 35:49, cols = clid+1 , colNames = FALSE)$X1[1:(length(Q2$d)-1)])
  
  if(FALSE){
    # when bins are given no chips, adjust the hard limits 
    nozeroprob = which(chips>0)
    nozeroprob = c(min(nozeroprob)-1,nozeroprob,max(nozeroprob)+1)[1:length(Q2$d)]
    Q2$d = Q2$d[nozeroprob]
    Q2$log10d = Q2$log10d[nozeroprob]
    chips = chips[nozeroprob] 
  }
  
  ## turn data into cdfs 
  
  #load("probmassoutside.Rdata")
  # get probability for each bin
  Q2$p_org <- chips/sum(chips) #probability in bin
  # turn into points from the cdf
  Q2$cdf_org <- cumsum(Q2$p_org)
  # adjust for probability mass outside the histogram
  Q2$p_adj <- Q2$p_org*(1-2*probmassoutside)
  # add 0.01 to lowest value
  Q2$p_adj[which(Q2$p_adj==0)] <- probmassoutside/sum(length(Q2$p_adj==0)) ## the sum should be 0.99 sum(Q2$p_adj)
  # turn into points from the cdf
  Q2$cdf_adj <- cumsum(Q2$p_adj)
  
  # transform to ng 
  if(unit == "mg/kg.bw/day"){
    Q2$log10d = Q2$log10d-6
    Q2$d = Q2$d/10^6
    unit = "ng/kg.bw/day"
  }else if(unit == "ng/kg.bw/day"){
    
  }else{
    Q2$log10d = Q2$log10d-3
    Q2$d = Q2$d/10^3
    unit = "ng/kg.bw/day"
  }
  return(list(Q1=Q1,Q2=Q2,reason=reason,unit=unit,expert = expert,clid = clid))
}


do.one.fit = function(df,fit.mixture=FALSE,adj=TRUE){

  ## select if we are to adjust for probability outside bins or not
  # no adjustment
  if(!adj){
    df$Q2$cdf <- df$Q2$cdf_org
    df$Q2$p <- df$Q2$p_org
  }
  # adjustment
  if(adj){
    df$Q2$cdf <- df$Q2$cdf_adj
    df$Q2$p <- df$Q2$p_adj
  }
  
  ## Fit a distribution to data the logged (base 10) scale. 
  fit_log10 <- SHELF::fitdist(vals = df$Q2$log10d, 
                              probs = df$Q2$cdf, 
                              lower = min(df$Q2$log10d)-1e-6, 
                              upper = max(df$Q2$log10d)+1e-6)
  
  
  fit_log10$ssq <- fit_log10$ssq[c(1,2,3,4,5,11)]
  
  ## metrics to re-evaluate best fit
  fit_log10$num_param = c(2,3,3,4,5,2)
  fit_log10$R2 = 1-fit_log10$ssq/sum(df$Q2$cdf^2) 
  ## adjR2 are giving very similar values (close to 99%)
  fit_log10$adjR2 = 1-(1-fit_log10$R2)*(length(df$Q2$cdf)-1)/(length(df$Q2$cdf)-fit_log10$num_param-1)
  
  ## Derive goodness of fit based on linear approximation of cdf
  {
    fit_log10$ssq_approx <- fit_log10$ssq 
    pp <- seq(0.001,0.999,by=0.001)
    app_inv <- approx(fit_log10$probs,fit_log10$vals,n = 20)
    app <- app_inv
    app$x <- app_inv$y
    app$y <- app_inv$x
    
    {
      cdf <- pt((app$x-fit_log10$Student.t$location)/fit_log10$Student.t$scale, fit_log10$Student.t$df)
      fit_log10$ssq_approx[names(fit_log10$ssq)=='t'] = sum((app$y-cdf)^2)
    }
    {
      cdf <- pnorm(app$x,fit_log10$Normal$mean,fit_log10$Normal$sd)
      fit_log10$ssq_approx[names(fit_log10$ssq)=='normal'] = sum((app$y-cdf)^2)
    }
    
    {
      cdf <- pst(app$x,fit_log10$Skewed.normal$xi,fit_log10$Skewed.normal$omega,fit_log10$Skewed.normal$alpha)
      fit_log10$ssq_approx[names(fit_log10$ssq)=='sn'] = sum((app$y-cdf)^2) 
    }
    
    {
      cdf <- pst(app$x,fit_log10$Skewed.t$xi,fit_log10$Skewed.t$omega,fit_log10$Skewed.t$alpha,fit_log10$Skewed.t$nu)
      fit_log10$ssq_approx[names(fit_log10$ssq)=='st'] = sum((app$y-cdf)^2) 
    }

    {
      cdf <-   fit_log10$Mix.of.normals$pmix*pnorm(app$x,fit_log10$Mix.of.normals$mean1,fit_log10$Mix.of.normals$sd1)+
        (1-fit_log10$Mix.of.normals$pmix)*pnorm(app$x,fit_log10$Mix.of.normals$mean2,fit_log10$Mix.of.normals$sd2)
        fit_log10$ssq_approx[names(fit_log10$ssq)=='normal_mix'] = sum((app$y-cdf)^2)
    }
    
    {
      cdf <- fit_log10$Mix.of.skewed.normals$pmix*
        psn(app$x,fit_log10$Mix.of.skewed.normals$xi1,fit_log10$Mix.of.skewed.normals$omega1,
            fit_log10$Mix.of.skewed.normals$alpha1) + 
        (1-fit_log10$Mix.of.skewed.normals$pmix)*
        psn(app$x,fit_log10$Mix.of.skewed.normals$xi2,fit_log10$Mix.of.skewed.normals$omega2,
            fit_log10$Mix.of.skewed.normals$alpha2)
        fit_log10$ssq_approx[names(fit_log10$ssq)=='sn_mix'] = sum((app$y-cdf)^2) 
    }
    
    {
     cdf <- pbeta((app$x-fit_log10$limits$lower)/(fit_log10$limits$upper-fit_log10$limits$lower),fit_log10$Beta$shape1,fit_log10$Beta$shape2)
      fit_log10$ssq_approx[names(fit_log10$ssq)=='beta'] = sum((app$y-cdf)^2) 
    }
    
    fit_log10$R2_approx = 1-fit_log10$ssq_approx/sum(df$Q2$cdf^2)
    fit_log10$adjR2_approx = 1-(1-fit_log10$R2_approx)*(length(df$Q2$cdf)-1)/(length(df$Q2$cdf)-fit_log10$num_param-1)
    
  }
  
  
  ## three different options for goodness of fit. Here we use ssq based on 20 interpolated points, which penalises overfit of mixtures  
  #temp <- -fit_log10$ssq
  #temp <- round(fit_log10$adjR2,3)
  temp <- -fit_log10$ssq_approx
  #temp <- round(fit_log10$adjR2_approx,3)
  temp[(length(df$Q2$cdf)-fit_log10$num_param-1)<1] = 10^3 #remove distributions with more than data-1 number of parameters
  fit_log10$best.fitting$best.fit <- names(fit_log10$ssq)[min(which(temp == max(temp)))] #use min to select the most parsimonious in presence of mixtures   
  if(!fit.mixture){
    nam_no_mix <- names(fit_log10$ssq)
    temp_no_mix <- temp[which(nam_no_mix != "normal_mix")]
    nam_no_mix <- nam_no_mix[which(nam_no_mix != "normal_mix")]
    fit_log10$best.fitting$best.fit <- nam_no_mix[min(which(temp_no_mix == max(temp_no_mix)))] #use min to select the most parsimonious in presence of mixtures   
  }
  ## Plot CDF of the best fit 
  {
    plotcdf = FALSE
    pp <- seq(0.001,0.999,by=0.001)
    
    if(plotcdf){
      plot(c(fit_log10$limits$lower,fit_log10$vals,fit_log10$limits$upper),
         c(0,fit_log10$probs,1),
         type='l', xlab = paste("log10dose",df$unit), 
         ylab = 'cdf', 
         main = paste0(df$cluster,", expert ",LETTERS[df$expert]))
    }
    if(fit_log10$best.fitting$best.fit == 'normal'){
      log10dd <- qnorm(pp, fit_log10$Normal$mean,fit_log10$Normal$sd)
      cdf <- pnorm(log10dd,fit_log10$Normal$mean,fit_log10$Normal$sd)
      if(plotcdf){
        lines(log10dd,cdf,col='blue') 
      }
      pfit <- function(log10dd){
        pnorm(log10dd,fit_log10$Normal$mean,fit_log10$Normal$sd)
      }
      param <- fit_log10$Normal
    }
    
    if(fit_log10$best.fitting$best.fit == 't'){
      log10dd <- fit_log10$Student.t$location + qt(pp, fit_log10$Student.t$df)*fit_log10$Student.t$scale
      cdf <- pt((log10dd-fit_log10$Student.t$location)/fit_log10$Student.t$scale, fit_log10$Student.t$df)
      if(plotcdf){
        lines(log10dd,cdf,col='blue') 
      }
      pfit <- function(log10dd){
        pt((log10dd-fit_log10$Student.t$location)/fit_log10$Student.t$scale, 
           fit_log10$Student.t$df)}
      param <- fit_log10$Student.t
    }
    
    if(fit_log10$best.fitting$best.fit == 'sn'){
      log10dd <- qsn(pp, fit_log10$Skewed.normal$xi,fit_log10$Skewed.normal$omega,fit_log10$Skewed.normal$alpha)
      cdf <- psn(log10dd,fit_log10$Skewed.normal$xi,fit_log10$Skewed.normal$omega,fit_log10$Skewed.normal$alpha)
      if(plotcdf){
        lines(log10dd,cdf,col='blue') 
      }
      pfit <- function(log10dd){
        psn(log10dd,fit_log10$Skewed.normal$xi,fit_log10$Skewed.normal$omega,fit_log10$Skewed.normal$alpha)
      }
      param <- fit_log10$Skewed.normal
    }
    
    if(fit_log10$best.fitting$best.fit == 'st'){
      log10dd <- qst(pp, fit_log10$Skewed.t$xi,fit_log10$Skewed.t$omega,fit_log10$Skewed.t$alpha,fit_log10$Skewed.t$nu)
      cdf <- pst(log10dd,fit_log10$Skewed.t$xi,fit_log10$Skewed.t$omega,fit_log10$Skewed.t$alpha,fit_log10$Skewed.t$nu)
      if(plotcdf){
        lines(log10dd,cdf,col='blue') 
      }
      pfit <- function(log10dd){
        pst(log10dd,fit_log10$Skewed.t$xi,fit_log10$Skewed.t$omega,fit_log10$Skewed.t$alpha,fit_log10$Skewed.t$nu)      
      }
      param <- fit_log10$Skewed.t
    }
    
    if(fit_log10$best.fitting$best.fit == 'normal_mix'){
      log10dd <- seq(fit_log10$limits$lower,fit_log10$limits$upper,length.out = 200)
      cdf <-  fit_log10$Mix.of.normals$pmix*pnorm(log10dd,fit_log10$Mix.of.normals$mean1,fit_log10$Mix.of.normals$sd1)+
        (1-fit_log10$Mix.of.normals$pmix)*pnorm(log10dd,fit_log10$Mix.of.normals$mean2,fit_log10$Mix.of.normals$sd2)
      if(plotcdf){
        lines(log10dd,cdf,col='blue') 
      }
      pfit <- function(log10dd){
        fit_log10$Mix.of.normals$pmix*pnorm(log10dd,fit_log10$Mix.of.normals$mean1,fit_log10$Mix.of.normals$sd1)+
          (1-fit_log10$Mix.of.normals$pmix)*pnorm(log10dd,fit_log10$Mix.of.normals$mean2,fit_log10$Mix.of.normals$sd2)
      }
      param <- fit_log10$Mix.of.normals
    }
    
    if(fit_log10$best.fitting$best.fit == 'sn_mix'){
      cdf <-  fit_log10$Mix.of.skewed.normals$pmix*
        psn(log10dd,fit_log10$Mix.of.skewed.normals$xi1,fit_log10$Mix.of.skewed.normals$omega1,fit_log10$Mix.of.skewed.normals$alpha1)+
        (1-fit_log10$Mix.of.skewed.normals$pmix)*
        psn(log10dd,fit_log10$Mix.of.skewed.normals$xi2,fit_log10$Mix.of.skewed.normals$omega2,fit_log10$Mix.of.skewed.normals$alpha2)
      if(plotcdf){
        lines(log10dd,cdf,col='blue') 
      }
      pfit <- function(log10dd){
        psn(log10dd,fit_log10$Mix.of.skewed.normals$xi1,fit_log10$Mix.of.skewed.normals$omega1,fit_log10$Mix.of.skewed.normals$alpha1)+
          (1-fit_log10$Mix.of.skewed.normals$pmix)*
          psn(log10dd,fit_log10$Mix.of.skewed.normals$xi2,fit_log10$Mix.of.skewed.normals$omega2,fit_log10$Mix.of.skewed.normals$alpha2)
      }
      param <- fit_log10$Mix.of.skewed.normals
    }
    
    if(fit_log10$best.fitting$best.fit == 'beta'){
      log10dd <- fit_log10$limits$lower + qbeta(pp, fit_log10$Beta$shape1,fit_log10$Beta$shape2)*
        (fit_log10$limits$upper-fit_log10$limits$lower)
      cdf <- pbeta((log10dd-fit_log10$limits$lower)/(fit_log10$limits$upper-fit_log10$limits$lower),
                   fit_log10$Beta$shape1,fit_log10$Beta$shape2)
      if(plotcdf){
        lines(log10dd,cdf,col='blue') 
      }
      pfit <- function(log10dd){
        pbeta((log10dd-fit_log10$limits$lower)/(fit_log10$limits$upper-fit_log10$limits$lower),
              fit_log10$Beta$shape1,fit_log10$Beta$shape2)
      }
      param <- fit_log10$Beta
    }
    if(plotcdf){
      mtext(paste("distribution:",fit_log10$best.fitting$best.fit))
    }
    
  }
  
  ## Plot PDF of the best fit
  
  {
  
    if(fit_log10$best.fitting$best.fit == "normal"){
      leg = "normal distribution"
    }
    if(fit_log10$best.fitting$best.fit == "t"){
      leg = "t distribution"
    }
    if(fit_log10$best.fitting$best.fit == "sn"){
      leg = "skewed normal distribution"
    }
    if(fit_log10$best.fitting$best.fit == "st"){
      leg = "skewed t distribution"
    }
    if(fit_log10$best.fitting$best.fit == "normal_mix"){
      leg = "mixture of two normal distribution"
    }
    if(fit_log10$best.fitting$best.fit == "beta"){
      leg = "beta distribution"
    }
    {
    density.pairs <- NULL
    difflog10d <- c(0,df$Q2$log10d[-1] - df$Q2$log10d[-length(df$Q2$d)]) 
    temp <- df$Q2$p[-1]/difflog10d[-1]#/.5
    xlabels = seq(ceiling(min(fit_log10$limits$lower)),floor(max(fit_log10$limits$upper)),by = 1)
    ylabels = seq(0,max(temp)*1.3,length.out = 6)
    density.pairs$h  <- c(0,temp[rep(1:length(temp),each=2)],0)
    density.pairs$d <- rep(df$Q2$d,each=2)
    rr <- range(log10(density.pairs$d))
    rr_breaks <- seq(rr[1],rr[2],length.out = 4)
    plot(log10(density.pairs$d),density.pairs$h,type='l',
         ylim = range(ylabels),
         xlab= "Estimated lowest BMD in this cluster \n (HED, ng BPA/kg bw per day)", 
         ylab = 'Probability density',
         main = paste0(df$cluster, " expert ",LETTERS[df$expert]), xaxt = "n")
    axis(side = 1, at = xlabels, labels = round(10^xlabels,3))#round(10^rr_breaks,2))
    }
    legend('topright',c(leg,"roulette histogram"),col = c('blue','black'),
           lty = c(1,1), bty='n')
    
    
    if(fit_log10$best.fitting$best.fit == 'normal'){
      log10dd <- qnorm(pp, fit_log10$Normal$mean,fit_log10$Normal$sd)
      prob_range <- pnorm(fit_log10$limits$upper,fit_log10$Normal$mean,fit_log10$Normal$sd)-pnorm(fit_log10$limits$lower,fit_log10$Normal$mean,fit_log10$Normal$sd)
      pdf <- dnorm(log10dd,fit_log10$Normal$mean,fit_log10$Normal$sd)
      
      lines(log10dd,pdf,col='blue') 
    }
    
    if(fit_log10$best.fitting$best.fit == 't'){
      log10dd <- fit_log10$Student.t$location + 
        qt(pp,fit_log10$Student.t$df)*fit_log10$Student.t$scale
      
      prob_range <- pt((fit_log10$limits$upper-fit_log10$Student.t$location)/fit_log10$Student.t$scale, fit_log10$Student.t$df)-pt((fit_log10$limits$lower-fit_log10$Student.t$location)/fit_log10$Student.t$scale, fit_log10$Student.t$df)
      
      pdf <- dt((log10dd-fit_log10$Student.t$location)/fit_log10$Student.t$scale, fit_log10$Student.t$df)/fit_log10$Student.t$scale
      
      lines(log10dd,pdf,col='blue') 
      
    }
    
    if(fit_log10$best.fitting$best.fit == 'beta'){
      log10dd <- fit_log10$limits$lower + qbeta(pp, fit_log10$Beta$shape1,fit_log10$Beta$shape2)*
        (fit_log10$limits$upper-fit_log10$limits$lower)
      prob_range <- pbeta((fit_log10$limits$upper-fit_log10$limits$lower)/(fit_log10$limits$upper-fit_log10$limits$lower),
                          fit_log10$Beta$shape1,fit_log10$Beta$shape2)-
        pbeta((fit_log10$limits$lower-fit_log10$limits$lower)/(fit_log10$limits$upper-fit_log10$limits$lower),
              fit_log10$Beta$shape1,fit_log10$Beta$shape2)
      pdf <- dbeta((log10dd-fit_log10$limits$lower)/(fit_log10$limits$upper-fit_log10$limits$lower),
                   fit_log10$Beta$shape1,fit_log10$Beta$shape2)/(fit_log10$limits$upper-fit_log10$limits$lower)
      
      lines(log10dd,pdf,col='blue') 
    }
    
    if(fit_log10$best.fitting$best.fit == 'sn'){
      log10dd <- qsn(pp, fit_log10$Skewed.normal$xi,fit_log10$Skewed.normal$omega,fit_log10$Skewed.normal$alpha)
      pdf <- dsn(log10dd,fit_log10$Skewed.normal$xi,fit_log10$Skewed.normal$omega,fit_log10$Skewed.normal$alpha)
      lines(log10dd,pdf,col='blue') 
    }
    
    if(fit_log10$best.fitting$best.fit == 'st'){
      log10dd <- qst(pp, fit_log10$Skewed.t$xi,fit_log10$Skewed.t$omega,fit_log10$Skewed.t$alpha,fit_log10$Skewed.t$nu)
      pdf <- dst(log10dd,fit_log10$Skewed.t$xi,fit_log10$Skewed.t$omega,fit_log10$Skewed.t$alpha,fit_log10$Skewed.t$nu)
      lines(log10dd,pdf,col='blue') 
    }
   
    if(fit_log10$best.fitting$best.fit == 'normal_mix'){
      log10dd <- seq(fit_log10$limits$lower,fit_log10$limits$upper,length.out = 200)
      pdf <- fit_log10$Mix.of.normals$pmix*dnorm(log10dd,fit_log10$Mix.of.normals$mean1,fit_log10$Mix.of.normals$sd1)+
        (1-fit_log10$Mix.of.normals$pmix)*dnorm(log10dd,fit_log10$Mix.of.normals$mean2,fit_log10$Mix.of.normals$sd2)
      lines(log10dd,pdf,col='blue') 
    }
    
    if(fit_log10$best.fitting$best.fit == 'sn_mix'){
      log10dd <- seq(fit_log10$limits$lower,fit_log10$limits$upper,length.out = 200)
      pdf <- fit_log10$Mix.of.skewed.normals$pmix*dsn(log10dd,fit_log10$Mix.of.skewed.normals$xi1,fit_log10$Mix.of.skewed.normals$omega1,fit_log10$Mix.of.skewed.normals$alpha1)+
        (1-fit_log10$Mix.of.normals$pmix)*dsn(log10dd,fit_log10$Mix.of.skewed.normals$xi2,fit_log10$Mix.of.skewed.normals$omega2,fit_log10$Mix.of.skewed.normals$alpha2)
      lines(log10dd,pdf,col='blue') 
    }
    
    }
  return(list(df_fit = data.frame(log10dd=log10dd,cdf=cdf,pdf=pdf,expert = df$expert),
              density.pairs=density.pairs,
              ssq = fit_log10$ssq,
              ssq_approx = fit_log10$ssq_approx,
              #diffssq = fit_log10$ssqperparam,
              param = param,
              dist = fit_log10$best.fitting$best.fit,
              pfit = pfit,
              boxcdf = approxfun(c(-10^10,df$Q2$log10d,10^10), 
                                             c(0,df$Q2$cdf,max(df$Q2$cdf)))))
}


get_pfunctions <- function(df_group){
  pfunctions  <- lapply(1:length(df_group),function(expert){
    pfunc <- lapply(1:length(df_group[[expert]]),function(clid){
      df <- df_group[[expert]][[clid]]
      pbmd <- function(log10nd){df$Q1*df$fit$pfit(log10nd)}
      cluster = names(df_group[[expert]])[clid]
      return(list(pbmd=pbmd,expert = df$expert, expertnr = df$expertnr, cluster = cluster))
    })
    return(pfunc)
  })
  return(pfunctions)
}


get_pfunctions_histo <- function(df_group){
  pfunctions  <- lapply(1:length(df_group),function(expert){
    pfunc <- lapply(1:length(df_group[[expert]]),function(clid){
      df <- df_group[[expert]][[clid]]
      pbmd <- function(log10nd){df$Q1*df$fit$boxcdf(log10nd)}
      cluster = names(df_group[[expert]])[clid]
      return(list(pbmd=pbmd,expert = df$expert, expertnr = df$expertnr, cluster = cluster))
    })
    return(pfunc)
  })
  return(pfunctions)
}

make_distr_dataframe <- function(df_group){
   df.pq <- do.call('rbind',lapply(1:length(df_group),function(expert){
    pq <- lapply(1:length(df_group[[expert]]),function(clid){
      df <- df_group[[expert]][[clid]]
      ## transform to have unit nano.unit
      #if(df$unit == nano.unit){
      #  log10x = df$fit$df_fit$log10dd  
      #}else if(df$unit == milli.unit){
      #  log10x = df$fit$df_fit$log10dd+6
      #}else{
      #  log10x = df$fit$df_fit$log10dd+3
      #}
      log10x = df$fit$df_fit$log10dd
     
      cdf_lower = df$Q1[1]*df$fit$df_fit$cdf
      cdf_upper = df$Q1[2]*df$fit$df_fit$cdf
      cdf_hist_lower = df$Q1[1]*df$fit$boxcdf(df$fit$df_fit$log10dd)
      cdf_hist_upper = df$Q1[2]*df$fit$boxcdf(df$fit$df_fit$log10dd)
      cluster = names(df_group[[expert]])[clid]
      return(data.frame(log10d = log10x,cdf_lower = cdf_lower, cdf_upper=cdf_upper,cdf_hist_lower = cdf_hist_lower,
                        cdf_hist_upper = cdf_hist_upper,
                        expert = paste("expert",LETTERS[df$expert]), expertnr = df$expert, cluster = cluster))
    })
    return(do.call('rbind',pq))
  }))
  return(df.pq)
}


plot_clusters <- function(df.pq){
  xa = seq(min(unique(round(df.pq$log10d,0)))-1,
           max(unique(round(df.pq$log10d,0)))+1,by=2)
  cls = unique(df.pq$cluster)
  df.ext = do.call('rbind',lapply(1:length(cls),function(clid){
    temp <-   df.pq[df.pq$cluster==cls[clid],]
    agg <- aggregate(temp$log10d,by=list(expert=temp$expert),'max')
    cdf_lower <- aggregate(temp$cdf_lower,by=list(expert=temp$expert),'max')$x
    cdf_upper <- aggregate(temp$cdf_upper,by=list(expert=temp$expert),'max')$x
    data.frame(agg,cdf_lower,cdf_upper,cluster=cls[clid])
  }))
  df.ext$xend = max(xa)
  df.pq %>%
    ggplot(aes(x=log10d,y=100*cdf_upper,col=cluster))+
    geom_line() +
    geom_line(aes(x=log10d,y=100*cdf_lower,col=cluster))+
    geom_segment(data=df.ext,aes(x=x,xend=xend,y=100*cdf_upper,yend=100*cdf_upper))+
    geom_segment(data=df.ext,aes(x=x,xend=xend,y=100*cdf_lower,yend=100*cdf_lower))+
    facet_wrap(~expert,nrow = 3) +
    theme_bw() +
    ylab('Cumulative probability (%)') +
    ylim(0,100) +
      scale_x_continuous(name="Estimed lowest BMD in the cluster \n (HED, ng BPA/kg bw per day)", 
      breaks=xa, labels=10^xa,limits = NULL)#range(xa)) 
  #xlim(-2,1)
}

plot_clusters_oneexpert <- function(df.pq,experttoplot){
  df.pqexpert <- df.pq %>%
    filter(expert == paste("expert",LETTERS[experttoplot]))
  
  xa = seq(-3,5,by=2)#seq(min(unique(round(df.pq$log10d,0)))-1,
           #max(unique(round(df.pq$log10d,0)))+1,by=2)
  cls = unique(df.pqexpert$cluster)
  df.ext = do.call('rbind',lapply(1:length(cls),function(clid){
    temp <- df.pqexpert[df.pqexpert$cluster==cls[clid],]
    agg <- aggregate(temp$log10d,by=list(expert=temp$expert),'max')
    cdf_lower <- aggregate(temp$cdf_lower,by=list(expert=temp$expert),'max')$x
    cdf_upper <- aggregate(temp$cdf_upper,by=list(expert=temp$expert),'max')$x
    data.frame(agg,cdf_lower,cdf_upper,cluster=cls[clid])
  }))
  df.ext$xend = 6#max(xa)
   df.pqexpert %>%
    ggplot(aes(x=log10d,y=100*cdf_upper,col=cluster))+
    geom_line() +
    geom_line(aes(x=log10d,y=100*cdf_lower,col=cluster))+
    geom_segment(data=df.ext,aes(x=x,xend=xend,y=100*cdf_upper,yend=100*cdf_upper))+
    geom_segment(data=df.ext,aes(x=x,xend=xend,y=100*cdf_lower,yend=100*cdf_lower))+
    theme_bw() +
    ylab('Cumulative probability (%)') +
    ylim(0,100) +
    ggtitle(paste("Expert",LETTERS[experttoplot])) + 
    scale_x_continuous(name="Estimated lowest BMD in the cluster \n (HED, ng BPA/kg bw per day)", 
                       breaks=xa, labels=10^xa,limits = c(-3,6)) 
  #xlim(-2,1)
 #  ggsave(filename = paste("clusteroneexpert",origALI,".jpeg"))
 #  ggsave(filename = paste("clusteroneexpert",origALI,".pdf"))
}

plot_experts <- function(df.pq){
  if(df.pq$type[1] == "histogram"){
    tit = "Uncertainty distributions from interpolated histograms"
  }else{
    tit = "Uncertainty distributions"
  }
  df.pq$expert = paste('expert',LETTERS[df.pq$expert])
  xa = seq(min(unique(round(df.pq$dose,0)))-1,
           max(unique(round(df.pq$dose,0)))+1,by=2)
  cls = unique(df.pq$cluster)
  df.ext = do.call('rbind',lapply(1:length(cls),function(clid){
    temp <-   df.pq[df.pq$cluster==cls[clid],]
    agg <- aggregate(temp$dose,by=list(expert=temp$expert),'max')
    lower_cdf <- aggregate(temp$lower_cdf,by=list(expert=temp$expert),'max')$x
    upper_cdf <- aggregate(temp$upper_cdf,by=list(expert=temp$expert),'max')$x
    data.frame(agg,lower_cdf,upper_cdf,cluster=cls[clid])
  }))
  df.ext$xend = max(xa)
  
  df.pq %>%
    ggplot(aes(x=dose,y=100*upper_cdf,col=cluster,group=expert))+#linetype=expert))+
    geom_line() +
    geom_line(aes(x=dose,y=100*lower_cdf,col=cluster))+
    geom_segment(data=df.ext,aes(x=x,xend=xend,y=100*upper_cdf,yend=100*upper_cdf))+
    geom_segment(data=df.ext,aes(x=x,xend=xend,y=100*lower_cdf,yend=100*lower_cdf))+
    #geom_line(aes(x=log10d,y=cdf_hist_lower),col='black',alpha=0.5)+
   # geom_line(aes(x=log10d,y=cdf_hist_upper),col='black',alpha=0.5)+
    facet_wrap(~cluster, scales = 'free') +
    theme_bw()  +
    ylab('Cumulative probability (%)') +
    ylim(0,100) +
    #ggtitle(tit) +
    scale_x_continuous(name="Estimated lowest BMD in the cluster \n (HED, ng BPA/kg bw per day)", 
                       breaks=xa, labels=10^xa,limits = NULL)#range(xa)) #+
  #xlim(-2,1)
}


plot_experts_with_env <- function(df.pq_range,df_env){
  df.pq_range$expert = paste('expert',LETTERS[df.pq_range$expert])
  df_env2 <- df_env
  df_env2$expert = "envelope"
  df.ee <- rbind(df.pq_range[,colnames(df_env2)],df_env2)
  
  xa = seq(min(unique(round(df.ee$dose,0))),
           max(unique(round(df.ee$dose,0)))+1,by=2)
  cls = unique(df.ee$cluster)
  df.ext = do.call('rbind',lapply(1:length(cls),function(clid){
    temp <-   df.pq_range[df.pq_range$cluster==cls[clid],]
    agg <- aggregate(temp$dose,by=list(expert=temp$expert),'max')
    lower_cdf <- aggregate(temp$lower_cdf,by=list(expert=temp$expert),'max')$x
    upper_cdf <- aggregate(temp$upper_cdf,by=list(expert=temp$expert),'max')$x
    data.frame(agg,lower_cdf,upper_cdf,cluster=cls[clid])
  }))
  df.ext$xend = max(xa)
  
  
  df.ee %>%
    ggplot(aes(x=dose,y=100*upper_cdf,col=cluster,group = expert))+ #,linetype=expert))+
    geom_line() +
    geom_line(aes(x=dose,y=100*lower_cdf,col=cluster))+
    #geom_segment(data=df.ext,aes(x=x,xend=xend,y=cdf_upper,yend=cdf_upper))+
    #geom_segment(data=df.ext,aes(x=x,xend=xend,y=cdf_lower,yend=cdf_lower))+
    #geom_line(aes(x=log10d,y=cdf_hist_lower),col='black',alpha=0.5)+
    # geom_line(aes(x=log10d,y=cdf_hist_upper),col='black',alpha=0.5)+
    geom_line(data=df_env2,aes(x=dose,y=100*upper_cdf),col='black')+
    geom_line(data=df_env2,aes(x=dose,y=100*lower_cdf),col='black')+
    facet_wrap(~cluster, scales = 'free') +
    theme_bw()  +
    ylab('Cumulative probability (%)') +
    ylim(0,100) +
    #ggtitle("Uncertainty distributions",subtitle = "with envelope across individual experts") +
    scale_x_continuous(name="Estimated lowest BMD in the cluster \n (HED, ng BPA/kg bw per day)", 
                       breaks=xa, labels=10^xa,limits = NULL)#range(xa)) #+
  
}

plot_experts_with_env_onecluster <- function(df.pq_range,df_env,clid){
  df.pq_range$expert = paste('expert',LETTERS[df.pq_range$expert])
  df_env2 <- df_env
  df_env2$expert = "envelope"
  df.ee <- rbind(df.pq_range[,colnames(df_env2)],df_env2)
  
  xa = c(-2,0,2,4)
  cls = unique(df.ee$cluster)
  #df.ext = do.call('rbind',lapply(1:length(cls),function(clid){
  #  temp <-   df.pq_range[df.pq_range$cluster==cls[clid],]
  #  agg <- aggregate(temp$dose,by=list(expert=temp$expert),'max')
  #  lower_cdf <- aggregate(temp$lower_cdf,by=list(expert=temp$expert),'max')$x
  #  upper_cdf <- aggregate(temp$upper_cdf,by=list(expert=temp$expert),'max')$x
  #  data.frame(agg,lower_cdf,upper_cdf,cluster=cls[clid])
  #}))
  #df.ext$xend = max(xa)
  
  df_env2_clid <- df_env2 %>%
    filter(cluster == cls[clid]) %>%
    mutate(what = paste(cls[clid],"with envelope and average"))
  
  df.ee_clid <-  df.ee %>%
    filter(cluster == cls[clid])
  lp_lower_cdf <- aggregate(df.ee_clid$lower_cdf,by=list(dose=df.ee_clid$dose),'mean')$x
  lp_upper_cdf <- aggregate(df.ee_clid$upper_cdf,by=list(dose=df.ee_clid$dose),'mean')$x
  
  df.ee_clid <- rbind(df.ee_clid,df.ee_clid)
  df.ee_clid$what  <- rep(c(cls[clid],"with envelope and average"),each=nrow(df.ee_clid)/2) 
  removerow <- df.ee_clid$what == cls[clid] & df.ee_clid$expert == "envelope"
  df.ee_clid <- df.ee_clid[!removerow,]
  
  df.lp_clid  <- data.frame(lower_cdf=lp_lower_cdf,upper_cdf=lp_upper_cdf,dose=df.ee_clid$dose[df.ee_clid$expert== "envelope"],
             cluster = df.ee_clid$cluster[1], type = df.ee_clid$type[1],expert = 'average',what = "with envelope and average")
  
  df_env2_clid$what = "with envelope and average"
  df.ee_clid %>%
    ggplot(aes(x=dose,y=100*upper_cdf,col=cluster,group = expert))+#linetype=expert))+
    geom_line() +
    geom_line(aes(x=dose,y=100*lower_cdf,col=cluster), linetype = "dashed") +
    geom_line(data=df.lp_clid,aes(x=dose,y=100*lower_cdf,col=cluster), col='blue',size = 1) +
    geom_line(data=df.lp_clid,aes(x=dose,y=100*upper_cdf,col=cluster), col='blue',size = 1) +
    geom_line(data=df_env2_clid,aes(x=dose,y=100*upper_cdf),col='black',alpha = 0.5,size = 1)+
    geom_line(data=df_env2_clid,aes(x=dose,y=100*lower_cdf),col='black',alpha = 0.5,size = 1)+
    theme_bw()  +
    ylab('Cumulative probability (%)') +
    ylim(0,100) +
    facet_wrap(~what) +
    #ggtitle("Uncertainty distributions") +
    scale_x_continuous(name="Estimated lowest BMD in the cluster \n (HED, ng BPA/kg bw per day)", 
                       breaks=xa, labels=10^xa)+#,limits = range(xa)) +
    guides(color="none") #+ 
   # theme(legend.position="bottom")
 # ggsave(filename = paste("env_cluster",cls[clid],origALI,".jpeg"),width = 10, height = 6)
  #ggsave(filename = paste("env_cluster",cls[clid],origALI,".pdf"),width = 10, height = 6)
  
  
}

plot_experts_with_env_onecluster_twopanes <- function(df.pq_range,df_env,clid){
  df.pq_range$expert = paste('expert',LETTERS[df.pq_range$expert])
  df_env2 <- df_env
  df_env2$expert = "envelope"
  df.ee <- rbind(df.pq_range[,colnames(df_env2)],df_env2)
  
  xa = c(-2,0,2)
  #xa = seq(min(unique(round(df.ee$dose,0))),
  #         max(unique(round(df.ee$dose,0)))+1,by=2)
  cls = unique(df.ee$cluster)
  df.ext = do.call('rbind',lapply(1:length(cls),function(clid){
    temp <-   df.pq_range[df.pq_range$cluster==cls[clid],]
    agg <- aggregate(temp$dose,by=list(expert=temp$expert),'max')
    lower_cdf <- aggregate(temp$lower_cdf,by=list(expert=temp$expert),'max')$x
    upper_cdf <- aggregate(temp$upper_cdf,by=list(expert=temp$expert),'max')$x
    data.frame(agg,lower_cdf,upper_cdf,cluster=cls[clid])
  }))
  df.ext$xend = max(xa)
  
  df_env2_clid <- df_env2 %>%
    filter(cluster == cls[clid]) %>%
    mutate(what = paste(cls[clid],"with envelope"))
  
  df.ee_clid <-  df.ee %>%
    filter(cluster == cls[clid])
  df.ee_clid <- rbind(df.ee_clid,df.ee_clid)
  df.ee_clid$what  <- rep(c(cls[clid],"with envelope"),each=nrow(df.ee_clid)/2) 
  removerow <- df.ee_clid$what == cls[clid] & df.ee_clid$expert == "envelope"
  df.ee_clid <- df.ee_clid[!removerow,]
  
  df_env2_clid$what = "with envelope"
  df.ee_clid %>%
    ggplot(aes(x=dose,y=100*upper_cdf,col=cluster,group = expert))+#linetype=expert))+
    geom_line() +
    geom_line(aes(x=dose,y=100*lower_cdf,col=cluster), linetype = "dashed") +
    geom_line(data=df_env2_clid,aes(x=dose,y=100*upper_cdf),col='black',alpha = 0.5)+
    geom_line(data=df_env2_clid,aes(x=dose,y=100*lower_cdf),col='black',alpha = 0.5)+
    theme_bw()  +
    ylab('Cumulative probability (%)') +
    ylim(0,100) +
    facet_wrap(~what) +
    #ggtitle("Uncertainty distributions") +
    scale_x_continuous(name="Estimated lowest BMD in the cluster \n (HED, ng BPA/kg bw per day)", 
                       breaks=xa, labels=10^xa)+#,limits = range(xa)) +
    guides(color="none") #+ 
  # theme(legend.position="bottom")
  # ggsave(filename = paste("env_cluster",cls[clid],origALI,".jpeg"),width = 10, height = 6)
  #ggsave(filename = paste("env_cluster",cls[clid],origALI,".pdf"),width = 10, height = 6)
  
  
}

get_linear_pool_WRONG <- function(df.pq_range){
  df.pq_range$expert = paste('expert',LETTERS[df.pq_range$expert])
  
  cls = unique(df.pq_range$cluster)
  df.ee = do.call('rbind',lapply(1:length(cls),function(clid){
    temp <-   df.pq_range[df.pq_range$cluster==cls[clid],]
    agg <- aggregate(temp$dose,by=list(expert=temp$expert),'max')
    lower_cdf <- aggregate(temp$lower_cdf,by=list(expert=temp$expert),'max')$x
    upper_cdf <- aggregate(temp$upper_cdf,by=list(expert=temp$expert),'max')$x
    data.frame(agg,lower_cdf,upper_cdf,cluster=cls[clid])
  }))
  
  lower_agg = aggregate(df.ee$lower_cdf,by = list(dose=df.ee$dose,cluster=df.ee$cluster,
                                                  type=df.ee$type),FUN = 'mean')
  colnames(lower_agg)[4] <- "lower_cdf"
  upper_agg = aggregate(df.ee$upper_cdf,by = list(dose=df.ee$dose,cluster=df.ee$cluster,
                                                  type=df.ee$type),FUN = 'mean')
  df.lp <- data.frame(lower_agg,upper_cdf = upper_agg$x,
                      mid_cdf = (upper_agg$x+lower_agg$lower_cdf)/2)
  
  df.lp %>%
    ggplot(aes(x=dose,y=100*upper_cdf,col=cluster))+
    geom_line() +
    geom_line(aes(x=dose,y=100*lower_cdf,col=cluster))+
    #geom_line(aes(x=dose,y=mid_cdf),linetype = "dashed",col='black')+
    #geom_segment(data=df.ext,aes(x=x,xend=xend,y=cdf_upper,yend=cdf_upper))+
    #geom_segment(data=df.ext,aes(x=x,xend=xend,y=cdf_lower,yend=cdf_lower))+
    #geom_line(aes(x=log10d,y=cdf_hist_lower),col='black',alpha=0.5)+
    # geom_line(aes(x=log10d,y=cdf_hist_upper),col='black',alpha=0.5)+
    geom_line(data=df_env2,aes(x=dose,y=100*upper_cdf),col='black')+
    geom_line(data=df_env2,aes(x=dose,y=100*lower_cdf),col='black')+
    facet_wrap(~cluster, scales = 'free') +
    theme_bw()  +
    ylab('Cumulative probability (%)') +
    ylim(0,100) +
    #ggtitle("Uncertainty distributions",subtitle = "with envelope and linear pool (lower, upper, mid)") +
    scale_x_continuous(name="Estimated lowest BMD in the cluster \n (HED, ng BPA/kg bw per day)", 
                       breaks=xa, labels=10^xa,limits = range(xa)) #+
  
  
}

plot_experts_with_env_linear_pool <- function(df.pq_range,df_env){
  df.pq_range$expert = paste('expert',LETTERS[df.pq_range$expert])
  df_env2 <- df_env
  df_env2$expert = "envelope"
  df.ee <- rbind(df.pq_range[,colnames(df_env2)],df_env2)
  
  xa = seq(min(unique(round(df.ee$dose,0))),
           max(unique(round(df.ee$dose,0)))+1,by=2)
  cls = unique(df.ee$cluster)
  df.ext = do.call('rbind',lapply(1:length(cls),function(clid){
    temp <-   df.pq_range[df.pq_range$cluster==cls[clid],]
    agg <- aggregate(temp$dose,by=list(expert=temp$expert),'max')
    lower_cdf <- aggregate(temp$lower_cdf,by=list(expert=temp$expert),'max')$x
    upper_cdf <- aggregate(temp$upper_cdf,by=list(expert=temp$expert),'max')$x
    data.frame(agg,lower_cdf,upper_cdf,cluster=cls[clid])
  }))
  df.ext$xend = max(xa)
  
  lower_agg = aggregate(df.ee$lower_cdf,by = list(dose=df.ee$dose,cluster=df.ee$cluster,
                            type=df.ee$type),FUN = 'mean')
  colnames(lower_agg)[4] <- "lower_cdf"
  upper_agg = aggregate(df.ee$upper_cdf,by = list(dose=df.ee$dose,cluster=df.ee$cluster,
                                                  type=df.ee$type),FUN = 'mean')
  df.lp <- data.frame(lower_agg,upper_cdf = upper_agg$x,
                      mid_cdf = (upper_agg$x+lower_agg$lower_cdf)/2)
  
  df.lp %>%
    ggplot(aes(x=dose,y=100*upper_cdf,col=cluster))+
    geom_line() +
    geom_line(aes(x=dose,y=100*lower_cdf,col=cluster))+
    #geom_line(aes(x=dose,y=mid_cdf),linetype = "dashed",col='black')+
    #geom_segment(data=df.ext,aes(x=x,xend=xend,y=cdf_upper,yend=cdf_upper))+
    #geom_segment(data=df.ext,aes(x=x,xend=xend,y=cdf_lower,yend=cdf_lower))+
    #geom_line(aes(x=log10d,y=cdf_hist_lower),col='black',alpha=0.5)+
    # geom_line(aes(x=log10d,y=cdf_hist_upper),col='black',alpha=0.5)+
    geom_line(data=df_env2,aes(x=dose,y=100*upper_cdf),col='black')+
    geom_line(data=df_env2,aes(x=dose,y=100*lower_cdf),col='black')+
    facet_wrap(~cluster, scales = 'free') +
    theme_bw()  +
    ylab('Cumulative probability (%)') +
    ylim(0,100) +
    #ggtitle("Uncertainty distributions",subtitle = "with envelope and linear pool (lower, upper, mid)") +
    scale_x_continuous(name="Estimated lowest BMD in the cluster \n (HED, ng BPA/kg bw per day)", 
                       breaks=xa, labels=10^xa,limits = range(xa)) #+
  
  
}

derive_prob <- function(rowids,loo = 'none',dose){
  #extract expert and cluster 
  what_carcino <- info_carcino[as.numeric(design_c[rowids[1],]),]
  what_carcino = what_carcino[!(what_carcino$cluster %in% loo),]
  
  temp_carcino <- lapply(1:length(dose),function(di){
    do.call('rbind', lapply(1:nrow(what_carcino),function(clid){
      1-pfunctions_carcino[[what_carcino[clid,'i']]][[what_carcino[clid,'j']]]$pbmd(dose[di])  
    }))
  })
  
  what_immunotox <- info_immunotox[as.numeric(design_i[rowids[2],]),]
  what_immunotox = what_immunotox[!(what_immunotox$cluster %in% loo),]
  
  temp_immunotox <- lapply(1:length(dose),function(di){
    do.call('rbind', lapply(1:nrow(what_immunotox),function(clid){
      1-pfunctions_immunotox[[what_immunotox[clid,'i']]][[what_immunotox[clid,'j']]]$pbmd(dose[di])  
    }))
  })
  
  what_metabolic <- info_metabolic[as.numeric(design_m[rowids[3],]),]
  what_metabolic = what_metabolic[!(what_metabolic$cluster %in% loo),]
  
  temp_metabolic <- lapply(1:length(dose),function(di){
    do.call('rbind', lapply(1:nrow(what_metabolic),function(clid){
      1-pfunctions_metabolic[[what_metabolic[clid,'i']]][[what_metabolic[clid,'j']]]$pbmd(dose[di])  
    }))
  })  
  
  what_neurotox <- info_neurotox[as.numeric(design_n[rowids[4],]),]
  what_neurotox = what_neurotox[!(what_neurotox$cluster %in% loo),]
  
  temp_neurotox <- lapply(1:length(dose),function(di){
    do.call('rbind', lapply(1:nrow(what_neurotox),function(clid){
      1-pfunctions_neurotox[[what_neurotox[clid,'i']]][[what_neurotox[clid,'j']]]$pbmd(dose[di])  
    }))
  })  
  
  what_repro <- info_repro[as.numeric(design_r[rowids[5],]),]
  what_repro <- what_repro[!(what_repro$cluster %in% loo),]
  
  temp_repro <- lapply(1:length(dose),function(di){
    do.call('rbind', lapply(1:nrow(what_repro),function(clid){
      1-pfunctions_repro[[what_repro[clid,'i']]][[what_repro[clid,'j']]]$pbmd(dose[di])  
    }))
  })  
  
  #  derive the probability of at least one effect
  
  prob_atleastone <-  do.call('rbind',lapply(1:length(dose),function(di){
    return(data.frame(lower = 1 - prod(temp_carcino[[di]][,1])*
                        prod(temp_metabolic[[di]][,1])*
                        prod(temp_immunotox[[di]][,1])*
                        prod(temp_neurotox[[di]][,1])*
                        prod(temp_repro[[di]][,1]),
                      upper = 1 -  prod(temp_carcino[[di]][,2])*
                        prod(temp_metabolic[[di]][,2])*
                        prod(temp_immunotox[[di]][,2])*
                        prod(temp_neurotox[[di]][,2])*
                        prod(temp_repro[[di]][,2]), dose= dose[di]))
  }))
  return(prob_atleastone)
}

get_env <- function(info,dose_curve,pfunc,expert = 'all'){
  cls = unique((info$cluster))
  do.call('rbind',lapply(1:length(cls),function(k){
    what <- info[info$cluster==cls[k],]
    if(cls[k]=="Allergic lung inflammation" & expert != 'all'){
      what <- what[what$expert==expert,]
    } 
    temp_cdf_env <- do.call('rbind',lapply(1:length(dose_curve),function(di){
      temp_lowup <- do.call('rbind', lapply(1:nrow(what),function(clid){
        pfunc[[what[clid,'i']]][[what[clid,'j']]]$pbmd(dose_curve[di])  
      }))
      c(min(temp_lowup[,1]),max(temp_lowup[,2]))
    }))
    data.frame(lower_cdf = temp_cdf_env[,1],upper_cdf = temp_cdf_env[,2], 
               dose = dose_curve, cluster = rep(what$cluster[1],length(dose_curve)))
  }))
  }

get_env_list <- function(info,dose_curve,pfunc,expert = 'all'){
  cls = unique((info$cluster))
  lapply(1:length(cls),function(k){
    what <- info[info$cluster==cls[k],]
    if(cls[k]=="Allergic lung inflammation" & expert != 'all'){
      what <- what[what$expert==expert,]
    } 
    temp_cdf_env <- do.call('rbind',lapply(1:length(dose_curve),function(di){
      temp_lowup <- do.call('rbind', lapply(1:nrow(what),function(clid){
        pfunc[[what[clid,'i']]][[what[clid,'j']]]$pbmd(dose_curve[di])  
      }))
      c(min(temp_lowup[,1]),max(temp_lowup[,2]))
    }))
    data.frame(lower_cdf = temp_cdf_env[,1],upper_cdf = temp_cdf_env[,2], 
               dose = dose_curve, cluster = rep(what$cluster[1],length(dose_curve)))
  })
}

approx_jointprobability <- function(target_d,rho,i,j,clid_i,clid_j,niter, env_hocs){

  # specify a correlation
  #rho=0.9
  # choose a dose
  #d = -1
  # make sure the range over which optimisation is done covers the target dose 
  maxd = max(list_env_hocs[[i]][[clid_i]]$dose,list_env_hocs[[j]][[clid_j]]$dose,target_d + 1)
  mind = min(list_env_hocs[[i]][[clid_i]]$dose,list_env_hocs[[j]][[clid_j]]$dose,target_d - 1)
  
  # sample from bivariate normal 
  Sigma <- matrix(c(1,rho,rho,1),2,2)
  sam <- MASS::mvrnorm(niter,c(0,0),Sigma)
  # translate to lowest BMD (lowest cdf) for cluster i
  invx <- approxfun(c(0,list_env_hocs[[i]][[clid_i]]$lower_cdf,1),c(mind,list_env_hocs[[i]][[clid_i]]$dose,maxd),ties='mean')
  transx <- invx(pnorm(sam[,1]))
  transx[pnorm(sam[,1]) > max(list_env_hocs[[i]][[clid_i]]$lower_cdf)] = target_d + 1
  invy <- approxfun(c(0,list_env_hocs[[j]][[clid_j]]$lower_cdf,1),c(mind,list_env_hocs[[j]][[clid_j]]$dose,maxd),ties='mean')
  transy <- invy(pnorm(sam[,2]))
  transy[pnorm(sam[,2]) > max(list_env_hocs[[j]][[clid_j]]$lower_cdf)] = target_d + 1
  probisd = mean(transx>target_d & transy>target_d)
  
  # translate to lowest BMD (upper cdf) for cluster i
  invx <- approxfun(c(0,list_env_hocs[[i]][[clid_i]]$upper_cdf,1),c(mind,list_env_hocs[[i]][[clid_i]]$dose,maxd),ties='mean')
  transx <- invx(pnorm(sam[,1]))
  transx[pnorm(sam[,1]) > max(list_env_hocs[[i]][[clid_i]]$upper_cdf)] = target_d + 1
  invy <- approxfun(c(0,list_env_hocs[[j]][[clid_j]]$upper_cdf,1),c(mind,list_env_hocs[[j]][[clid_j]]$dose,maxd),ties='mean')
  transy <- invy(pnorm(sam[,2]))
  transy[pnorm(sam[,2]) > max(list_env_hocs[[j]][[clid_j]]$upper_cdf)] = target_d + 1
  probisd = c(probisd,mean(transx>target_d & transy>target_d))
  
  return(list(probisd = probisd))
}

approx_jointprobability_linear_pool <- function(target_d,rho,pfunction_1,pfunction_2,niter){
  
  # specify a correlation
  #rho=0.9
  # choose a dose
  
  # sample from bivariate normal 
  Sigma <- matrix(c(1,rho,rho,1),2,2)
  sam <- MASS::mvrnorm(niter,c(0,0),Sigma)
  # translate to lowest BMD (lowest cdf) for cluster i
  invx <- approxfun(c(0,pfunction_1$lower_cdf,1),c(-6,pfunction_1$dose,6),ties='mean')
  transx <- invx(pnorm(sam[,1]))
  transx[pnorm(sam[,1]) > max(pfunction_1$lower_cdf)] = target_d + 1
  invy <- approxfun(c(0,pfunction_2$lower_cdf,1),c(-6,pfunction_2$dose,6),ties='mean')
  transy <- invy(pnorm(sam[,2]))
  transy[pnorm(sam[,2]) > max(pfunction_2$lower_cdf)] = target_d + 1
  probisd = mean(transx>target_d & transy>target_d)
  
  # translate to lowest BMD (upper cdf) for cluster i
  invx <- approxfun(c(0,pfunction_1$upper_cdf,1),c(-6,pfunction_1$dose,6),ties='mean')
  transx <- invx(pnorm(sam[,1]))
  transx[pnorm(sam[,1]) > max(pfunction_1$upper_cdf)] = target_d + 1
  invy <- approxfun(c(0,pfunction_2$upper_cdf,1),c(-6,pfunction_2$dose,6),ties='mean')
  transy <- invy(pnorm(sam[,2]))
  transy[pnorm(sam[,2]) > max(pfunction_2$upper_cdf)] = target_d + 1
  probisd = c(probisd,mean(transx>target_d & transy>target_d))
  
  return(list(probisd = probisd))
}


get_linear_pool <- function(info,dose_curve,pfunc,expert = 'all'){
  cls = unique((info$cluster))
  do.call('rbind',lapply(1:length(cls),function(k){
    what <- info[info$cluster==cls[k],]
    if(cls[k]=="Allergic lung inflammation" & expert != 'all'){
      what <- what[what$expert==expert,]
    }
    if(cls[k]=="Cellular immunity " & expert != 'all'){
      what <- what[what$expert==expert,]
    }
    temp_lowup <- do.call('rbind', lapply(1:nrow(what),function(clid){
        pfunc[[what[clid,'i']]][[what[clid,'j']]]$pbmd(dose_curve)  
      }))
        data.frame(lower_cdf = mean(temp_lowup[,1]),
                 upper_cdf = mean(temp_lowup[,2]),
                 mid_cdf = mean(temp_lowup),
                 dose = dose_curve, 
                 cluster = rep(what$cluster[1],length(dose_curve)))
  }))
}

get_linear_pool_list <- function(info,dose_curve,pfunc,expert = 'all'){
  cls = unique((info$cluster))
  lapply(1:length(cls),function(k){
    what <- info[info$cluster==cls[k],]
    if(cls[k]=="Allergic lung inflammation" & expert != 'all'){
      what <- what[what$expert==expert,]
    } 
    temp_lowup <- do.call('rbind', lapply(1:nrow(what),function(clid){
      pfunc[[what[clid,'i']]][[what[clid,'j']]]$pbmd(dose_curve)  
    }))
    data.frame(lower_cdf = mean(temp_lowup[,1]),
               upper_cdf = mean(temp_lowup[,2]),
               mid_cdf = mean(temp_lowup),
               dose = dose_curve, 
               cluster = rep(what$cluster[1],length(dose_curve)))
  })
}

get_impagg <- function(info,dose_curve,pfunc){
  cls = unique((info$cluster))
  do.call('rbind',lapply(1:length(cls),function(k){
    what <- info[info$cluster==cls[k],]
      temp_lowup <- do.call('rbind', lapply(1:nrow(what),function(eid){
        pfunc[[what[eid,'i']]][[what[eid,'j']]]$pbmd(dose_curve)  
      }))
      #objective <- function(po){
      #sum(temp_lowup[,1]^2*(1-po)) + sum((1-temp_lowup[,2])^2*po)
      #}
      #optimize(objective, interval = range(temp_lowup))$minimum
      ppo = seq(min(temp_lowup),max(temp_lowup),length.out=10^3)
      ypo = unlist(lapply(ppo,function(po){
        if(TRUE){
          ## add brier type of measure here
          mean(unlist(
            lapply(1:nrow(temp_lowup),function(w){
              if(po<temp_lowup[w,1]){
                score = (po/temp_lowup[w,1])^2
              }else if(po>temp_lowup[w,2]){
                score = ((1-po)/(1-temp_lowup[w,2]))^2
              }else{
                score = (1 - (po<temp_lowup[w,1] | po>temp_lowup[w,2]))
              }
              return(score)
            })))
        }else if(FALSE){
          ## linear
        mean(unlist(
        #prod(unlist(
            lapply(1:nrow(temp_lowup),function(w){
        if(po<temp_lowup[w,1]){
          score = po/temp_lowup[w,1]
        }else if(po>temp_lowup[w,2]){
          score = (1-po)/(1-temp_lowup[w,2])
          }else{
          score = (1 - (po<temp_lowup[w,1] | po>temp_lowup[w,2]))
          }
        return(score)
        })))#^(1/nrow(temp_lowup))
        }else{
        # zero outside range
        mean(unlist(
          lapply(1:nrow(temp_lowup),function(w){
              score = (1 - (po<temp_lowup[w,1] | po>temp_lowup[w,2]))
            return(score)
          })))
          }
        
        }))
      #plot(ppo,ypo,type='l')
      data.frame(lower_cdf = min(ppo[ypo == max(ypo)]),
           upper_cdf=max(ppo[ypo == max(ypo)]),conf = max(ypo),dose = dose_curve,
               cluster = rep(what$cluster[1],length(dose_curve)))
  }))
}
