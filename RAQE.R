#RAQUE function

RAQUE <- function(x, sampleID = NULL, p, tail.percentage = 25, plot.empirical = TRUE, use.weights = FALSE, normalize.values = TRUE, display.results = TRUE, save.AEDFvalues = FALSE){
  #read sample statistics
  #if sampleID is not defined, create the corresponding trivial vector with 1 class
  if(is.null(sampleID) == TRUE){
    sampleID = rep(1,length(x))
  }
  
  # determine which tail to use
  upper.tail = TRUE
  if(p < 0.5) upper.tail = FALSE
  
  # sample statistics
  xbar = tapply(x, sampleID, mean)
  s = tapply(x, sampleID, sd)
  n = tapply(x, sampleID, length)
  ksamples = length(n) #number of samples
  N = sum(n)
  IDname = names(xbar)
  
  # standardize samples
  Xbar = rep(xbar,n)
  S = rep(s,n)
  z = (x - Xbar)/S
  
  #Expand empirical distribution
  o = order(z)
  z = z[o]
  
  p.up = rank(z)/N
  p.down = (rank(z)-1) / N
  p.mid = (p.up + p.down) / 2
  
  z.up = z[2:N]
  z.down = z[1:(N-1)]
  z.mid = (z.up + z.down) / 2
  
  Z = c(z,z.mid)
  P = c(p.mid, p.up[1:(N-1)])
  
  O = order(Z)
  Z = Z[O]
  P = P[O]
  
  #Define tail to fit
  if(upper.tail == TRUE){
    tail.limit = quantile(z, probs = 1 - tail.percentage/100)
    Z.tail = Z[Z >= tail.limit]
    P.tail = P[Z >= tail.limit]
    datos = data.frame(Z = Z.tail, P = P.tail) 
  }else{
    tail.limit = quantile(z, probs = tail.percentage/100)
    Z.tail = Z[Z <= tail.limit]
    P.tail = P[Z <= tail.limit]
    datos = data.frame(Z = Z.tail, P = P.tail)
  }
  
  
  if(use.weights != FALSE){
    W = 1 / (P.tail*(1-P.tail)) 
  } else{
    W = 1
  }
  
  q = rep(NA,5)
  SSE = rep(NA,5)
  fitted.function = c("Gumbel.max", "Gumbel.min", "Exponential", "Weibull", "Logit")
  #  Gumbel type 1 (max extreme value). Goes slower than exponential
  fitGumbel.max = optim(par = c(0, 1), SSEgumbel.max, data = datos, W = W)
  q[1] = gumbel.max.inv(p = p, u = fitGumbel.max$par[1], s = fitGumbel.max$par[2])
  SSE[1] = fitGumbel.max$value
  
  #  Gumbel type 2? (min extreme value). Goes faster than exponential
  fitGumbel.min = optim(par = c(0, 1), SSEgumbel.min, data = datos, W = W)
  q[2] = gumbel.min.inv(p = p, u = fitGumbel.min$par[1], s = fitGumbel.min$par[2])
  SSE[2] = fitGumbel.min$value
  
  #  Exponential. Goes equal to the exponential! :)
  fitExponential = optim(par = c(0,1), SSEexponential, data = datos, W = W)
  q[3] = exponential.inv(p = p, u = fitExponential$par[1], theta = fitExponential$par[2])
  SSE[3] = fitExponential$value
  
  #  Weibull. Goes faster, equal or slower than exponential
  fitWeibull = optim(par = c(min(Z),1,1), SSEweibull, data = datos, W = W)
  q[4] = weibull.inv(p = p, u = fitWeibull$par[1], a = fitWeibull$par[2], b = fitWeibull$par[3])
  SSE[4] = fitWeibull$value
  
  #  Logit. Similar to normal
  fitLogit = optim(par = c(0, (sd(Z)*sqrt(3)/pi)), SSElogit, data = datos, W = W)
  q[5] = logit.inv(p = p, u = fitLogit$par[1], s = fitLogit$par[2])
  SSE[5] = fitLogit$value
  
  SSEm = matrix(SSE, nrow=1,ncol=5)
  colnames(SSEm) <- c("Gumbel.max","Gumbel.min","Exponential","Weibull", "Logit")
  rownames(SSEm) <- c("SSEw")
  
  o1 = order(SSE)
  zq = q[o1[1]] #estimated quantile in standard form
  
  Xq = xbar + s*zq #estimated quantile
  
  best.fit = fitted.function[o1[1]] # best fit function
  wSSE.best = SSE[o1[1]]
  
  #Gathering the first results
  results = list(requested.p = p, tail.percentage = tail.percentage, tail.limit = tail.limit, Xq = Xq, Zq = zq, best.fit = best.fit, wSSE = SSEm, wSSE.best = wSSE.best)
  
  if(display.results == TRUE){
    # String to use when printing results
    p.separator ="............................................................................"
    p.title = "EXTREME VALUE ESTIMATION WITH RAQE."
    p.input = "Inputs:"
    p.obs = "observations in"
    p.sam = "sample(s)."
    p.request = "Requested quantile Xp where p ="
    p.samtitle = "Sample(s) Information"
    p.sample = "Sample"
    p.restitle = "Estimation Results"
    p.curvestring = "Curve Ritting Results"
    p.IDstring = "Sample ID name:"
    p.fittedstring = "Best fit:"
    p.otherstring = "All functions and their weighed sum of squares:"
    p.taildescription = "Tail proportion used:"
    p.Xpdescription = "and P(X <= Xp) = p"
    p.warning =
      "WARNING: By combining samples to improve estimation you are assuming samples
    are shape-homogeneous (same shape but different mean and variance). If this
    is not the case, run samples individually."
    p.warning2 =
      "Note: Request to display Augmented Empirical Distribution Function (AEDF)
    using orignal scale is invalid. When two or more samples are available,
    combined values used to define AED can only be displayed in standard form.
    Set 'normalize.values = TRUE' to avoid this message."
    
    # Print results starts here
    cat(p.separator,"\n")
    
    cat(p.title,"\n", p.input, N, p.obs, ksamples, p.sam, "\n")
    cat("",paste0(p.IDstring),IDname,"\n")
    
    cat(p.separator,"\n")
    
    cat(p.samtitle,"\n")
    for(l in 1:ksamples){
      cat("",p.sample, paste0(IDname[l], ":"), paste0("n",l), "=", paste0(n[l], ","), paste0("xbar",l), "=", paste0(round(xbar[l], digits = 3), ","), paste0("s",l), "=", round(s[l], digits = 3), "\n")
    }
    
    cat(p.separator,"\n")
    
    cat(p.curvestring,"\n")
    cat("", p.fittedstring, results$fitted.function, "\n")
    cat("", p.otherstring,"\n\n")
    print(round(results$wSSE, digits = 5))
    cat("\n")
    
    cat(p.separator,"\n")
    
    cat(p.restitle,"\n")
    cat("", p.request, p, p.Xpdescription, "\n")
    cat("", p.taildescription, paste0(tail.percentage,"%"), "\n")
    for(l in 1:ksamples){
      cat("",p.sample, IDname[l], "\n")
      cat("     ","Xp =", round(Xq[l], digits = 3), "\n")
    }
    cat(p.separator,"\n")
    
    if(ksamples > 1){
      cat(p.warning,"\n")
      
      cat(p.separator,"\n") 
    }
    if(ksamples > 1 && normalize.values != TRUE){
      cat(p.warning2,"\n")
      
      cat(p.separator,"\n") 
    }
    #Print results end here 
  }
  
  # Plot
  
  # Plot text adjustements
  if(plot.empirical == TRUE)
  {
    main.title = "Augmented Empirical Distribution Function"
    sub.title = "Normalized Scale and Location"
    xlab.name = "Augmented Z values"
    ylab.name = "Probability"
    
    # Original values
    Zmu = 0
    Zs = 1
    if(ksamples == 1 && normalize.values != TRUE){
      Zmu = xbar
      Zs = s
      Z = as.vector(Zmu) + as.vector(Zs)*Z
      zq = Zmu + Zs*zq
      sub.title = "Original Scale and Location"
      xlab.name = "Augmented Observations"
    }
    if(ksamples > 1){
      xlab.name = "Augmented Z values of combined samples"
    } 
  }
  
  if(plot.empirical == TRUE & fitted.function[o1[1]] == "Weibull"){
    min.Xplot = min(Z,zq)
    max.Xplot = max(Z,zq)
    clases = (max.Xplot - min.Xplot)/50
    xline = seq(from = min.Xplot, to = max.Xplot, by = clases)
    plot(Z,P,
         pch = 20,
         cex = 0.5,
         xlim = c(min.Xplot,max.Xplot),
         main = main.title,
         sub = sub.title,
         xlab = xlab.name,
         ylab = ylab.name)
    points(zq, p, pch = 1, col = "blue")
    lines(xline, weibull(x = (xline - as.vector(Zmu))/as.vector(Zs), u = fitWeibull$par[1], a = fitWeibull$par[2], b = fitWeibull$par[3]), type = "l", col = "red")
    lines(xline, (xline*0 + p), type = "l", col = "blue")
    abline(v = zq, col = "blue")
    abline(v = tail.limit, col = "gray", lty = 2)
  }
  
  # Actual plotting
  if(plot.empirical == TRUE & fitted.function[o1[1]] == "Gumbel.max"){
    min.Xplot = min(Z,zq)
    max.Xplot = max(Z,zq)
    clases = (max.Xplot - min.Xplot)/50
    xline = seq(from = min.Xplot, to = max.Xplot, by = clases)
    plot(Z,P,
         pch = 20,
         cex = 0.5,
         xlim = c(min.Xplot,max.Xplot),
         main = main.title,
         sub = sub.title,
         xlab = xlab.name,
         ylab = ylab.name)
    points(zq, p, pch = 1, col = "blue")
    lines(xline, gumbel.max(x = (xline - as.vector(Zmu))/as.vector(Zs), u = fitGumbel.max$par[1], s = fitGumbel.max$par[2]), type = "l", col = "red")
    lines(xline, (xline*0 + p), type = "l", col = "blue")
    abline(v = zq, col = "blue")
    abline(v = tail.limit, col = "gray", lty = 2)
  }
  
  if(plot.empirical == TRUE & fitted.function[o1[1]] == "Gumbel.min"){
    min.Xplot = min(Z,zq)
    max.Xplot = max(Z,zq)
    clases = (max.Xplot - min.Xplot)/50
    xline = seq(from = min.Xplot, to = max.Xplot, by = clases)
    plot(Z,P,
         pch = 20,
         cex = 0.5,
         xlim = c(min.Xplot,max.Xplot),
         main = main.title,
         sub = sub.title,
         xlab = xlab.name,
         ylab = ylab.name)
    points(zq, p, pch = 1, col = "blue")
    lines(xline, gumbel.min(x = (xline - as.vector(Zmu))/as.vector(Zs), u = fitGumbel.min$par[1], s = fitGumbel.min$par[2]), type = "l", col = "red")
    lines(xline, (xline*0 + p), type = "l", col = "blue")
    abline(v = zq, col = "blue")
    abline(v = tail.limit, col = "gray", lty = 2)
  }
  
  if(plot.empirical == TRUE & fitted.function[o1[1]] == "Exponential"){
    min.Xplot = min(Z,zq)
    max.Xplot = max(Z,zq)
    clases = (max.Xplot - min.Xplot)/50
    xline = seq(from = min.Xplot, to = max.Xplot, by = clases)
    plot(Z,P,
         pch = 20,
         cex = 0.5,
         xlim = c(min.Xplot,max.Xplot),
         main = main.title,
         sub = sub.title,
         xlab = xlab.name,
         ylab = ylab.name)
    points(zq, p, pch = 1, col = "blue")
    lines(xline, exponential(x = (xline - as.vector(Zmu))/as.vector(Zs), u = fitExponential$par[1], theta = fitExponential$par[2]), type = "l", col = "red")
    lines(xline, (xline*0 + p), type = "l", col = "blue")
    abline(v = zq, col = "blue")
    abline(v = tail.limit, col = "gray", lty = 2)
  }
  
  if(plot.empirical == TRUE & fitted.function[o1[1]] == "Logit"){
    min.Xplot = min(Z,zq)
    max.Xplot = max(Z,zq)
    clases = (max.Xplot - min.Xplot)/50
    xline = seq(from = min.Xplot, to = max.Xplot, by = clases)
    plot(Z,P,
         pch = 20,
         cex = 0.5,
         xlim = c(min.Xplot,max.Xplot),
         main = main.title,
         sub = sub.title,
         xlab = xlab.name,
         ylab = ylab.name)
    points(zq, p, pch = 1, col = "blue")
    lines(xline, logit(x = (xline - as.vector(Zmu))/as.vector(Zs), u = fitLogit$par[1], s = fitLogit$par[2]), type = "l", col = "red")
    lines(xline, (xline*0 + p), type = "l", col = "blue")
    abline(v = zq, col = "blue")
    abline(v = tail.limit, col = "gray", lty = 2)
  }
  
  # Results to be included as invisible output
  if(save.AEDFvalues == TRUE){
    results$AEDF.X = as.numeric(Z)
    results$AEDF.Y = as.numeric(P) 
  }
  invisible(results) # only available if results are assigned to a variable.
}
