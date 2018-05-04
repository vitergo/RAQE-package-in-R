#functions to fit quantiles

gumbel.max <- function(x, u, s){
  f = exp(-exp(-(x-u)/s))
  return(f)
}

gumbel.max.inv <- function(p, u, s){
  q = u - s*log(-log(p))
  return(q)
}

SSEgumbel.max <- function(data, par, W = 1){
  u = par[1]
  s = par[2]
  E = data[,2] - gumbel.max(data[,1],u = u, s = s)
  SE = W*E^2
  SSE = sum(SE)
  return(SSE)
}

gumbel.min <- function(x, u, s){
  f = 1- exp(-exp((x-u)/s))
  return(f)
}

gumbel.min.inv <- function(p, u, s){
  q = u + s*log(-log(1-p))
  return(q)
}

SSEgumbel.min <- function(data, par, W = 1){
  u = par[1]
  s = par[2]
  E = data[,2] - gumbel.min(data[,1],u = u, s = s)
  SE = W*E^2
  SSE = sum(SE)
  return(SSE)
}

exponential <- function(x, u, theta){
  f = 1- exp(-(x-u)/theta)
  return(f)
}

exponential.inv <- function(p, theta, u){
  q = u-theta*log(1-p)
  return(q)
}

SSEexponential <- function(data, par, W = 1){
  u = par[1]
  theta = par[2]
  E = data[,2] - exponential(data[,1],theta = theta, u = u)
  SE = W*E^2
  SSE = sum(SE)
  return(SSE)
}

weibull <- function(x, u, a, b){ #u is location, a is shape, b is scale
  f = 1 - exp( -((x - u)/b)^a)
  return(f)
}

weibull.inv <- function(p, u, a, b){ #u is location, a is shape, b is scale
  q = u + b * ( -log(1-p))^(1/a)
  return(q)
}

SSEweibull <- function(data, par, W = 1){ # par = c(a,b), a is shape, b is scale
  u = par[1]
  a = par[2]
  b = par[3]
  E = data[,2] - weibull(data[,1],u = u, a = a, b = b)
  SE = W*E^2
  SSE = sum(SE)
  return(SSE)
}

logit <-function(x, u, s){
  f = 1 / ( 1 + exp(-((x-u)/s)) )
  return(f)
}

logit.inv <- function(p, u, s){
  q = u + s * log(p/(1-p))
  return(q)
}

SSElogit <- function(data, par, W = 1){
  u = par[1]
  s = par[2]
  E = data[,2] - logit(data[,1],u = u, s = s)
  SE = W*E^2
  SSE = sum(SE)
  return(SSE)
}
