# tests
library(infiniteMC)
# testing for discrete finite case
threeHeads = function(){
  p = matrix(0, nrow=3, ncol=3)
  p[1, c(1,2)] = 0.5
  p[2, c(1,3)] = 0.5
  p[3, 1] = 1
  return(p)
}
ex = mc.create(pijdef = threeHeads(), discrete = TRUE, infinite = FALSE)
ans = getStationaryDistribution(ex)
if(sum(round(ans,3) != c(0.571,0.286,0.143)))
  stop("DF method is wrong")
ans = getHittingTime(mc.obj = ex)
if(length(which((as.numeric(ans)!=c(NA,1.5,1.0,2.0,NA,3.0,6.0,4.0,NA))==TRUE)))
  stop("DF method is wrong")


# testing for discrete infinite case
singleServer = function(i,j){
  p = 0.3
  q = 0.7
  r = 0
  if(j == i+1)
    return(p)
  if(j == i-1)
    return(q)
  if(j==i)
    return(r)
  return(0)
}
ex = mc.create(pijdef=singleServer, discrete=TRUE, infinite=TRUE)
ans = getStationaryDistribution(ex)
if(round(ans[[3]][1:5]) != c(0.0618, 0.0883, 0.0996, 0.1045, 0.1065))
   stop("DI method is wrong")


# testing for continuous finite case
machineRepair = function(){
  pijdef = matrix(0, nrow=3, ncol=3)
  pijdef[1,2] = 1
  pijdef[2, c(1,3)] = c(0.28, 0.72)
  pijdef[3,2] = 1
  lamda = c(0.25, 0.175, 0.08)
  return(list(pijdef=pijdef, lamda = lamda))
}
ex = mc.create(pijdef = machineRepair()$pijdef, qidef = machineRepair()$lamda, discrete = FALSE, infinite = FALSE)
ans = getStationaryDistribution(ex)
if(sum(round(ans,3) != c(0.071,0.361,0.568)))
  stop("CF method is wrong")


# testing for continuous infinite case



