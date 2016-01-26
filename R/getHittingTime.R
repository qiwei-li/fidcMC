getHittingTime = function(i=NULL, j=NULL, mc.obj){
  UseMethod("getHittingTime", mc.obj)
}

getHittingTime.DFmc = function(i=NULL, j=NULL, mc.obj){
  if(!check.irreducible(mc.obj))
    stop("Since this Markov chain is not irreducible, hitting time can't be computed")
  p = mc.obj$pijdef
  n = nrow(p)
  mat = matrix(-999, nrow=n, ncol=n)
  diag(mat) = NA
  for(x in 1:n){
    q = diag(n) - p
    q = q[-x, -x]
    ones = rep(1, n-1)
    ans = solve(q, ones)
    mat[!is.na(mat[,x]), x] = ans
  }
  if(is.null(i))
    i = 1:n
  if(is.null(j))
    j = 1:n
  return(mat[i,j])
}


getHittingTime.DImc = function(i=NULL, j=NULL, mc.obj){
  a = getStationaryDistribution(mc.obj)
  if(a[[1]]=="Fail")
    stop("ERROR: could not find a converging stationary distribution")
  p = a[[4]]
  n = nrow(p)
  mat = matrix(-999, nrow=n, ncol=n)
  diag(mat) = NA
  for(x in 1:n){
    q = diag(n) - p
    q = q[-x, -x]
    ones = rep(1, n-1)
    ans = solve(q, ones)
    mat[!is.na(mat[,x]), x] = ans
  }
  if(is.null(i))
    i = 1:n
  if(is.null(j))
    j = 1:n
  return(mat[i,j])
}


getHittingTime.CFmc = function(i=NULL, j=NULL, mc.obj){
  
}
  

getHittingTime.CImc = function(i=NULL, j=NULL, mc.obj){
  
}