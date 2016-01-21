calcPI = function(mc.obj){
  if(mc.obj$type == "DF"){
    p = mc.obj$pijdef
    n = nrow(p)
    imp = diag(n) - t(p)
    imp[n, ] = rep(1, n)
    rhs = c(rep(0, n-1), 1)
    return(solve(imp,rhs))
  }
  
  if(mc.obj$type == "DI"){
    warning("under construction")
  }
  
  if(mc.obj$type == "CF"){
    warning("under construction")
  }
  
  if(mc.obj$type == "CI"){
    warning("under construction")
  }
  
}

calcHit = function(i=NULL, j=NULL, mc.obj){
  if(mc.obj$type == "DF"){
    p = mc.obj$pijdef
    n = nrow(p)
    q = diag(n) - p
    q = q[-j, -j]
    ones = rep(1, n-1)
    ans = solve(q, ones)
    tmp = (1:n)[-j]
    return(ans[i])
  }
  
  if(mc.obj$type == "DI"){
    warning("under construction")
  }
  
  if(mc.obj$type == "CF"){
    warning("under construction")
  }
  
  if(mc.obj$type == "CI"){
    warning("under construction")
  }
  
}



getProb = function(start, end, mc.obj){
  pijdef = mc.obj$pijdef
  type = mc.obj$type
  n = length(start:end)
  if(type=="DI"){
    mat = matrix(0, nrow = n, ncol = n)
    for(i in c(1:n)){
      for(j in c(1:n)){
        mat[i,j] = pijdef(i,j)
      }
    }
    if(matCheck(mat) == "NOT PASS"){
      stop("ERROR: incorrect pijdef function")
    }
    return(mat)
  }
  
  if(type=="CI"){
    mat = matrix(0, nrow = n, ncol = n)
    rates = numeric(n)
    for(i in c(1:n)){
      for(j in c(1:n)){
        ans = pijdef(i,j)
        mat[i,j] = ans[[1]]
        rates[i] = ans[[2]]
      }
    }
    if(matCheck(mat) == "NOT PASS")
      stop("ERROR: incorrect pijdef definition")
    if(ratesCheck(rates) == "NOT PASS")
      stop("ERROR: incorrect qidef definition")
    return(mat)
  }
}

matCheck = function(mat){
  if(sum(mat<0))
    return("NOT PASS")
  if(sum(mat>1))
    return("NOT PASS")
  return("PASS")
}
ratesCheck = function(rates){
  if(sum(rates) >1)
    return("NOT PASS")
}