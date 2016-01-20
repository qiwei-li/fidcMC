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