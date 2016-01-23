getSteadyStates = function(mc.obj, epsilon = 0.01){
  if(mc.obj$type == "DF"){
    p = mc.obj$pijdef
    n = nrow(p)
    imp = diag(n) - t(p)
    imp[n, ] = rep(1, n)
    rhs = c(rep(0, n-1), 1)
    PI = solve(imp,rhs)
    names(PI) = mc.obj$states
    return(PI)
  }

  if(mc.obj$type == "DI"){
    k1=10
    k2=20

    p1 = getProb(1,k1, mc.obj)
    n1 = nrow(p1)
    imp1 = diag(n1) - t(p1)
    imp1[n1, ] = rep(1, n1)
    rhs1 = c(rep(0, n1-1), 1)
    ans1 = solve(imp1,rhs1)

    p2 = getProb(1,k2, mc.obj)
    n2 = nrow(p2)
    imp2 = diag(n2) - t(p2)
    imp2[n2, ] = rep(1, n2)
    rhs2 = c(rep(0, n2-1), 1)
    ans2 = solve(imp2, rhs2)

    meanDiff = mean(ans1 - ans2[1:k1])

    while(meanDiff > epsilon){
      k1 = k1*2
      k2 = k2*2

      p1 = getProb(1,k1, mc.obj)
      n1 = nrow(p1)
      imp1 = diag(n1) - t(p1)
      imp1[n1, ] = rep(1, n1)
      rhs1 = c(rep(0, n1-1), 1)
      ans1 = solve(imp1,rhs1)

      p2 = getProb(1,k2, mc.obj)
      n2 = nrow(p2)
      imp2 = diag(n2) - t(p2)
      imp2[n2, ] = rep(1, n2)
      rhs2 = c(rep(0, n2-1), 1)
      ans2 = solve(imp2, rhs2)

      meanDiff = mean(ans1 - ans2[1:k1])
    }
    return(ans2)
  }

  if(mc.obj$type == "CF"){
    warning("under construction")
  }

  if(mc.obj$type == "CI"){
    warning("under construction")
  }

}

calcHit = function(i=NULL, j=NULL, mc.obj){
  if(!check.irreducible(mc.obj))
    stop("Since this Markov chain is not irreducible, hitting time can't be computed")

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
