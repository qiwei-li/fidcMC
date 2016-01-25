getStationaryDistribution = function(mc.obj, epsilon = 0.01, iteration=10){
  UseMethod("getStationaryDistribution", mc.obj)
}

getStationaryDistribution.DFmc = function(mc.obj, epsilon = 0.01, iteration=10){
  p = mc.obj$pijdef
  n = nrow(p)
  imp = diag(n) - t(p)
  imp[n, ] = rep(1, n)
  rhs = c(rep(0, n-1), 1)
  PI = solve(imp,rhs)
  names(PI) = mc.obj$stateNames
  return(PI)
}

getStationaryDistribution.DImc = function(mc.obj, epsilon = 0.01, iteration=10){
  currentTry = 1
  k1=10
  k2=20
  p1 = getFromFunction(mc.obj, 1, k1)
  n1 = nrow(p1)
  imp1 = diag(n1) - t(p1)
  imp1[n1, ] = rep(1, n1)
  rhs1 = c(rep(0, n1-1), 1)
  ans1 = solve(imp1,rhs1)
  p2 = getFromFunction(mc.obj, 1, k2)
  n2 = nrow(p2)
  imp2 = diag(n2) - t(p2)
  imp2[n2, ] = rep(1, n2)
  rhs2 = c(rep(0, n2-1), 1)
  ans2 = solve(imp2, rhs2)
  cosineDiff = 1-cosine(ans1, ans2[1:k1])
  
  while(cosineDiff > epsilon){
    currentTry = currentTry+1
    if(currentTry > iteration){
      return(list("Fail", paste0("Failed converage after ", iteration, " iterations")))
    }
    k1 = k1+20
    k2 = k2+20
    p1 = getFromFunction(mc.obj, 1, k1)
    n1 = nrow(p1)
    imp1 = diag(n1) - t(p1)
    imp1[n1, ] = rep(1, n1)
    rhs1 = c(rep(0, n1-1), 1)
    ans1 = solve(imp1,rhs1)
    p2 = getFromFunction(mc.obj, 1, k2)
    n2 = nrow(p2)
    imp2 = diag(n2) - t(p2)
    imp2[n2, ] = rep(1, n2)
    rhs2 = c(rep(0, n2-1), 1)
    ans2 = solve(imp2, rhs2)
    cosineDiff = 1-cosine(ans1, ans2[1:k1])
  }
  return(list("Success",  k2, ans2, p2))
}

getStationaryDistribution.CFmc = function(mc.obj, epsilon = 0.01, iteration=10){
  p = mc.obj$pijdef
  lamda = mc.obj$qidef
  q = getInfinitesimalGenerator(p, lamda)
  n = nrow(q)
  q[n, ] = rep(1, n)
  rhs = c(rep(0, n-1), 1)
  pi = solve(q, rhs)
  return(pi)
}

getStationaryDistribution.CImc = function(mc.obj, epsilon = 0.01, iteration=10){
  
}