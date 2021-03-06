getStationaryDistribution = function(mc.obj, epsilon = 0.01, iteration=30, totalProb=0.9){
  UseMethod("getStationaryDistribution", mc.obj)
}

getStationaryDistribution.DFmc = function(mc.obj, epsilon = 0.01, iteration=30, totalProb=0.9){
  p = mc.obj$pijdef
  n = nrow(p)
  imp = diag(n) - t(p)
  imp[n, ] = rep(1, n)
  rhs = c(rep(0, n-1), 1)
  PI = solve(imp,rhs)
  names(PI) = mc.obj$stateNames
  return(PI)
}

getStationaryDistribution.DImc = function(mc.obj, epsilon = 0.01, iteration=30, totalProb=0.9){
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
  cos = cosine(ans1, ans2[1:k1])
  sumProb = sum(ans2)
  
  while(cos < (1-epsilon) || cos > (1+epsilon) || sumProb < totalProb){
    currentTry = currentTry+1
    print(paste0(currentTry, "th try. cos is:", round(cos,3), " Trying approximation with ", k1, " stages."))
    if(currentTry > iteration){
      print(paste0("Failed converage after ", iteration, " iterations"))
      return(list("Fail", k2, ans2, p2))
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
    cos = cosine(ans1, ans2[1:k1])
    sumProb = sum(ans2)
  }
  print(paste0("Succeeded converage after ", currentTry, " iterations."))
  return(list("Success", k2, ans2, p2))
}

getStationaryDistribution.CFmc = function(mc.obj, epsilon = 0.01, iteration=30, totalProb=0.9){
  p = mc.obj$pijdef
  lamda = mc.obj$qidef
  if(!is.null(lamda)){
    q = getInfinitesimalGenerator(p, lamda)
    n = nrow(q)
    q[n, ] = rep(1, n)
    rhs = c(rep(0, n-1), 1)
    pi = solve(q, rhs)
  }
  else{
    n = nrow(p)
    p[n, ] = rep(1, n)
    rhs = c(rep(0, n-1), 1)
    pi = solve(p, rhs)
  }
  return(pi)
}

getStationaryDistribution.CImc = function(mc.obj, epsilon = 0.01, iteration=30, totalProb=0.9){
  currentTry = 1
  k1=10
  k2=20

  tt = getFromFunction(mc.obj, 1, k1)
  p1 = tt$mat
  lamda1 = tt$lamda
  if(!is.null(lamda1)){
    q1 = getInfinitesimalGenerator(p1, lamda1)
    n1 = nrow(q1)
    q1[n1, ] = rep(1, n1)
    rhs = c(rep(0, n1-1), 1)
    ans1 = solve(q1, rhs)
  }
  else{
    n1 = nrow(p1)
    p1[n1, ] = rep(1, n1)
    rhs = c(rep(0, n1-1), 1)
    ans1 = solve(p1, rhs)
  }

  tt = getFromFunction(mc.obj, 1, k2)
  p2 = tt$mat
  lamda2 = tt$lamda
  if(!is.null(lamda2)){
    q2 = getInfinitesimalGenerator(p2, lamda2)
    n2 = nrow(q2)
    q2[n2, ] = rep(1, n2)
    rhs = c(rep(0, n2-1), 1)
    ans2 = solve(q2, rhs)
  }
  else{
    n2 = nrow(p2)
    p2[n2, ] = rep(1, n2)
    rhs = c(rep(0, n2-1), 1)
    ans2 = solve(p2, rhs)
  }

  cos = cosine(ans1, ans2[1:k1])
  sumProb = sum(ans2)
  
  while(cos < (1-epsilon) || cos > (1+epsilon) || sumProb < totalProb){
    currentTry = currentTry+1
    print(paste0(currentTry, "th try. cos is:", round(cos,3), " Trying approximation with ", k1, " stages."))
    if(currentTry > iteration){
      print(paste0("Failed converage after ", iteration, " iterations"))
      return(list("Fail", k2, ans2, p2))
    }
    k1 = k1+20
    k2 = k2+20
    tt = getFromFunction(mc.obj, 1, k1)
    p1 = tt$mat
    lamda1 = tt$lamda
    if(!is.null(lamda1)){
      q1 = getInfinitesimalGenerator(p1, lamda1)
      n1 = nrow(q1)
      q1[n1, ] = rep(1, n1)
      rhs = c(rep(0, n1-1), 1)
      ans1 = solve(q1, rhs)
    }
    else{
      n1 = nrow(p1)
      p1[n1, ] = rep(1, n1)
      rhs = c(rep(0, n1-1), 1)
      ans1 = solve(p1, rhs)
    }
    
    tt = getFromFunction(mc.obj, 1, k2)
    p2 = tt$mat
    lamda2 = tt$lamda
    if(!is.null(lamda2)){
      q2 = getInfinitesimalGenerator(p2, lamda2)
      n2 = nrow(q2)
      q2[n2, ] = rep(1, n2)
      rhs = c(rep(0, n2-1), 1)
      ans2 = solve(q2, rhs)
    }
    else{
      n2 = nrow(p2)
      p2[n2, ] = rep(1, n2)
      rhs = c(rep(0, n2-1), 1)
      ans2 = solve(p2, rhs)
    }
    cos = cosine(ans1, ans2[1:k1])
    sumProb = sum(ans2)
  }
  print(paste0("Succeeded converage after ", currentTry, " iterations."))
  return(list("Success", k2, ans2, p2))
}