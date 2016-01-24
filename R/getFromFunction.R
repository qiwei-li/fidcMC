getFromFunction = function(mc.obj, start, end){
  UseMethod("getFromFunction", mc.obj)
}

getFromFunction.DImc = function(mc.obj, start, end){
  pijdef = mc.obj$pijdef
  n = length(start:end)
  mat = matrix(0, nrow = n, ncol = n)
  for(i in c(1:n)){
    for(j in c(1:n)){
      mat[i,j] = pijdef(i,j)
    }
  }
  return(mat)
}

getFromFunction.CImc = function(mc.obj, start, end){
  pijdef = mc.obj$pijdef
  qidef = mc.obj$qidef
  n = length(start:end)
  mat = matrix(0, nrow = n, ncol = n)
  lamda = numeric(n)
  for(i in c(1:n)){
    for(j in c(1:n)){
      mat[i,j] = pijdef(i,j)
      lamda[i] = qidef(i)
    }
  }
  return(list(mat=mat, lamda=lamda))
}