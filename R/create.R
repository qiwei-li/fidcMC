mc.create = function(pijdef, stateNames=NULL, chainName=NULL, qidef=NULL, discrete, infinite){
  if(discrete==TRUE & infinite==FALSE){
    if(class(pijdef)!="matrix")
      stop("ERROR: pijdef needs to be a matrix")
    if(nrow(pijdef) != ncol(pijdef))
      stop("ERROR: pijdef needs to be a square matrix")
    if(sum(pijdef<0))
      stop("ERROR: pijdef needs to contain values >= 0")
    if(sum(pijdef>1))
      stop("ERROR: pijdef needs to contain values <= 1")
    if(is.null(stateNames)){
      if(is.null(dimnames(pijdef)))
        stateNames = as.character(c(1:nrow(pijdef)))
      else
        stateNames = dimnames(pijdef)[[1]]
    }
    obj = structure(list(pijdef=pijdef, stateNames=stateNames, chainName=chainName), class=c("DFmc","mc", "list"))
    return(obj)
  }

  if(discrete==TRUE & infinite==TRUE){
    if(class(pijdef)!="function")
      stop("ERROR: pijdef needs to be a function with input: (i,j) and output: a probability from i to j")
    obj = structure(list(pijdef=pijdef, chainName=chainName), class=c("DImc","mc", "list"))
    return(obj)
  }

  if(discrete==FALSE & infinite==FALSE){
    if(class(pijdef)!="matrix")
      stop("ERROR: pijdef needs to be a matrix")
    if(nrow(pijdef) != ncol(pijdef))
      stop("ERROR: pijdef needs to be a square matrix")
    if(sum(pijdef<0))
      stop("ERROR: pijdef needs to contain values >= 0")
    if(sum(pijdef>1))
      stop("ERROR: pijdef needs to contain values <= 1")
    if(class(qidef)!="numeric" & class(qidef)!="integer")
      stop("ERROR: qidef needs to be a numerical vector")
    if(nrow(pijdef) != length(qidef))
      stop("ERROR: numbers of stages from pijdef and qijdef need to agree")
    if(is.null(stateNames)){
      if(is.null(dimnames(pijdef)))
        stateNames = as.character(c(1:nrow(pijdef)))
      else
        stateNames = dimnames(pijdef)[[1]]
    }
    obj = structure(list(pijdef=pijdef, qidef=qidef, stateNames=stateNames, chainName=chainName), class=c("CFmc","mc", "list"))
    return(obj)
  }

  if(discrete==FALSE & infinite==TRUE){
    if(class(pijdef)!="function")
      stop("ERROR: pijdef needs to be a function with input: (i,j) and output: a probability from i to j")
    if(class(qidef)!="function")
      stop("ERROR: qidef needs to be a function which input: i and output: the lamda of the holding time")
    obj = structure(list(pijdef=pijdef, qidef=qidef, chainName=chainName), class=c("CImc","mc", "list"))
    return(obj)
  }
}
