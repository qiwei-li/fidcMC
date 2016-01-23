mc.create = function(pijdef, stateNames=NULL, chainName=NULL, qidef=NULL, discrete=TRUE, infinite=FALSE){
  if(discrete==TRUE & infinite==FALSE){
    if(class(pijdef)!="matrix")
      stop("ERROR: pijdef needs to be a matrix")
    if(nrow(pijdef) != ncol(pijdef))
      stop("ERROR: pijdef needs to be a square matrix")
    if(sum(pijdef<0))
      stop("ERROR: pijdef needs to contain values >= 0")
    if(sum(pijdef>1))
      stop("ERROR: pijdef needs to contain values <= 1")
    if(stateNames == NULL){
      if(is.null(dimnames(pijdef)))
        stateNames = c(1:nrow(pijdef))
      else
        stateNames = dimnames(pijdef)[[1]]
    }
    mc = list(pijdef=pijdef, stateNames=stateNames, chainName=chainName)
    structure(mc, class=c("DFmc","mc", "list"))
    return(mc)
  }

  if(discrete==TRUE & infinite==TRUE){
    if(class(pijdef)!="function")
      stop("ERROR: pijdef needs to be a function with input: (i,j) and output: a probability from i to j")
    mc = list(pijdef=pijdef, chainName=chainName)
    structure(mc, class=c("DImc","mc", "list"))
    return(mc)
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
    if(stateNames == NULL){
      if(is.null(dimnames(pijdef)))
        stateNames = c(1:nrow(pijdef))
      else
        stateNames = dimnames(pijdef)[[1]]
    }
    mc = list(pijdef=pijdef, qidef=qidef, stateNames=stateNames, chainName=chainName)
    structure(mc, class=c("CFmc","mc", "list"))
    return(mc)
  }

  if(discrete==FALSE & infinite==TRUE){
    if(class(pijdef)!="function")
      stop("ERROR: pijdef needs to be a function with input: (i,j) and output: a probability from i to j")
    if(class(qidef)!="function")
      stop("ERROR: qidef needs to be a function which input: i and output: the lamda of the holding time")
    mc = list(pijdef=pijdef, qidef=qidef, chainName=chainName)
    structure(mc, class=c("CImc","mc", "list"))
    return(mc)
  }
}
