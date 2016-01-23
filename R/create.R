mc.create = function(pijdef, states, qidef=NULL, discrete=TRUE, infinite=FALSE, name=NULL){
  if(discrete==TRUE & infinite==FALSE){
    if(class(pijdef)!="matrix" && all(rowSums(pijdef)==1) && all(pijdef<1))
      stop("ERROR: pijdef needs to be a matrix with values smaller than 1 and rowSums equal to 1")
    if(nrow(pijdef) != ncol(pijdef))
      stop("ERROR: pijdef needs to be a square matrix")

    if(missing(states))
    {
      if(is.null(dimnames(pijdef)))
        states = LETTERS[1:dim(pijdef)[1]]
      else
        states = dimnames(pijdef)[[1]]
    }
    
    mc = list(pijdef=pijdef, states=states,qidef=NULL, type = "DF", name = name)
    return(structure(mc,class=c('DFMKT','MKT', 'mc')))
  }

  if(discrete==TRUE & infinite==TRUE){
    if(class(pijdef)!="function")
      stop("ERROR: pijdef needs to be a function with input: (i,j) and output: a probability from i to j")
    mc = list(pijdef=pijdef, qidef=NULL, type = "DI")
    structure(mc, class=c("mc", "matrix"))
    return(mc)
  }

  if(discrete==FALSE & infinite==FALSE){
    if(class(pijdef)!="matrix" && all(rowSums(pijdef)==1) && all(pijdef<1))
      stop("ERROR: pijdef needs to be a matrix with values smaller than 1 and rowSums equal to 1")
    if(class(qidef)!="numeric" & class(qidef)!="integer")
      stop("ERROR: qidef needs to be a numerical vector")
    if(nrow(pijdef) != ncol(pijdef))
      stop("ERROR: pijdef needs to be a square matrix")
    if(nrow(pijdef) != length(qidef))
      stop("ERROR: numbers of stages from pijdef and qijdef need to agree")

    if(missing(states))
      states = LETTERS[1:dim(pijdef)[1]]
    mc = list(pijdef=pijdef, states=states, qidef=qidef, type = "CF")

    return(mc)
  }

  if(discrete==FALSE & infinite==TRUE){
    if(class(pijdef)!="function")
      stop("ERROR: pijdef needs to be a function with input: (i,j) and output: a probability from i to j")
    if(class(qidef)!="function")
      stop("ERROR: qidef needs to be a function which input: i and output: the lamda of the holding time")
    mc = list(pijdef=pijdef, qidef=qidef, type = "CI")
    structure(mc, class=c("mc", "matrix"))
    return(mc)
  }
}

