\name{mc.create}
\alias{mc.create}
\title{mc.create}
\description{Create an appropriate markov chain object}
\usage{
mc.create(pijdef, stateNames=NULL, chainName=NULL, qidef=NULL, discrete, infinite)
}
\arguments{
\item{pijdef}{Transition matrix definition. Can be a matrix or a function with input(i,j) and output P(i to j).}
\item{stateNames}{Names of states. Default to NULL, which forces the names to be 1 to n.}
\item{chainName}{Name of the markov chain. Default to NULL.}
\item{qidef}{A vector of holding time distribution parameters. Must have for continuous markov chain.}
\item{discrete}{A logical value indicating if the markov chain is discrete (FALSE means continuous).}
\item{infinite}{A logical value indicating if the markov chain is infinite (FALSE means finite).}
}
\value{
returns an appropriate markov chain object
}
\examples{
# discrete finite
threeHeads = function(){
  p = matrix(0, nrow=3, ncol=3)
  p[1, c(1,2)] = 0.5
  p[2, c(1,3)] = 0.5
  p[3, 1] = 1
  return(p)
}
ex = mc.create(pijdef = threeHeads(), discrete = TRUE, infinite = FALSE)

# discrete infinite 
singleServer = function(i,j){
  p = 0.3
  q = 0.7
  r = 0
  if(j == i+1)
    return(p)
  if(j == i-1)
    return(q)
  if(j==i)
    return(r)
  return(0)
}
ex = mc.create(pijdef=singleServer, discrete=TRUE, infinite=TRUE)
}
\keyword{mc.create}
