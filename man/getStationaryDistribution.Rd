\name{getStationaryDistribution}
\alias{getStationaryDistribution}
\title{getStationaryDistribution}
\description{Calculate the stationary distribution of a markov chain if it exists}
\usage{
getStationaryDistribution(mc.obj, epsilon = 0.01, iteration=10)
}
\arguments{
\item{mc.obj}{Markov chain object created by the 'mc.create' function}
\item{epsilon}{The threshold for the approximation to be considered as converging. Needed in the case of infinite markov chain.}
\item{iteration}{The max iteration for the approximation algorithm. Needed in the case of infinite markov chain.}

}
\value{
In the case of finite, returns the stationary distribution. In the case of infinite, returns a list containing an success indicator, converge state count, approximated stationary distribution, and converge transition matrix.
}
\examples{
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
ans = getStationaryDistribution(ex)
}
\keyword{getStationaryDistribution}
