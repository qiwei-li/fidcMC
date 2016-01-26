\name{getStationaryDistribution}
\alias{getStationaryDistribution}
\title{getStationaryDistribution}
\description{Calculate the stationary distribution of a markov chain if it exists}
\usage{
getStationaryDistribution(mc.obj, epsilon=0.01, iteration=30, totalProb=0.9)
}
\arguments{
\item{mc.obj}{Markov chain object created by the 'mc.create' function}
\item{epsilon}{The threshold for the converging (in cosine). Needed in infinite markov chain. Default to 0.01.}
\item{iteration}{The max iteration for the approximation algorithm. Needed in infinite markov chain. Default to 30}
\item{totalProb}{The proportion of information that the approximate distribution contains. Default to 0.9}
}
\value{
In the case of finite, returns the stationary distribution. In the case of infinite, returns a list containing an success indicator, converge state count, approximated stationary distribution, and converge transition matrix.
}
\examples{
singleServer = function(i,j){
  if(i==1 && j==2)
    return(1)
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
