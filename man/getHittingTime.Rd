\name{getHittingTime}
\alias{getHittingTime}
\title{getHittingTime}
\description{Calculate the hitting time matrix of a markov chain if it exists}
\usage{
getHittingTime(i=NULL, j=NULL, mc.obj)
}
\arguments{
\item{i}{row or rows to be returned in the hitting time matrix. Default to NULL and to return entire matrix.}
\item{j}{column or columns to be returned in the hitting time matrix. Default to NULL and to return entire matrix.}
\item{mc.obj}{Markov chain object created by the 'mc.create' function}
}
\value{
returns the submatrix of the hitting time matrix
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
ans = getHittingTime(mc.obj = ex)
}
\keyword{getHittingTime}
