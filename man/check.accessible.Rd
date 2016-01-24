\name{check.accessible}
\alias{check.accessible}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Verify if a state j is reachable from state i
}
\description{
This function verifies if a state is reachable from another, i.e., if exists a path that leads to state j leaving from state i with positive probability
}
\usage{
check.accessible(mc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mc}{
mc class object
}
}
\value{
a boolean value
}
\references{
markovchain CRAN package
}
\examples{
statesNames=c("a","b","c")
mcB<-mc.create(matrix(c(0.2,0.5,0.3,0,0.2,0.8,0.1,0.8,0.1),nrow=3, byrow=TRUE, dimnames=list(statesNames,statesNames)), discrete = T,infinite = F)
check.accessible(markovB,"a","c")
}
