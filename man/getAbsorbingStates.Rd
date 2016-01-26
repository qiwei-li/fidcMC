\name{getAbsorbingStates}
\alias{getAbsorbingStates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Absorbing States of Markov chain
}
\description{
The function return absorbing states of the markovchain object.
}
\usage{
getAbsorbingStates(mc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mc}{ mc class object}
}
\value{
vector of characters for state names
}
\references{
markovchain CRAN package
}

\examples{
## The function is currently defined as
statesNames=c("a","b","c")
markovB<-mc.create(matrix(c(0.2,0.5,0.3,
                       0,1,0,0.1,0.8,0.1),nrow=3, byrow=TRUE, dimnames=list(statesNames,statesNames)), discrete=TRUE, infinite=FALSE)
getAbsorbingStates(markovB)
}
