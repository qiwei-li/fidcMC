\name{getAbsorbingStates}
\alias{getTransientStates}
\alias{getCommunicatingClasses}
\alias{getRecurrentClasses}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Various function to perform structural analysis of Markov chain
}
\description{
These functions return absorbing and transient states of the markovchain objects.
}
\usage{
getAbsorbingStates(mc)
getTransientStates(mc)
getCommunicatingClasses(mc)
getRecurrentClasses(mc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mc}{ mc class object
}
}
\value{
vector or list object
}
\references{
markovchain CRAN package
}

\examples{
## The function is currently defined as
statesNames=c("a","b","c")
markovB<-mc.create(matrix(c(0.2,0.5,0.3,
                       0,1,0,0.1,0.8,0.1),nrow=3, byrow=TRUE, dimnames=list               (statesNames,statesNames)
                     ), discrete=T, infinite=F)
getAbsorbingStates(markovB)
getTransientStates(markovB)
getCommunicatingClasses(markovB)
getRecurrentClasses(markovB)
}
