\name{getRecurrentClasses}
\alias{getRecurrentClasses}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Recurrent Classes of Markov chain
}
\description{
The function returns recurrent classes of the markovchain object.
}
\usage{
getRecurrentClasses(mc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mc}{ mc class object}
}
\value{
list of separate communicating classes - state names
}
\references{
markovchain CRAN package
}
\examples{
statesNames=c("a","b","c")
markovB<-mc.create(matrix(c(0.2,0.5,0.3,
                       0,1,0,0.1,0.8,0.1),nrow=3, byrow=TRUE, dimnames=list               (statesNames,statesNames)
                     ), discrete=TRUE, infinite=FALSE)
getRecurrentClasses(markovB)
}
