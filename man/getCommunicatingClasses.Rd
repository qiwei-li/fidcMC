\name{getCommunicatingClasses}
\alias{getCommunicatingClasses}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Communicating Classes of Markov chain
}
\description{
The function returns communicating classes of the markovchain object.
}
\usage{
getCommunicatingClasses(mc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mc}{ mc class object}
%%     ~~Describe \code{x} here~~
}
\value{
list of separate communicating classes - state names
}
\references{
markovchain CRAN package
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
statesNames=c("a","b","c")
markovB<-mc.create(matrix(c(0.2,0.5,0.3,
                       0,1,0,0.1,0.8,0.1),nrow=3, byrow=TRUE, dimnames=list               (statesNames,statesNames)
                     ), discrete=TRUE, infinite=FALSE)
getCommunicatingClasses(markovB)
}

