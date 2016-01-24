\name{check.irreducible}
\alias{check.irreducible}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check if a Markov chain is irreducible
}
\description{
This function verifies whether a markovchain object transition matrix is composed by only one communicating class.
}
\usage{
check.irreducible(mc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mc}{
mc class object
}
}
\value{
A boolean value
\references{
markovchain CRAN package
}
\examples{
statesNames<-c("a","b","c")
mat = matrix(c(0.2,0.5,0.3,
                   0,1,0,
                   0.1,0.8,0.1),nrow=3, byrow=TRUE, dimnames=list(statesNames,statesNames)
)
mc.test = mc.create(mat,discrete=T,infinite=F)
check.irreducible(mc.test)
}
