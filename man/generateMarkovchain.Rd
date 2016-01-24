\name{generateMarkovchain}
\alias{generateMarkovchain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Generate a sequence of states from a Markov chain.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Provided any markovchain or markovchainList objects, it returns a sequence of states coming from the underlying stationary distribution.
}
\usage{
generateMarkovchain(n,mc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{x} here~~
integer
}
\item{mc}{
%%     ~~Describe \code{x} here~~
mc class
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A vector of states names
}
\references{
%% ~put references to the literature/web site here ~
A First Course in Probability (8th Edition), Sheldon Ross, Prentice Hall 2010

markovchain CRAN package
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## The function is currently defined as
#define the Markov chain
statesNames=c("a","b","c")
mcB<-mc.create(matrix(c(0.2,0.5,0.3,0,0.2,0.8,0.1,0.8,0.1),nrow=3, byrow=TRUE, dimnames=list(statesNames,statesNames)), discrete = T,infinite = F)
outs<-generateMarkovchain(n=20,mcB)
}

