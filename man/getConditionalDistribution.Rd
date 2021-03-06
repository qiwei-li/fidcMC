% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rewriteFunc.R
\name{getConditionalDistribution}
\alias{getConditionalDistribution}
\title{Extracts the conditional distribution from Markov Chain}
\usage{
getConditionalDistribution(mc, state)
}
\arguments{
\item{mc}{mc class.}

\item{state}{char.}
}
\value{
The conditional distribution for each subsequent state.
}
\description{
It extracts the conditional distribution of the subsequent state, given current state.
}

\references{
%% ~put references to the literature/web site here ~
A First Course in Probability (8th Edition), Sheldon Ross, Prentice Hall 2010

markovchain CRAN package
}

\examples{
statesNames=c("a","b","c")
markovB<- mc.create(matrix(c(0.2,0.5,0.3,0,1,0,0.1,0.8,0.1),nrow=3, byrow=TRUE, 
	dimnames=list(statesNames,statesNames)),discrete = TRUE ,infinite = FALSE)
getConditionalDistribution(markovB,"b")
}

