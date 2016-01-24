\name{fitDiscreteMarkovchain}
\alias{fitDiscreteMarkovchain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Fit a discrete Markov chain
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Given a sequence of states arising from a stationary state, it fits the underlying Markov chain dis- tribution using either MLE (also using a Laplacian smoother), bootstrap or by MAP (Bayesian) inference.
}
\usage{
fitDiscreteMarkovchain(data_seq, method = "mle", byrow = TRUE, nboot = 10L,
                   laplacian = 0, name = "", parallel = FALSE,
                   confidencelevel = 0.95, hyperparam = matrix())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_seq}{
%%     ~~Describe \code{x} here~~
A character list
}
\item{method}{Method used to estimate the Markov chain. Either "mle", "map", "bootstrap" or "laplace"}
\item{byrow}{it tells whether the output Markov chain should show the transition probabilities by row.}
\item{nboot}{Number of bootstrap replicates in case "bootstrap" is used.}
\item{laplaxian}{Laplacian smoothing parameter, default zero. It is only used when "laplace" method is chosen.}
\item{name}{Optional character for name slot.}
\item{confidencelevel}{level for conficence intervals width. Used only when method equal to "mle".}
\item{hyperparam}{Hyperparameter matrix for the a priori distribution. If none is provided, default value of 1 is assigned to each parameter. This must be of size kxk where k is the number of states in the chain and the values should typically be non-negative integers.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list containing an estimate, log-likelihood, and, when "bootstrap" method is used, a matrix of standards deviations and the bootstrap samples. When the "mle", "bootstrap" or "map" method is used, the lower and upper confidence bounds are returned along with the standard error. The "map" method also returns the expected value of the parameters with respect to the posterior distribution.
}
\references{
markovchain CRAN project

A First Course in Probability (8th Edition), Sheldon Ross, Prentice Hall 2010

Inferring Markov Chains: Bayesian Estimation, Model Comparison, Entropy Rate, and Out-of- Class Modeling, Christopher C. Strelioff, James P. Crutchfield, Alfred Hubler, Santa Fe Institute

Yalamanchi SB, Spedicato GA (2015). Bayesian Inference of First Order Markov Chains. R pack- age version 0.2.5
}

\examples{
sequence<-c("a", "b", "a", "a", "a", "a", "b", "a", "b", "a", "b", "a", "a",
    "b", "b", "b", "a")
mcFitMLE<-fitDiscreteMarkovchain(data_seq=sequence)
mcFitBSP<-fitDiscreteMarkovchain(data_seq=sequence,method="bootstrap",nboot=5, name="Bootstrap Mc")

}
