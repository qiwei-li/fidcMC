# extra functions
getInfinitesimalGenerator = function(p, lamda){
  n = nrow(p)
  Q = matrix(0, nrow=n, ncol=n)
  for(i in 1:n){
    for(j in 1:n){
      Q[i,j] = lamda[j]*p[j,i]
    }
  }
  diag(Q) = -lamda
  return(Q)
}

#Useful helper functions integrated from the "markovchain" package

#print <- function(mcObj) UseMethod("print", mcObj)
print.DFmc <- function(x, ...){
  mc = x
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames, name = '')
  mc.s4
}

#summary <- function(mcObj) UseMethod("summary", mcObj)
summary.DFmc <- function(object, ...){
  mc = object
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames, name = '')
  markovchain::summary(mc.s4)
}

#plot <- function(mcObj) UseMethod("plot", mcObj)
plot.DFmc <- function(x, ...){
  mc = x
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames)
  markovchain::plot(mc.s4)
}

check.accessible <- function(mc, from, to)
{
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames)
  markovchain::is.accessible(mc.s4, from=from, to=to)
}

check.irreducible <- function(mc)
{
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames)
  markovchain::is.irreducible(mc.s4)
}

getAbsorbingStates <- function(mc)
{
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames)
  markovchain::absorbingStates(mc.s4)
}

getTransientStates <- function(mc)
{
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames)
  markovchain::transientStates(mc.s4)
}

getCommunicatingClasses <- function(mc)
{
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames)
  markovchain::communicatingClasses(mc.s4)
}

getRecurrentClasses <- function(mc)
{
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames)
  markovchain::recurrentClasses(mc.s4)
}

getPeriod <- function(mc)
{
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames)
  markovchain::period(mc.s4)
}

fitDiscreteMarkovchain <- function(data_seq, method = "mle", byrow = TRUE, nboot = 10L,
                           laplacian = 0, name = "", parallel = FALSE,
                           confidencelevel = 0.95, hyperparam = matrix())
{
  #Given a sequence of states arising from a stationary state, it fits the underlying Markov chain dis- tribution using either MLE (also using a Laplacian smoother), bootstrap or by MAP (Bayesian) inference.
  markovchain::markovchainFit(data = data_seq, method = method, byrow = byrow, nboot = nboot,
                              laplacian = laplacian, name = name, parallel = parallel,
                              confidencelevel = confidencelevel, hyperparam = hyperparam)
}

generateMarkovchain <- function(n,mc)
{
  #Provided any markovchain or markovchainList objects, it returns a sequence of states coming from the underlying stationary distribution.
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames)
  markovchain::rmarkovchain(n=n, object=mc.s4)
}

#' It extracts the conditional distribution of the subsequent state, given current state.
#'
#' @param mc  mc class.
#' @param state char.
#' @return The conditional distribution for each subsequent state.
#' @examples
#' statesNames=c("a","b","c")
#' markovB<- mc.create(matrix(c(0.2,0.5,0.3,0,1,0,0.1,0.8,0.1),nrow=3, byrow=TRUE, dimnames=list(statesNames,statesNames)),discrete = T,infinite = F)
#' getConditionalDistribution(markovB,"b")
getConditionalDistribution <- function(mc,state)
{
  #It extracts the conditional distribution of the subsequent state, given current state.
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames)
  markovchain::conditionalDistribution(object=mc.s4,state=state)
}


