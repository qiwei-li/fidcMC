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

print <- function(mcObj) UseMethod("print", mcObj)
print.DFmc <- function(mc){
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames, name = '')
  mc.s4
}

summary <- function(mcObj) UseMethod("summary", mcObj)
summary.DFmc <- function(mc){
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$stateNames, name = '')
  markovchain::summary(mc.s4)
}

plot <- function(mcObj) UseMethod("plot", mcObj)
plot.DFmc <- function(mc)
{
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

fitMarkovchain <- function(data_seq, method = "mle", byrow = TRUE, nboot = 10L,
                           laplacian = 0, name = "", parallel = FALSE,
                           confidencelevel = 0.95, hyperparam = matrix())
{
  #Given a sequence of states arising from a stationary state, it fits the underlying Markov chain dis- tribution using either MLE (also using a Laplacian smoother), bootstrap or by MAP (Bayesian) inference.
  markovchain::markovchainFit(date = data_seq, method = method, byrow = byrow, nboot = nroot,
                              laplacian = laplacian, name = name, parallel = parallel,
                              confidencelevel = confidencelevel, hyperparam = hyperparam)
}

generateMarkovchain <- function(n,mc)
{
  #Provided any markovchain or markovchainList objects, it returns a sequence of states coming from the underlying stationary distribution.
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$states)
  markovchain::rmarkovchain(n=n, object=mc.s4)
}

getConditionalDistribution <- function(mc,state)
{
  #It extracts the conditional distribution of the subsequent state, given current state.
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$states)
  markovchain::conditionalDistribution(object=mc.s4,state=state)
}


