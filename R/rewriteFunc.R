print <- function(mcObj) UseMethod("print", mcObj)
print.DFMKT <- function(mc)
{
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$states, name = mc$name)
  mc.s4
}

summary <- function(mcObj) UseMethod("summary", mcObj)
summary.DFMKT <- function(mc)
{
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$states, name = mc$name)
  markovchain::summary(mc.s4)
}

plot <- function(mcObj) UseMethod("plot", mcObj)
plot.DFMKT <- function(mc)
{
  mc.s4 = new('markovchain',transitionMatrix=mc$pijdef,states=mc$states)
  markovchain::plot(mc.s4)
}

