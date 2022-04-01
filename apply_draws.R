
library(posterior)
library(bayesplot)
library(patchwork)

apply_draws <- function(obj, FUN=NULL, MARGIN=NULL, ...) {
  
  save_attr <- attr(obj,"dimnames")
  
  save_dim <- dim(obj)
  
  if(length(MARGIN)<2) {
    
    stop("You must use the transform function over at least two array indices (such as chains and variables), otherwise the result will collapse to a matrix.")
    
  }
  
  obj <- apply(obj,MARGIN=MARGIN,FUN=FUN)
  
  new_dim <- match(save_dim,dim(obj))
  
  # need to re-arrange indices as apply can do that 

  obj <- aperm(obj,perm=new_dim)
  
  dimnames(obj) <- save_attr
  
  class(obj) <- c("draws_array","draws","array")
  
  return(obj)
  
}

# example

x <- example_draws()

# by chain and by variable so we can test for convergence

x_sq <- apply_draws(x,FUN=function(c) c^2, MARGIN=c(2:3))

identical(dimnames(x),dimnames(x_sq))
identical(dim(x),dim(x_sq))
identical(x,x_sq)

# check

orig <- mcmc_trace(x,"theta[1]")
transform <- mcmc_trace(x_sq,"theta[1]")

orig + transform
