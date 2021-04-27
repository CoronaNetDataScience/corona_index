
library(posterior)
library(dplyr)

obj <- example_draws()

summarize_draws_mc <- function(obj,  ...,cores=parallel::detectCores()) {
  
  varlist <- dimnames(obj)$variable
  
  # find an optimal-ish breakdown of variable names
  
  var_break <- floor(length(varlist)/cores)
  var_extra <- length(varlist) %% cores
  
  if(var_extra>0) {
    
    var_splits <- c(rep(1:cores,each=var_break),
                    1:var_extra)
    
  } else {
    
    var_splits <- rep(1:cores,each=var_break)
    
  }
  
  over_vars <- parallel::mclapply(1:cores, function(c,...){
    
    subset_draws(obj,variable=varlist[var_splits==c]) %>% 
      summarize_draws(...)
    
  },mc.cores=cores) %>% 
    bind_rows
  
  slice(over_vars,match(varlist,variable))
  
}

sum_obj <- summarize_draws(obj)
sum_obj_mc <- summarize_draws_mc(obj)

identical(sum_obj,sum_obj_mc)

#check time differences

time1 <- system.time(summarize_draws(obj))
time2 <- system.time(summarize_draws_mc(obj))

time1
time2

# not particularly efficient, but should speed up with more variables/parameters
