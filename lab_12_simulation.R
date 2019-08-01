generate_data <- function(n, p){
  covariates <- matrix(rnorm(n*p), nrow = n, ncol = p)
  responses <- vector(length = n)
  
  for (i in 1:length(responses)){
    responses[i] <- rnorm(n*p)
  }
  
  return(list(covariates = covariates, responses = responses))
}