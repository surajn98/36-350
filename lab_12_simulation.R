generate_data <- function(n, p){
  covariates <- matrix(rnorm(n*p), nrow = n, ncol = p)
  responses <- vector(length = n)
  
  for (i in 1:length(responses)){
    responses[i] <- rnorm(n*p)
  }
  
  return(list(covariates = covariates, responses = responses))
}

model_select <- function(covariates, responses, cutoff){
  lin_reg_1 <- lm(responses ~ covariates)
  
  indices <- which(lin_reg_1$p.value <= cutoff)
  
  if (length(indices) == 0) return(c())
  
  lin_reg_2 <- lm(responses ~ covariates[,indices])
  
  return(lin_reg_2$p.value)
}