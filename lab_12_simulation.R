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

run_simulation <- function(n_trials, n, p, cutoff){
  library(ggplot2)
  for (i in 1:length(n)){
    data <- generate_data(n[i], p[i])
    p_values <- model_select(data[[1]], data[[2]], cutoff)
    plot(ggplot(data = as.data.frame(list(p_values = p_values)),
                aes(x = p_values)) +
           geom_hist() + 
           labs(x = "P-Values", y = "Count"))
  }
}

run_simulation(n_trials <- 1000, n <- c(100,1000, 10000),
               p <- c(10, 20, 50), cutoff <- 0.05)