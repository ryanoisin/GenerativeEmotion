
# Function to generate data from the generative model.
# Takes as input a list of model matrices, as created by model_matrices()
# 

datagen <- function(mm, # list of model matrices, as created by model_matrices()
                    n, # number of time points to simulate
                    sout = FALSE # output situations (TRUE) as well as emotions or not (FALSE)
                    ){
  # storage for states
  st <- rep(NA,n)
  # storage indicator variables
  I <- matrix(0,nrow = n, ncol = mm$s)
  # draw first observation from stationary distribution
  st[1] <- which(stats::rmultinom(1, size = 1, prob = mm$Pi) == 1)
  
  
  # draw all states
  for(i in 2:n) {
    # which row of the matrix do we draw probabilities from?
    prob = mm$tmat[st[i-1],]
    # draw a state
    st[i] <- which(stats::rmultinom(1, size = 1, prob = prob) ==1)
    # write an indicator variable
    I[i,st[i]] <- 1
  }
  # Drop reference state from indicator matrix
  I <- I[,-mm$zstate]
  
  # Generate emissions (gaussian)
  data <- matrix(NA, nrow = n, ncol = mm$p, dimnames = list(NULL, paste0("X", 1:mm$p)))
  for(i in 1:n){
    data[i,] <- MASS::mvrnorm(n = 1, mu = (mm$Lambda)%*%I[i,], Sigma = mm$Theta)
  }
  #if(sout) return(cbind(data,s,I)) else return(data)
  if(sout) return(cbind(data,st)) else return(data)
}
