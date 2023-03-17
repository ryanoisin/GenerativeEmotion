# Function to create model matrices for the generative model

model_matrices <- function(
    s = 5, # number of "situations"
    p = 6, # number of "emotions"
    stay_prob = .6, # staying probability, uniform for all situations
    switch_prob = .4, # switching probabilitity, equal for all transitions
    means = rbind(rep(0,p), # matrix of means for each emotion (columns) in each situation (rows)
                  c(1, .5, 1,  0  ,  0  ,  0  ),
                  c(.5, 1, .5,  0  ,  0  ,  0  ),
                  c( 0  ,  0  ,  0  , 1, .5, 1),
                  c( 0  ,  0  ,  0  , .5, 1, .5)), 
    theta = sqrt(.25), # variance of emotios
    tmat = NULL # custom transition matrix, optional
){
  
  # detect neutral state
  zstate <- which(apply(means,1,function(r) all(r==0)))
  
  #  create factor loadings
  Lambda <- t(means[-zstate,])
  
  # transition matrix
  if(is.null(tmat)){
    tmat <- matrix(NA, nrow = s, ncol = s)
    diag(tmat) <- stay_prob
    tmat[upper.tri(tmat)] <- tmat[lower.tri(tmat)] <- switch_prob
    tmat
  }
  # else{
  #   warning("tmat specified, ignoring stay_prob and switch_prob")
  # }
  
  # get steady state
  Pi <- steadystate(tmat)
  
  # get measurement error cov mat
  if(!is.matrix(theta)){
    Theta <- diag(rep(theta),p)
  } else {Theta <- theta }
  
  # Get model-implied matrices
  # 1: time-0 covariance matrix
  Sigma <- get_t0_cov(Pi, Lambda, zstate = zstate, Theta = Theta)
  
  # 2: lag - 1 auto-covariance
  Gamma <- get_an_ac(tmat, Pi, Lambda, zstate)
  
  # 3: cross-lagged matrix
  Phi <- get_lagged(Gamma, Sigma)
  
  # 4: residual covariance matrix
  Omega <- get_rcov(Phi, Sigma)
  
  # 5: residual partial correlations
  Omega_pc <- -stats::cov2cor(solve(Omega))
  diag(Omega_pc) <- 0
  Omega_pc <- as.matrix(Matrix::forceSymmetric(Omega_pc))
  
  # create output
  list("s" = s,
       "p" = p,
       "zstate" = zstate,
       "Lambda" = Lambda,
       "tmat" = tmat,
       "Pi" = Pi,
       "Theta" = Theta,
       "Sigma" = Sigma,
       "Gamma" = Gamma,
       "Phi" = Phi,
       "Omega" = Omega,
       "Omega_pc" = Omega_pc)
}
