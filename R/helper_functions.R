
# -------------------------------------------------------------------------------
# ------- helper functions for derivations and model matrix calculations --------
# -------------------------------------------------------------------------------


# calculate analytic autocovariance matrix
get_an_ac <- function(tmat, pivec, Lambda, zstate = 5){
  
  # Pi_s <- pimat[-zstate,-zstate]
  # P_s <- tmat[-zstate,-zstate]
  # D <- diag(diag(Pi_s))
  # 
  # Lambda%*%(D%*%(P_s - Pi_s))%*%t(Lambda)
  pivec <- pivec[-zstate]
  Pi_s <- matrix(pivec, length(pivec), length(pivec), byrow = T) 
  P_s <- tmat[-zstate,-zstate]
  D <- diag(diag(Pi_s))
  
  Lambda%*%(D%*%(P_s - Pi_s))%*%t(Lambda)
  
}

# calculate analytic stationary/contemporaneous covariance
get_t0_cov <- function(pivec, Lambda, zstate = 5, Theta){
  
  # # s <- ncol(pimat) -1 
  # Pi_s <- pimat[-zstate,-zstate]
  # D <- diag(diag(Pi_s))
  # Id <- diag(ncol(D))
  # 
  # Lambda%*%(D%*%(Id - Pi_s))%*%t(Lambda) + theta
  pivec <- pivec[-zstate]
  Lambda%*%(diag(pivec) - pivec%*%t(pivec))%*%t(Lambda) + Theta
  
}

# calculate model-implied lagged regression coefficients 
get_lagged <- function(Gamma, Sigma){
  t(Gamma)%*%solve(Sigma)
  # s <- ncol(pimat) -1 
  # 
  # Pi_s <- pimat[-zstate,-zstate]
  # P_s <- tmat[-zstate,-zstate]
  # D <- diag(diag(Pi_s))
  # Id <- diag(s)
  # 
  # Lambda%*%(D%*%(P_s - Pi_s))%*%(solve(Id - Pi_s))%*%solve(D)%*%solve(Lambda)
}

# calculate model-implied residual covariance matrix
get_rcov <- function(Phi, Sigma){
  Omega <- Sigma - Phi%*%Sigma%*%t(Phi)
}


# calculate steady state matrix of the situations
steadystate <- function(P){
  # we use the fact that:
  # \pi(P - I) = 0 (where pi is a 1 x s row vector)
  # thus, t(P - I)t(\pi) = 0
  # and 
  # \sum(\pi) = 1
  
  # number of states
  s = nrow(P)
  
  A <- rbind(t(P - diag(s)), rep(1,s))
  b <- c(rep(0,s),1)
  
  qr.solve(A, b)
  
}


