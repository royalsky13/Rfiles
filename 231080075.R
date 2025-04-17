poissonHorse <- function(y, X, n_iter, beta_start, lambda_start, tau_start) {
  p <- ncol(X)
  n <- nrow(X)
  
  # Precompute squared design matrix to avoid redundant calculations
  X_squared <- X^2
  
  # Initialize storage for parameters
  beta <- matrix(0, nrow = n_iter, ncol = p)
  lambda <- matrix(0, nrow = n_iter, ncol = p)
  tau <- numeric(n_iter)
  
  # Set initial values
  beta_current <- beta_start
  lambda_current <- lambda_start
  tau_current <- tau_start
  
  # Store initial values
  beta[1, ] <- beta_current
  lambda[1, ] <- lambda_current
  tau[1] <- tau_current
  
  # Initialize auxiliary variables
  a_js <- numeric(p)
  for (j in 1:p) {
    a_js[j] <- 1 / rgamma(1, shape = 1, rate = 1 + 1 / lambda_current[j]^2)
  }
  b <- 1 / rgamma(1, shape = 1, rate = 1 + 1 / tau_current^2)
  
  # Precompute initial eta and exp(eta)
  eta_current <- X %*% beta_current
  exp_eta_current <- exp(eta_current)  # Maintain exp(eta) to avoid recomputing
  
  for (t in 2:n_iter) {
    # Update beta using Metropolis-Hastings with optimized calculations
    for (j in 1:p) {
      xj <- X[, j]
      xj_squared <- X_squared[, j]
      
      # Compute proposal variance using precomputed X_squared and maintained exp_eta_current
      sum_xj2_exp_eta <- sum(xj_squared * exp_eta_current)
      prior_precision_j <- 1 / (tau_current^2 * lambda_current[j]^2)
      proposal_var_j <- 1 / (sum_xj2_exp_eta + prior_precision_j)
      proposal_sd_j <- sqrt(proposal_var_j)
      
      # Propose new beta_j
      beta_j_star <- rnorm(1, mean = beta_current[j], sd = proposal_sd_j)
      delta_j <- beta_j_star - beta_current[j]
      xj_delta <- xj * delta_j
      
      # Efficiently compute exp(eta_proposed) using current exp(eta)
      exp_xj_delta <- exp(xj_delta)
      exp_eta_proposed <- exp_eta_current * exp_xj_delta
      
      # Calculate log acceptance ratio
      log_lik_diff <- sum(y * xj_delta) - sum(exp_eta_proposed - exp_eta_current)
      log_prior_diff <- dnorm(beta_j_star, 0, tau_current * lambda_current[j], log = TRUE) -
        dnorm(beta_current[j], 0, tau_current * lambda_current[j], log = TRUE)
      log_alpha <- log_lik_diff + log_prior_diff
      
      # Accept/reject proposal
      if (log(runif(1)) < log_alpha) {
        beta_current[j] <- beta_j_star
        eta_current <- eta_current + xj_delta  # Update eta incrementally
        exp_eta_current <- exp_eta_proposed    # Update exp(eta) incrementally
      }
    }
    
    # Update lambda parameters
    for (j in 1:p) {
      a_js[j] <- 1 / rgamma(1, shape = 1, rate = 1 + 1 / lambda_current[j]^2)
      rate_lambda <- (beta_current[j]^2) / (2 * tau_current^2) + 1 / a_js[j]
      lambda_j_sq <- 1 / rgamma(1, shape = 1, rate = rate_lambda)
      lambda_current[j] <- sqrt(lambda_j_sq)
    }
    
    # Update tau parameter
    b <- 1 / rgamma(1, shape = 1, rate = 1 + 1 / tau_current^2)
    sum_beta_lambda <- sum((beta_current^2) / (2 * lambda_current^2))
    rate_tau <- sum_beta_lambda + 1 / b
    tau_sq <- 1 / rgamma(1, shape = (p + 1) / 2, rate = rate_tau)
    tau_current <- sqrt(tau_sq)
    
    # Store current values
    beta[t, ] <- beta_current
    lambda[t, ] <- lambda_current
    tau[t] <- tau_current
  }
  samples <- list("beta" = beta, "lambda" = lambda, "tau" = tau)
  return(samples)
}