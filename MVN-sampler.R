#Draws from N(mu, Sigma)

set.seed(1)
par(mfrow = c(2,2))

#function to simulate from multivariate normal
simulate.mvn <- function(mu, Sigma, n = 1000)
{
  #Eigenvalue decomposition
  decomp <- eigen(Sigma)
  
  #Square root of the Sigma matrix
  Sig.sq <- decomp$vectors %*% diag(decomp$values^(1/2)) %*% solve(decomp$vectors)
  
  #Sampling from N(0,1)
  samples.mvn <- matrix(0, nrow = n, ncol = 2)
  for(i in 1:n)
  {
    Z <- rnorm(2)
    samples.mvn[i,] <- mu + Sig.sq %*% Z
  }
  return(samples.mvn)
    
}

#Mean (-2,5), Variance (1,1), Correlation = 0.5
mu <- c(-2,5)
Sigma <- matrix(c(1,0.5,0.5, 1),nrow = 2, ncol = 2)
samp <- simulate.mvn(mu = mu, Sigma = Sigma)
par(mfrow = c(1,3))
plot(samp, asp = 1, main = "Correlation = 0.5", xlab = "x_1", ylab = "x_2")
plot(density(samp[,1]), main = "Marginal density of X1")
plot(density(samp[,2]), main = "Marginal density of X2")


###
par(mfrow = c(2,2))
plot(samp, asp = 1, main = "Correlation = 0.5", xlab = "x_1", ylab = "x_2")


#Mean (-2,5), Variance (1,1), Correlation = 0.99
mu <- c(-2,5)
Sigma <- matrix(c(1,0.99,0.99, 1),nrow = 2, ncol = 2)
samp <- simulate.mvn(mu = mu, Sigma = Sigma)
plot(samp, asp = 1, main = "Correlation = 0.99", xlab = "x_1", ylab = "x_2")

#Mean (-2,5), Variance (1,1), Correlation = 0.8
mu <- c(-2,5)
Sigma <- matrix(c(1,0.8,0.8, 1),nrow = 2, ncol = 2)
samp <- simulate.mvn(mu = mu, Sigma = Sigma)
plot(samp, asp = 1, main = "Correlation = 0.8", xlab = "x_1", ylab = "x_2")


#Mean (-2,5), Variance (1,1), Correlation = 0
mu <- c(-2,5)
Sigma <- matrix(c(1,0,0, 1),nrow = 2, ncol = 2)
samp <- simulate.mvn(mu = mu, Sigma = Sigma)
plot(samp, asp = 1, main = "Correlation = 0", xlab = "x_1", ylab = "x_2")

