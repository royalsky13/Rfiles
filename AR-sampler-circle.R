set.seed(123)

simulate.cirle <- function()
{
  accept <- 0
  counter <- 0
  
  while(accept == 0)
  {
    counter = counter + 1
    proposal.point <- runif(2, min = -1, max = 1)
    if(proposal.point[1]^2 + proposal.point[2]^2 < 1)
    {
      accept <- 1
      return(c(proposal.point, counter))
    }
  }
}

#generate n draws from the circle 
n = 10000
sample <- matrix(0,nrow = n, ncol = 2)
counts <- vector("numeric", length = n)

for(i in 1:n)
{
  dummy <- simulate.cirle()
  sample[i,] <- dummy[1:2]
  counts[i] <- dummy[3]
}

#visalisation check
plot(sample[,1], sample[,2], xlab = "x", ylab = "y", xlim = c(-1,1), ylim = c(-1,1), main = "Uniform samples from the circle", asp = 1)


#check for c
c = 4/pi
c
mean(counts)