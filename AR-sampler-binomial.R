#Accept/Reject Sampler to draw from Binomial(n,p)

#set the seed 
set.seed(123) #for reproducibility

#function to simulate from Bin(n,p)
simulate_bin <- function(n,p)
{
  accept <- 0
  
  x <- 0:n
  #choosing constant c
  ratio.target.proposal <-  choose(n,x) * p^{x-1} * (1-p)^{n-2*x}
  c = max(ratio.target.proposal) + 0.001
  
  while(accept == 0)
  {
    U <- runif(1,0,1) #generating a random number from U(0,1)
    proposal.value <- rgeom(1, prob = p) #generating from proposal distribution which in this case is geometric(p)
    ratio <- dbinom(x=proposal.value, size = n, prob=p)/(c*dgeom(x = proposal.value, prob = p))
    if(U<ratio)
    {
      accept <- 1
      random.target <- proposal.value
    }
  }
  return(random.target)
}

#one sample
#call the above function
simulate_bin(10,0.25)

#let us  simulate 1000 numbers from the distribution
x.samples.target <- vector("numeric", length = 1000) 
for(t in 1:1000)
{
  x.samples.target[t] <- simulate_bin(10,0.25)
}

#a quick check
mean(x.samples.target)
#does it match with the xpected value?
#expected value: n*p