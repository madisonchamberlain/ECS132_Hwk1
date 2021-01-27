
simvir <- function(c,r,m,n,nreps) {
  
  counter <- 0
  # Simulating the reps 
  for(reps in 1:nreps) {
    # Creating the infected and non-infected 
    infected <- 1 
    noninfected <- (c - 1)
    
    # Simulating the epochs 
    for (epoch in 1:m) {
      # Temporary storing variables 
      k <- infected
      s <- noninfected 
      # Simulating all the computers  
      for (computer in 1:s) {
        if (runif(1) > ((r)^k) ) {
          noninfected <- noninfected - 1 
          infected <- infected + 1 
        }
      }
    }
    if(noninfected == n) counter <- counter + 1 
  }
  return (counter / nreps)
}