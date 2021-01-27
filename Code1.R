roll <- function() return(sample(1:6,1))

simjj <- function(d,nreps) {
  # Creating all the vectors 
	w <- vector(length=nreps)
	p <- vector(length=nreps)
	lD <- vector(length=nreps)
	for (rep in 1:nreps) {
	  # Initializing the total number of dots 
		dots1 <- 0
		dots2 <- 0
		# Simulating the game 
		while(dots1 < d && dots2 < d) {
			dots1 <- dots1 + roll()
			if(dots1 < d) {
				dots2 <- dots2 + roll()
			}
		}
		# Checking the winner 
		if(dots1 >= d) {
			w[rep] <- 'Jill'
			p[rep] <- dots1
			lD[rep] <- dots2
		} else {
			w[rep] <- 'Jack'
			p[rep] <- dots2
			lD[rep] <- dots1
		}
	}
	return (list(winner = w, prize = p, loserDots = lD))
}




