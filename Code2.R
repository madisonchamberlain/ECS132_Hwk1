
pnk <- function(d,s,k) {
  # Base Cases 
  if (d<=0 || k<=0) return (0)
  if (k > d) return (0)
  if (k==1 && d==1) return (1)
  if (d > k*s) return (0)
  
  # Recursion 
  sum <- 0
  for (r in 1:s) {
    sum <- sum + pnk(d-r, s, k-1)
  }
  return (sum / s)
}