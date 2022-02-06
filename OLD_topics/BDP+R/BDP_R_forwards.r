# Exact simulation of continuous-time birth-and-death process with interaction term

# Parameters l:birth, m:death, c:coupling, z0:first generation, it:iterations
# Returns array with times of events and array of events (0 death, 1 birth, 2 recombination)
simulate_bdpr <- function(z0,l,m,c,it) {
  times <- c(rep(NA,it))
  events <- c(rep(NA,it))
  z <- z0 # current number of lineages
  dummy <- 0
  i <- 1
  while(i < it+1) {
    rate <- l+m+c*(z-1)
    if(rate == 0) {
      times[i] <- infty
      dummy <- 1
    }
    else {
      if(i == 1) {
        times[i] <- rexp(1,1/rate)
      }
      else {
        times[i] <- times[i-1] + rexp(1,1/rate)
      }
     
    }
    if(dummy == 0) {
      rand <- runif(1,0,1)
      if(rand*rate < l) {
        z <- z+1
        events[i] <- 1
      } else if (l <= rand*rate && rand*rate < l+m) {
        z <- z-1
        events[i] <- 0
      } else {
        z <- z-1
        events[i] <- 2
      }
      i <- i+1
    }
    else {
      i <- it
    }
  }
  return(list(times,events))
}

output <- simulate_bdpr(1,0.2,0.1,0.1,5)

# To get a tree now we should compute which lineages are involved in each event (uniformly at random)
# Somehow then draw the tree itself
