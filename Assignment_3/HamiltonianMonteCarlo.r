## Major portions of this code are derived from the Radford Neal Paper (Page 14). Although, a few changes like adding animation have been incorporated
## to make the debugging easier.
require(animation)

HMC = function (U, grad_U, epsilon, L, current_q, anim = FALSE)
{
  q = current_q
  p = rnorm(length(q),0,1) # independent standard normal variates
  current_p = p
  
  ## Make a half step for momentum at the beginning
  p = p - epsilon * grad_U(q) / 2
  
  # make a list for animation
  p_l = p
  q_l = q
  
  ## Alternate full steps for position and momentum
  for(i in 1:(L-1))
  {
    ## Make a full step for the position
    q = q + epsilon * p
    
    ## Make a full step for the momentum, except at end of trajectory
    p = p - epsilon * grad_U(q)
    
    if(anim)
    {
      H = function(x, y) { U(x) + 0.5 * y^2 }
      
      t = seq(-2, 2, 0.01)
      Hvals = outer(t, t, H)
      contour(t, t, Hvals, levels=seq(0, 3, 0.1), asp=1,
              drawlabel=FALSE, xlab="q", ylab="p")
      lines(q_l, p_l, lwd=2, col='red')
      ani.record()
      p_l = c(p_l, p)
      q_l = c(q_l, q)
    }
  }
  
  #q = q + epsilon * p
  
  ## Make a half step for momentum at the end.
  p = p - epsilon * grad_U(q) / 2
  
  ## Negate momentum at end of trajectory to make the proposal symmetric
  p = -p
  
  ## Evaluate potential and kinetic energies at start and end of trajectory
  current_U = U(current_q)
  current_K = sum(current_p^2) / 2
  proposed_U = U(q)
  proposed_K = sum(p^2) / 2
  
  ## Accept or reject the state at end of trajectory, returning either
  ## the position at the end of the trajectory or the initial position
  if(current_U + current_K > proposed_U + proposed_K)
    return(q) # reject
  else if (runif(1) < exp(current_U-proposed_U+current_K-proposed_K))
    return (q) # accept
  else
    return (current_q) # reject
}

#pdf from the Metropolis Hasting Algorithm. It is derived from the MetropolisHastings.r code on the class website
f = function(x)
{
  exp(-x^8 + 4 * x^4 - 3 * x^2)
}



