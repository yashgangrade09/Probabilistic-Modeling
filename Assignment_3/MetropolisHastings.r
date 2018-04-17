## A crazy (unnormalized) pdf that we want to sample from
f = function(x)
{
  exp(-x^8 + 4 * x^4 - 3 * x^2)
}

t = seq(-2, 2, 0.01)
plot(t, f(t), type='l', lwd = 3)

## Random walk Metropolis algorithm to sample from f (using Gaussian proposal density)
metropolis = function(n, f, sigma)
{
  w = rnorm(n, 0, sigma)
  x = numeric(n)
  x[1] = w[1]

  acc = 0
  for(i in 2:n)
  {
    u = runif(1)
    y = x[i - 1] + w[i]

    if(f(y) >= f(x[i - 1]))
      a = 1
    else
      a = f(y) / f(x[i - 1])

    if(u < a)
    {
      x[i] = y
      acc = acc + 1
    }
    else
      x[i] = x[i - 1]
  }

  cat(paste("Acceptance rate =", acc / n, "\n"))

  return(x)
}

## Rerun this code with sigma = 0.1, 1, and 10
## See how it trades off between exploration and acceptance rate
sigma = 1
b = 2000 ## this is the burn-in
n = 10000 ## this is the number of MCMC samples

k = 4 ## number of independent chains to run

## MCMC sampling
x = metropolis(n, f, sigma)
## Discard burn-in
x = x[b:n]
plot(x[1:500], type = 'l', main = paste("Metropolis Samples, sigma =", sigma))


## Approximate the normalizing constant (for purposes of plotting the pdf)
C = sum(f(t)) * 0.01

hist(x, freq=FALSE, breaks=40)
t = seq(-2, 2, 0.01)
lines(t, f(t) / C, lwd = 3, col = 'red')

X = matrix(0, n, k)
for(i in 1:k)
  X[,i] = metropolis(n, f, sigma)

psi = apply(X, MARGIN = 2, FUN = cumsum)
for(i in 1:k)
  psi[,i] = psi[,i] / (1:nrow(psi))


## Plot the means
plot(psi[,1], type='l', ylim=c(-1,1))
for(i in 2:k)
  lines(psi[,i], type='l')

## Histogram of randomly selected sample means (should be roughly Gaussian)
mu = numeric(1000)
for(i in 1:1000)
  mu[i] = mean(sample(X[b:n,], 1000))
hist(mu, freq=FALSE)
m = mean(mu)
s = sd(mu)
t = seq(m-3*s, m+3*s, s*0.01)
lines(t, dnorm(t, mean(mu), s), col='red', lwd=3)


## Gelman-Rubin convergence test
Gelman.Rubin = function(psi)
{
  n = nrow(psi)
  psi.means = colMeans(psi)
  B = n * var(psi.means)
  W = mean(apply(psi, MARGIN = 1, FUN = var))

  return((W * (n - 1) / n + (B / n)) / W)
}

## Plot the Gelman-Rubin R as a function of MCMC iteration
start = b
end = n
steps = 100
ind = seq(b, n, by = (n - b) / steps)
r.hat = numeric(length(ind))
for(i in 1:length(ind))
  r.hat[i] = Gelman.Rubin(psi[1:ind[i],])

plot(ind, r.hat, type='l', main = "Gelman-Rubin Plot")
