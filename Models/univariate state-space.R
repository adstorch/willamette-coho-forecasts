# univariate state-space model --------------------------------------------
# hatchery proportion model
## likelihood
cat('
    model {
X[1] ~ dnorm(X0 + u, inv.q);
EY[1] <- X[1];
Y[1] ~ dnorm(EY[1], inv.r);
for(m in 2:nHWobs) {
  X[m] ~ dnorm(X[m-1] + u, inv.q);
  EY[m] <- X[m];
  Y[m] ~ dnorm(EY[m], inv.r); 
}',
    file={willCohoUSP.mod <- tempfile()})