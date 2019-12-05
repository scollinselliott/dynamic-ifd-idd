zmfit <- function(rank_, size_) {
dat.kappa <- data.frame(kappa = rank_, size = size_)

q.hats <- c()
beta.hats <- c()
ss.hats <- c()

for (bx in 1:200) {

q.mc <- c()
res.sq.mc <- c()

for (b in 1:200) {
q.b <- runif(1, 0, 500)
q.mc <- append(q.mc, q.b) 
res.sq.b <- sum( lm(log(dat.kappa$size) ~ log(dat.kappa$kappa + q.b))$residuals^2)
res.sq.mc <- append(res.sq.mc, res.sq.b)
}

res.q.mc.dat <- data.frame(q.mc,res.sq.mc)
q.hat <- res.q.mc.dat[which(res.q.mc.dat$res.sq.mc == min(res.q.mc.dat$res.sq.mc) ),]$q.mc
beta.hat <- lm(log(dat.kappa$size) ~ log(dat.kappa$kappa + q.hat))$coefficients[2]
ss.hat <- sum( lm(log(dat.kappa$size) ~ log(dat.kappa$kappa + q.hat))$residuals^2)

q.hats  <- append(q.hats, q.hat)
beta.hats <- append(beta.hats, beta.hat)
ss.hats <- append(ss.hats, ss.hat)

qbeta.dat <- data.frame(q.hats,beta.hats)

}

fit <- lm( log(dat.kappa$size) ~ log(dat.kappa$kappa + mean(q.hats) ) )

q <- signif(mean(q.hats), 4)
z <- signif(fit$coef[[2]], 4) * (-1)

qzlist <- list("q" = q, "z" = z)

return(qzlist)
}

