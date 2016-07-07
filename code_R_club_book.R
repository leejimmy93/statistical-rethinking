# R code 2.1
ways <- c(0, 3, 8, 9, 0)
ways/sum(ways)
fomula(sum)

# R code 2.2
temp<- dbinom(6, size=9, prob = 0.5)
temp
help(dbinom)

# R code 2.3
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 100) 
# p_grid <- seq(from = 0, to = 1, by=0.2)
p_grid
# define prior
prior <- rep(2, 100)
prior <- ifelse(p_grid < 0.5, 1, 2) # does 0 & 1 matter or not? 
# prior <- exp(-5*abs(p_grid - 0.5))
help("ifelse")
# log(exp(3))
prior

# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)
likelihood
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# R code 2.4, display the posterior distribution
plot(p_grid, posterior, type = "b", 
     xlab="probability of water", ylab="posterior probabilty")
mtext("20 points")
help(plot)

# R code 2.5: replicate the different prior in the previous code

# R code 0.5
install.packages(c("coda","mvtnorm","devtools"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")

# R code 2.6 
library(rethinking)
globe.qa <- map(
  alist(
    w ~ dbinom(9,p), # binormial likelihood
    p ~ dunif(0,1)   # uniform prior
  ),
  data = list(w=6)
)
# display summary of quandratic approximation
precis(globe.qa)

help("~")
help("dbeta")
data = list(w=6)
data

# R code 2.7
# analytical calculation
w <- 6
n <- 9 
curve(dbeta(x, w+1, n-w+1), from = 0, to = 1)
# quandratic approximation
curve(dnorm(x, 0.67, 0.16), lty=2, add=TRUE)

# 2M1
# R code 2.3
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)
p_grid
# define prior
prior <- rep(1, 20)
prior
# compute likelihood at each value in grid
likelihood <- dbinom(5, size = 7, prob = p_grid)
likelihood
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# R code 2.4, display the posterior distribution
plot(p_grid, posterior, type = "b", 
     xlab="probability of water", ylab="posterior probabilty")
mtext("3 points")
help(plot)

# 2M2
# R code 2.3
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)
p_grid
# define prior
prior <- ifelse(p_grid<0.5, 0, 1)
prior
# compute likelihood at each value in grid
likelihood <- dbinom(5, size = 7, prob = p_grid)
likelihood
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# display the posterior distribution
plot(p_grid, posterior, type = "b", 
     xlab="probability of water", ylab="posterior probabilty")
mtext("3 points")
help("ifelse")

# 2H3
# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)
p_grid
# define prior
prior <- ifelse(p_grid<0.1, 0.5, 0)
prior
# compute likelihood at each value in grid
likelihood <- dbinom(1, size = 1, prob = p_grid)
likelihood
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# display the posterior distribution
plot(p_grid, posterior, type = "b", 
     xlab="probability of species A", ylab="posterior probabilty")
mtext("2 points")

# 02/25/2016
# R code 3.1
PrPV <- 0.95 # Pr(vam|positive)
PrPM <- 0.01 # Pr(positive|mortal)
PrV <- 0.001 # Pr(vam)
# the Pr of positive
PrP <- PrPV * PrV + PrPM * (1-PrV)  
# calculate the Pr of correctly identify a vampire
(PrVP <- PrPV*PrV / PrP)

# R code 3.2, compute posteror for the globe tossing model 
# in the begining of this chapter
p_grid <- seq(from=0, to=1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
str(posterior)

# R code 3.3
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE) # replace arg means you put back 
# the sample you picked
str(samples)
samples
PI(samples, prob = 0.8) 
HPDI(samples, prob = 0.8)
help("sample")
# R code 3.4
plot(samples)
# R code 3.5
library(rstan)
library(rethinking)
dens(samples) # plot the density 
help("dens")

# R code 3.6
# add up posterior probability where p<0.5 = the posterior probability 
# that the proportion of water is less than 0.5

sum(posterior[p_grid < 0.5])

# R code 3.7, add up all of the samples below 0.5, and also divide the resulting count
# by the total number of samples
sum(samples<0.5)/1e4
# R code 3.8, the posterior probability lies between 0.5 and 0.75
sum(samples > 0.5 & samples<0.75)/1e4 
samples < 0.5

# R code 3.9, the boundary of the lower 80% posterior probability
quantile(samples, 0.8)
# the boundary of the middle 80% posterior probability lies between 10% and 90% quantile
quantile(samples, c(0.1, 0.9))
help("quantile")
quantile(samples, probs = c(0.8))
help("seq")
seq(0,1,0.25)

#R code 3.11, compute posterior probability and draw 1e4 random samples for 
# observing 3 water out of 3 tosses
p_grid <- seq(0,1, length.out = 1000)
prior <- rep(1,1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
samples<- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)

# R code 3.12, the 50% percentile confidence intervel 
PI(samples, prob = 0.5) # don't quite understand this! 
quantile(samples, c(0.25, 0.75))
help(PI) 
# how to check the underlying code for a defined funtion??? 

# practice seq()
seq(0,1,length.out = 10)
help(seq)
seq(from = 0, to = 1, length.out = 10)

# R code 3.13, compute the 50% highest posterior density interval 
HPDI(samples, prob = 0.5)

# R code 3.14, compute the maximum posterior (MAP)
p_grid[which.max(posterior)]
# using samples to approximate the MAP
chainmode(samples, adj=0.01)
help("chainmode")
# R code 3.16, posterior mean or median
mean(samples)
median(samples)

# R code 3.17, suppose p=0.5 is our decision, the expected loss will be 
sum(posterior*abs(0.5 - p_grid))
# R code 3.18, using sapply to repeat calculation, pick every number from p_grid to see 
# which is the best decision. 
loss <- sapply(p_grid, function(d) sum(posterior*abs(d-p_grid))) # pick every p_grid and apply
# some fxn to it, function(d): define the fxn, but this fxn is only used here; the last arg
# applys the function on p_grid, in here it may works like take an item from p_grid, and subtract
# every item in p_grid to see how far this item (d) is away from every item in p_grid. 

help(sapply) # learn more about sapply

# R code 3.19, finding the parameter value that minimize the loss
p_grid[which.min(loss)]

# 03/03/2016
# R code 3.20
dbinom(0:2, size = 2, prob = 0.7)
help("dbinom")
# R code 3.21, a single dummy data observation of w can be sampled with, among two tosses, 
# how many times you can see "w", with the prob of 0.7
rbinom(1, size = 2, prob = 0.7)

#R code 3.22, a set of 10 simulations can be made by 
rbinom(10, size = 2, prob = 0.7)
# R code 3.23, generate 100,000 dummy observations, to verify that each value appears 
# in proportion to its likelihood

dummy_w <- rbinom(1e5, size = 2, prob = 0.7)
table(dummy_w)/1e5
help("table")

# R code 3.24, simulate the same sample as before, 9 tosses
dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
library(rethinking)
simplehist(dummy_w, xlab="dummy water count")

# R code 3.25, to simulate predicted obervations for a single value of p=0.6, and 
# to generate random binomial samples
w <- rbinom(1e4, size = 9, prob = 0.6)
simplehist(w)

# propagate parameter uncertanty into predictions, replace a specific prob with samples
# from posterior
w <- rbinom(1e4, size = 9, prob = samples)
samples
min(samples)
max(samples)
plot(samples)
simplehist(w)

# goal of this analysis
# 1) develop a model that fits the data + prior data: use the likelihood fxn and prior info to model data
# 2) use model to estimate parameters or principles: estimate posterior w/ grid approximation and bayes' equation
#                                                    sample from posterior and ask about probability and max/min parameters
#               why sample instead of using the distribution itself? because it is much easier to estimate from the samples
#               than from the distribution... 
# 3) make predictions and comapre to other situations: use the model and posterior of uncertainty (0.1 to 0.9 figure 3.6 middle)
# to make predictions. 
# Application in real world 
# 1) segregation ratio 
# 2) seed germination 

############ Chapter 4
# R code 4.1
library(rethinking)
pos <- replicate(1000, sum(runif(16, -1, 1))) # suppose each step's length is different
?runif
runif(16, -1, 1)
plot(density(pos))
# 4.2 code
prod(1+runif(12,0,0.1))
# 4.3 code
growth <- replicate(10000, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = T)
# R code 4.4
big <- replicate(10000, prod(1 + runif(12,0,0.5)))
small <- replicate(10000, prod(1 + runif(12, 0, 0.01)))
dens(big, norm.comp = T)
dens(small, norm.comp = T)
# R code 4.5
log.big <- replicate(10000, log(prod(1 + runif(12, 0, 0.5))))
dens(log.big,norm.comp = T)
dnorm(0,0,0.1)
?dnorm
# 4.6
w <- 6; n <- 9;
p_grid <-seq(0,1, length.out = 100)
posterior <- dbinom(w,n,p_grid)*dunif(p_grid,0,1)
posterior <- posterior/sum(posterior)
# R code 4.7
library(rethinking)
data("Howell1")
d <- Howell1
class(d)
d
# 4.8
str(d)
# 4.9
d$height
# 4.10
d2 <-d[d$age>=18,]
str(d2)
dens(d2$height)
# 4.11
curve(dnorm(x, 178, 20), from = 100, to = 250)
?dnorm
# 4.12
curve(dunif(x, 0, 50), -10, 60)
?dunif # the uniform distribution
# 4.13
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)
?rnorm
prior_h
# R code 4.14 ??????????????
mu.list <-seq (140, 160, length.out = 200)
sigma.list <-seq(4, 9, length.out = 200)
post <- expand.grid(mu=mu.list, sigma=sigma.list)
?expand.grid # create a data frame from all combinations of factors
post$LL <- sapply(1:nrow(post), function(i) sum( dnorm(
  d2$height, 
  mean=post$mu[i],
  sd = post$sigma[i],
  log = T))) # LL is what? sum of what??? 
?dnorm
?sum
post$prod <- post$LL + dnorm(post$mu, 178, 20, T) +
  dunif(post$sigma, 0, 50, T) # prod is what? 
post$prob <- exp(post$prod - max(post$prod)) # prob is probability
?exp # logarithms and expoenentials
# 4.15
contour_xyz(post$mu, post$sigma, post$prob)
# 4.16
image_xyz(post$mu, post$sigma, post$prob)
?expand.grid
?exp
?"contour_xyz"
# 4.17
sample.row <- sample(1:nrow(post), size = 1e4, replace = T,
                     prob = post$prob)
sample.mu <- post$mu[sample.row]
sample.sigma <- post$sigma[sample.row]
# 4.18
plot(sample.mu, sample.sigma, cex=2, pch=16, col=col.alpha(rangi2,0.1))
# 4.19
dens(sample.mu)
dens(sample.sigma)
# 4.20
HPDI(sample.mu)
HPDI(sample.sigma)
# 4.21, focus on 20 heights
d3 <- sample(d2$height, size = 20)
# 4.22 ???????????????????????
mu.list <-seq (150, 170, length.out = 200)
sigma.list <-seq(4, 20, length.out = 200)
post2 <- expand.grid(mu=mu.list, sigma=sigma.list)
post2$LL <- sapply(1:nrow(post2), function(i) sum( dnorm(
  d3, 
  mean=post2$mu[i],
  sd = post2$sigma[i],
  log = T)))
post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, T) +
  dunif(post2$sigma, 0, 50, T)
post2$prob <- exp(post2$prod - max(post2$prod))

sample2.row <- sample(1:nrow(post2), size = 1e4, replace = T,
                     prob = post2$prob)
sample2.mu <- post2$mu[sample2.row]
sample2.sigma <- post2$sigma[sample2.row]
plot(sample2.mu, sample2.sigma, cex=2, pch=16, col=col.alpha(rangi2,0.1))
# R code 4.23
dens(sample2.sigma, norm.comp = T) # a long tail on the right
# R code 4.24, load data and select out the adults
library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >=18,]
# R code 4.25, fitting the model with map
flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)
?alist
flist
# R code 4.26
m4.1 <- map(flist, data = d2) 
?map # find max a posterior estimate
# R code 4.27
precis(m4.1)
?precis # display 
# R code 4.28
start <- list(
  mu=mean(d2$height),
  sigma=sd(d2$height)
)
?list # alist VS. list
# R code 4.29, a more informative prior for mu, change sd to 0.1, and build the formula 
# into the call map
m4.2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~ dunif(0, 50)
  ),
  data = d2)
precis(m4.2)
# R code 4.30
vcov(m4.1) # variance-covariance matrix ??????????????????
?"vcov" # calculate variance-covariance matrix for a fitted model object
# R code 4.31
diag(vcov(m4.1))
?diag # construct a diagonal matrix
cov2cor(vcov(m4.1))
?cov2cor # scales a covariance matrix into the correpsonding correlation matrix
sqrt(diag(vcov(m4.1)))
precis(m4.1)
# R code 4.32, sample from multi-dimentional posterior
library(rethinking)
post <- extract.samples(m4.1, n = 1e4)
head(post)
mean(post$mu)
mean(post$sigma)
# R code 4.33
precis(post)
plot(post)
# R code 4.34
library(MASS)
post <- mvrnorm(n=1e4, mu = coef(m4.1), Sigma = vcov(m4.1))
?mvrnorm
# R code 4.35
m4.1_logsigma <- map(
  alist(
    height ~ dnorm(mu, exp(log_sigma)),
    mu ~ dnorm(178, 20),
    log_sigma ~ dnorm(2,10)
  ), data = d2)
# R code 4.36
post <- extract.samples(m4.1_logsigma)
sigma <- exp(post$log_sigma)

######## for 03/24/2016 ###########################
library(rethinking)
data("Howell1")

# R code 4.37
plot(d2$height ~ d2$weight)

# R code 4.38
# load data again
d <- Howell1
d2 <- d[d$age >=18, ]

# fit model, question, where is start list???????
m4.3 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(156, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), 
data=d2)
?alist # make a collection of arbitrary R objects. alist does not evaluates the code embeded in
# it. so when define a list of fomulas, should use alist, so the code isn't executed.  

# R code 4.39
m4.3 <- map(
  alist(
    height ~ dnorm(a + b*weight, sigma),
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), 
  data=d2)

# R code 4.40
precis(m4.3)

# R code 4.41
precis(m4.3, corr = T)

# R code 4.42, centering... 
d2$weight.c <- d2$weight - mean(d2$weight)
mean(d2$weight.c)

# R code 4.43
m4.4 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight.c, 
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), 
  data=d2)

# R code 4.44
precis(m4.4, corr = T)

# R code 4.45
plot(height ~ weight, data=d2)
abline(a=coef(m4.3)["a"], b = coef(m4.3)["b"]) ### understand coef... 

# R code 4.46
post <- extract.samples(m4.3)

# R code 4.47
post[1:5,]

# R code 4.48, start with some of the data only, so you will see how adding in more 
# data changes the scatter of the lines, begin with just the first 10 cases in d2
N <- 352
dN <- d2[1:N,] # d <- Howell1; d2 <- d[d$age >=18, ]
mN <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10), 
    sigma ~ dunif(0, 50)
  ), data = dN)
?map # find mode of posterior distribution for arbitrary fixed effet models

# R code 4.49
# extract 20 samples from the posterior
post <- extract.samples(mN, n=20)
?extract.samples

# display raw data and sample size
plot(dN$weight, dN$height,
     xlim = range(d2$weight), ylim = range(d2$height),
     col=rangi2, xlab="weight", ylab="height")
mtext(concat("N= ", N))

# plot the lines, with transparency
for (i in 1:20)
  abline(a=post$a[i], b=post$b[i], col=col.alpha("black", 0.3)) # understand this... 

# R code 4.50
post <- extract.samples(m4.3, n=1e4) # extract 1e4 samples
mu_at_50 <- post$a + post$b *50 
mu_at_50

# R code 4.51
dens(mu_at_50, col=rangi2, lwd=2, xlab="mu|weight=50")

# R code 4.52
HPDI(mu_at_50, prob = 0.89)

# R code 4.53, understand link!!!!!!!!!!!!!!!!! #################
####################### problem start from here##################
precis(m4.3)
mu <- link(m4.3)
str(mu) # problem!!!!!!!!!! doesn't look right

# R code 4.54
# define sequence of weights to compute predictions for 
# these values will be on the horizontal axis
weight.seq <- seq(25, 70, by=1)

# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link(m4.3, data = data.frame(weight=weight.seq))
str(mu)

# R code 4.55
# use type="n" to hide raw data

plot(height ~ weight, d2, type="n")

# loop over samples and plot each mu value
for (i in 1:100)
  points(weight.seq, mu[i,], pch=16, col=col.alpha(rangi2, 0.1))

# R code 4.56
# summarize the distributon of mu
mu.mean <- apply(mu, 2, mean)
mu.mean
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)

# R code 4.57
# plot raw data
# fading out points to make line and interval more visible
plot(height ~ weight, data=d2, col=col.alpha(rangi2, 0.5))

# plot the MAP line, aka the man mu for each weight
lines(weight.seq, mu.mean)

# plot a shaded region for 89% HDPI
shade(mu.HPDI, weight.seq)

# R code 4.58
post <- extract.samples(m4.3)
?extract.samples
mu.link <- function(weight) post$a + post$b*weight
weight.seq <- seq(25, 70, by=1)
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)

plot(height ~ weight, data=d2, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)

# prediction intervals
# R code 4.59
sim.height <- sim(m4.3, data = list(weight=weight.seq)) # m4.3 is the linear model
str(sim.height) 
?sim # difference between R4.59 & R4.58

# R code 4.60
height.PI <- apply(sim.height, 2, PI, prob=0.89)

# R code 4.61, plot
# plot raw data
plot(height~weight, d2, col=col.alpha(rangi2, 0.5))

# draw MAP line
lines(weight.seq, mu.mean)

# draw PI region for simulated heights
shade(height.PI, weight.seq)

# R code 4.62
sim.height <- sim(m4.3, data = list(weight=weight.seq), n = 1e4)
height.PI <- apply(sim.height, 2, PI, prob=0.89)

# plot raw data
plot(height~weight, d2, col=col.alpha(rangi2, 0.5))

# draw MAP line
lines(weight.seq, mu.mean)

# draw PI region for simulated heights
shade(height.PI, weight.seq)

# R code 4.63, how sim works
post <- extract.samples(m4.3)
weight.seq <- 25:70
sim.height <- sapply(weight.seq, function(weight)
  rnorm(
    n=nrow(post),
    mean = post$a+post$b*weight,
    sd=post$sigma))
height.PI <- apply(sim.height, 2, PI, prob=0.89)
?rnorm

############## for 04/04/2016 ###################################
# R code 4.64
library(rethinking)
data("Howell1")
d <- Howell1
str(d)
plot(d$height, d$weight)

# R code 4.65
d$weight.s <- (d$weight - mean(d$weight))/sd(d$weight)
plot(d$height, d$weight.s)

# R code 4.66
d$weight.s2 <- d$weight.s^2
m4.5 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight.s + b2*weight.s2,
    a~ dnorm(178, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10), 
    sigma ~ dunif(0, 50)
  ),
data = d)

# R code 4.67
precis(m4.5)

# R code 4.68
# calculate the mean relationship and the 89% interval 
# of the mean and the prediction
weight.seq <-seq(-2.2, 2, length.out = 30)
pred_dat <- list(weight.s = weight.seq, weight.s2=weight.seq^2)
mu <- link(m4.5, data = pred_dat) 
# take map model fit, sample from posterior distribution, and then compute 
# mu for each case in the data and sample from the posterior distribution
?link
dim(mu)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)
sim.height <- sim(m4.5, data = pred_dat)
?sim
sim(m4.5, data = pred_dat)
height.PI <- apply(sim.height, 2, PI, prob=0.89)

# R code 4.69, plot
plot(height ~ weight.s, d, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

# R code 4.70
# fit the model with a slight modification of the parabolic model's code
d$weight.s3 <- d$weight.s^3
m4.6 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight.s +b2 *weight.s2 + b3*weight.s3,
    a ~dnorm(178, 100),
    b1 ~ dnorm(0,10),
    b2 ~ dnorm(0,10),
    b3 ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ),
data = d)

# R code 4.71
# plot the estimates on the original scale
plot(height ~ weight.s, d, col=col.alpha(rangi2, 0.5), xaxt="n")

# R code 4.72
at <- c(-2, -1, 0, 1,2 )
labels <- at*sd(d$weight) + mean(d$weight)
axis(side = 1, at = at, labels = round(labels, 1))

### notes from R club
# equation R markdown for math symbols
# table function in R make a table from the code 

############### For 04-11-2016 ################################

# R code 5.1
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

# standardize predictor
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)
# fit model
m5.1 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bA * MedianAgeMarriage.s,
    a ~ dnorm(10, 10),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d)

# R code 5.2 
# compute percentile interval of mean
MAM.seq <- seq(from = -3, to = 3.5, length.out= 30) # why make a list of numbers instead of using the orignal data? 
# because you want to get a precise prediction of the shaded area
mu <- link(m5.1, data = data.frame(MedianAgeMarriage.s=MAM.seq))
mu.PI <- apply(mu, 2, PI)

# plot it all
plot(Divorce ~ MedianAgeMarriage.s, data=d, col=rangi2)
abline(m5.1)
shade(mu.PI, MAM.seq)

############ what if using the original data 
mu.2 <- link(m5.1)
mu.2.PI <- apply(mu.2, 2, PI)
plot(Divorce ~ MedianAgeMarriage.s, data=d, col=rangi2)
abline(m5.1)
shade(mu.2.PI, d$MedianAgeMarriage.s)
#############################################

precis(m5.1) # how to inspect the result, read the result. 

# R code 5.3
d$Marriage.s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage) # standardize predictor values
m5.2 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR * Marriage.s,
    a ~ dnorm(10, 10),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d) # build the model

MAM.seq <- seq(from = -3, to = 3.5, length.out= 30) # make a list of predictor values
mu <- link(m5.2, data = data.frame(Marriage.s=MAM.seq)) # compute model values for each predictor values 
?link
data.frame(Marriage.s=MAM.seq) 
mu
mu.PI <- apply(mu, 2, PI) # compute 95% PI for model values  
mu.PI

# plot it all
plot(Divorce ~ Marriage.s, data=d, col=rangi2) 
# make a plot using data d with Divorce as y axis and Marrige.s as x axis
abline(m5.2) 
?abline
shade(mu.PI, MAM.seq)
?shade

# R code 5.4
m5.3 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR*Marriage.s + bA * MedianAgeMarriage.s,
    a ~ dnorm(10, 10),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
  data = d )
precis(m5.3)

# R code 5.5
plot(precis(m5.3)) # could not plot??? 

# R code 5.6 predictor residual plot
m5.4 <- map(
  alist(
    Marriage.s ~ dnorm(mu, sigma),
    mu <- a + b*MedianAgeMarriage.s,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
data = d)

# R code 5.7
# compute the residual by sbutracting the observed marriage rate in each
# state from the predicted rate, based upon using age at marriage
# compuate expected value at MAP, for each state
mu <- coef(m5.4)['a'] + coef(m5.4)['b'] * d$MedianAgeMarriage.s
mu
# compute residual for each state
mu.resid <- d$Marriage.s - mu
d$Marriage.s
mu.resid
# R code 5.8
plot(Marriage.s ~ MedianAgeMarriage.s, d, col=rangi2)
abline(m5.4)
# loop over states
for (i in 1:length(mu.resid)) {
  x <- d$MedianAgeMarriage.s[i]  # x loaction of line segment
  y <- d$Marriage.s[i]  # observed endpoint of line segment
  # draw the line segment
  lines(c(x, x), c(mu[i], y), lwd=0.5, col=col.alpha("black", 0.7))
} 

?lines
# R code 5.9
# prepare new counterfactural data
A.avg <- mean(d$MedianAgeMarriage.s) # mean of standard marriage age
A.avg # a single value
R.seq <- seq(from = -3, to = 3, length.out = 30) # 30 marriage rate values from -3 to 3
R.seq
pred.data <- data.frame( 
  Marriage.s = R.seq,
  MedianAgeMarriage.s=A.avg
) # make a data frame using Marriage.s & MedianAgeMarrige.s
pred.data
# compute counterfactual mean divorce (mu)
mu <- link(m5.3, data = pred.data) 
# compute model values for each combination of predictor value (a constant marriage age)  
###################### need to understand link more thouroughly ########################
head(mu)
dim(mu)
m5.3
mu.mean <- apply(mu, 2, mean) # find mean for mu
mu.PI <- apply(mu, 2, PI) # 95% PI of mu 

# simulate counterfactual divorce outcomes
R.sim <- sim(m5.3, data = pred.data, n = 1e4) # difference between sim() and link() ????????
# simulate 1e4 mu values based on model 5.3 using predictor values of pred.data (constant marriage age)
head(R.sim)
dim(R.sim)
R.PI <- apply(R.sim, 2, PI)

# display predictions, hiding raw data with type="n"
plot(Divorce ~ Marriage.s, data = d, type="n") # what is Marriage.s 
# d$Marriage.s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage) # $Marriage is Marriage rate
mtext("MedianAgeMarriage.s=0")
lines(R.seq, mu.mean) # 
?lines
shade(mu.PI, R.seq) # mu.PI is for the computed mean value?
shade(R.PI, R.seq) # R.PI is for the predicted value? 

# R code 5.10, do the same computation as R code 5.9
# control for Marraige rate
R.avg <- mean(d$Marriage.s)
A.seq <- seq(from = -3, to = 3.5, length.out = 30)
pred.data2 <- data.frame(
  Marriage.s=R.avg,
  MedianAgeMarriage.s=A.seq
)

mu <- link(m5.3, data = pred.data2)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

A.sim <- sim(m5.3, data = pred.data2, n = 1e4)
A.PI <- apply(A.sim, 2, PI)

plot(Divorce ~ MedianAgeMarriage.s, data = d, type="n") 
mtext("Marriage.s=0")
lines(A.seq, mu.mean) 
shade(mu.PI, A.seq) 
shade(A.PI, A.seq) 

# how to understand these two plots? 
# posterior prediciton plots
# R code 5.11
# call link without specifying new data
# so it uses original data
mu <- link(m5.3)

# summarize samples across cases
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# simulate observations
# again no new data, so uses original data
divorce.sim <- sim(m5.3, n = 1e4)
divorce.PI <- apply(divorce.sim, 2, PI) # how these simulated data were used in the plot? 

# R code 5.12 
# plot predictions against observed. add a line to show perfect 
# prediction and line segment for the confidence interval of each prediciton
plot(mu.mean ~ d$Divorce, col=rangi2, ylim=range(mu.PI),
     xlab="Observed divorce", ylab="Predicted divorce") # plot computed data
mu.mean # 50 iterms
mu # 1000 items, computed mu, default is 1000 items
?link
mu.PI # 50 items

abline(a=0, b=1, lty=2) # a the intercept, b the slope
?abline
nrow(d) # 50 items
for(i in 1:nrow(d))
  lines(rep(d$Divorce[i],2), c(mu.PI[1,i], mu.PI[2,i]), 
# for each data point, draw 5% and 95% confididence interval
  col=rangi2)

rep(d$Divorce[1],2)
?rep
mu.PI[1,1]
mu.PI[2,1]
c(mu.PI[1,1], mu.PI[2,1])
mu.PI
?lines

# R code 5.13
identify(x=d$Divorce, y=mu.mean, labels = d$Loc, cex=0.8)

# R code 5.14
# compute residuals
divorce.resid <- d$Divorce - mu.mean
divorce.resid # 50 items
# get ordering by divorce rate
o <- order(divorce.resid) 
?order
# make the plot
dotchart(divorce.resid[o], labels = d$Loc[o], xlim = c(-6,5), cex = 0.6) 
# make a dot chart using divorce resid according to order o 
?dotchart
abline(v =0, col=col.alpha("black", 0.2)) # draw a line at 0 
?abline
for (i in 1:nrow(d)){
  j <- o[i] # which state in order
  lines(d$Divorce[j]-c(mu.PI[1,j], mu.PI[2,j]), rep(i,2)) 
  points(d$Divorce[j]-c(divorce.PI[1,j], divorce.PI[2,j]), rep(i,2), # the divorce.PI used here
         pch=3, cex=0.6, col="red")      
}

o
o[1]
d$Divorce[13]-c(mu.PI[1,13], mu.PI[2,13])
lines(d$Divorce[13]-c(mu.PI[1,13], mu.PI[2,13]))
rep(1,2)

# Note from class
# why standardize... 

################################## For 04/25/2016 #####################################

# R code 5.16
library(rethinking)
data("milk")
d <- milk
str(d)

# R code 5.17 check the simple bivariate regression between kilocalories & neocortex percent
m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), 
data = d)

# R code. 5.18
d$neocortex.perc # missing data in this column

# R code 5.19 make a new data frame w/ only complete cases in it
dcc <- d[complete.cases(d),]
complete.cases(d)
dcc

# R code 5.20 now check the regression with complete cases
m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), 
  data = dcc)

# R code 5.21 take a look at the quandratic approximate posterior
precis(m5.5, digits = 3) # the 89% interval of the bn are on both sides of zero, so positive or negative??? 

# R code 5.22 
# a change from the smallest neocortex percent to the largest would result in an unexpected change of 
coef(m5.5)["bn"]*(76-55)
?"coef"

# R code 5.23. plot the predicted mean and 89% interval for the mean to see how the distance on both sides of zero
np.seq <- 0:100 # generate 101 numbers from 0 to 100 with interval of 1 
np.seq
pred.data <- data.frame(neocortex.perc=np.seq) # make a dataframe with the 101 numbers as neocortex percentage
pred.data 

mu <- link(m5.5, data = pred.data, n = 1e4) # compute linear model values for m5.5 using 101 numbers
?link
head(mu)
m5.5
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
nrow(mu.PI)

plot(kcal.per.g ~ neocortex.perc, data=dcc, col=rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2) # the lower 5.5% interval  
lines(np.seq, mu.PI[2,], lty=2) # the upper 5.5% interval 
?lines

# R code 5.24 fit another bivariate regression, with the log of body mass as the predictor variable 
dcc$log.mass <- log(dcc$mass) # log transform mass 
# Q when to transform??? 
min(dcc$log.mass)
max(dcc$log.mass)

# R code 5.25. fit the model
m5.6 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm * log.mass,
    a ~ dnorm(0, 100),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = dcc)
precis(m5.6)

# make a plot 
np.seq <- -5:5 # generate 101 numbers from 0 to 100 with interval of 1 
pred.data <- data.frame(log.mass=np.seq) # make a dataframe with the 101 numbers as neocortex percentage

mu <- link(m5.6, data = pred.data, n = 1e4) # compute linear model values for m5.5 using 101 numbers
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data=dcc, col=rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2) # the lower 5.5% interval  
lines(np.seq, mu.PI[2,], lty=2) # the upper 5.5% interval 

# 5.26 fit a multivaraite model 
m5.7 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc + bm*log.mass,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), 
  data = dcc)
precis(m5.7)

# R code 5.27
mean.log.mass <- mean(log(dcc$mass)) # only use mean of the log.mass as the log.mass
np.seq <- 0:100
pred.data <- data.frame(
  neocortex.perc = np.seq,
  log.mass=mean.log.mass
)

mu <- link(m5.7, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data=dcc, type="n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)


mean.neocortex.perc <- mean(dcc$neocortex.perc) # only use mean of the log.mass as the log.mass
np.seq <- -5:5
pred.data <- data.frame(
  neocortex.perc = mean.neocortex.perc,
  log.mass=np.seq
)

mu <- link(m5.7, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data=dcc, type="n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

# R code 5.28 simulate data in which two meaningful predictors act to mask one another
N <- 100
rho <- 0.7
x_pos <- rnorm(N) # produce 100 numbers with normal distribution, mean of 0 and sd of 1

x_neg <- rnorm(N, mean=rho*x_pos,
               sd=sqrt(1-rho^2)) # produce 100 numbers with normal distribution (mean of )
y <- rnorm(N, mean=x_pos- x_neg)
d <- data.frame(y, x_pos, x_neg)
d

pairs(d)

# R code 5.29 simulate heights and leg lengths of 100 individuals
N <- 100
height <- rnorm(N, 10, 2)
leg_prop <- runif(N, 0.4, 0.5)
leg_left <- leg_prop * height + 
  rnorm(N, 0, 0.02)
leg_right <- leg_prop * height + 
  rnorm(N, 0, 0.02)

d <- data.frame(height, leg_left, leg_right)

# R code 5.30
m5.8 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ), 
  data=d)
precis(m5.8)

# a graphic view of the precis output
plot(precis(m5.8))

# R code 5.32
post <- extract.samples(m5.8)
?extract.samples
plot(bl ~ br, post, col=col.alpha(rangi2, 0.1), pch=16)

# R code 5.33
sum_blbr <- post$bl + post$br
dens(sum_blbr, col=rangi2, lwd=2, xlab="sum of bl and br")

# R code 5.34
m5.9 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ), 
  data = d)
precis(m5.9)

# R code 5.35, get back to the primate mild data
data("milk")
d <- milk

# R code 5.36 model kcal.per.g as a fxn of perc.fat & perc.lactose
# with two bivariate regressions
# kcal.per.g regressed on perc.fat
m5.10 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf * perc.fat,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
  data = d)

# kcal.per.g regressed on perc.lactose
m5.11 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bl*perc.lactose,
    a ~ dnorm(0.6, 10),
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
  data = d)

precis(m5.10, digits = 3)
precis(m5.11, digits = 3)

# R code 5.37
m5.12 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf * perc.fat + bl* perc.lactose,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0, 1),
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d)
precis(m5.12, digits = 3)

# R code 5.38 plot to see the problem ???
pairs(~ kcal.per.g + perc.fat + perc.lactose, # how to understand this fomula
      data=d, col=rangi2)
?pairs

# R code 5.39 compute the correlation between the two variables
cor(d$perc.fat, d$perc.lactose)

# R code 5.40 # how to understand this, leave for later... 
sim.coll <- function(r=0.9){
  d$x <- rnorm(nrow(d), mean=r*d$perc.fat,
  sd=sqrt((1-r^2)*var(d$perc.fat)))
  m <- lm(kcal.per.g ~ perc.fat + x, data = d)
  sqrt(diag(vcov(m)))[2] # stddev of parameter
}

rep.sim.coll <- function(r=0.9, n=100){
  stddev <- replicate(n, sim.coll(r))
  mean(stddev)
}

r.seq <- seq(0, 0.99, by = 0.01)
stddev <- sapply(r.seq, function(z) rep.sim.coll(r=z, n = 100))
plot(stddev ~ r.seq, type="l", col=rangi2, lwd=2, xlab="corrleation")

# R code 5.41 simulate data for post-treatment variable 
# number of plants
N <- 100

# simulate initial heights
h0 <- rnorm(N, 10, 2) # size N, mean 10, sd of 2 
h0 # initial height
hist(h0)

# assign treatments and simulate fungus and growth
treatment <- rep(0:1, each=N/2) # repeat 0 & 1 each N/2 times, 1 trt, 0 no trt
treatment
fungus <- rbinom(N, size = 1, prob = 0.5-treatment*0.4) 
# N plants, each with 1 trial and 0.5-treatment*0.4 probability of fungus infection
fungus # 1 means w/ fungus infection, 0 means w/o
?rbinom
h1 <- h0+ rnorm(N, 5-3*fungus) # final height of plants
set.seed(100)
rnorm(N, 5-3*fungus, 1) # size N, mean 5-3*fungus, sd of 1 yes!!! 
# Q!!! rnomr iterate through every item in 5-3*fungus as the mean? 
set.seed(100)
rnorm(N, 5-3*fungus)
5-3*fungus
h1
?rnorm

# compse a clean data frame
d <- data.frame(h0=h0, h1=h1, treatment=treatment, fungus=fungus)
d

# R code 5.42, fit a model that includes all the available variable from R 5.41
m5.13 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh*h0 + bt *treatment + bf*fungus, # fomula
    a ~ dnorm(0, 100), # intercept, mean of 0, sd of 100 
    c(bh,bt,bf) ~ dnorm(0, 10), # what is this? QQQQQQ !!!
    sigma ~ dunif(0, 10)
  ),
  data = d)
precis(m5.13)

?dnorm

# R code 5.43 omit post-trt variable fungus
m5.14 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh*h0 + bt*treatment,
    a ~ dnorm(0, 100),
    c(bh,bt) ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data = d)
precis(m5.14)

# Note from R club 
# Discussion on plot 5.4 in the book 
# I lost my code again!!! although it is fine this time!
# remember not to checkout head on a branch next time!!! 

##################### for 05/02/2016 ##############

# R code 5.44
library(rethinking)
data(Howell1)
d <- Howell1
str(d)

# R code 5.45, fit a model
m5.15 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bm*male,
    a ~ dnorm(178, 100),
    bm ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), 
  data = d)
precis(m5.15)
plot(precis(m5.15))

# R code 5.46, the posteiro distribution of male height
post <- extract.samples(m5.15)
mu.male <- post$a + post$bm
PI(mu.male)

# R code 5.47, re-parameterizing the model
m5.15b <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- af * (1-male) + am*male,
    af ~ dnorm(178, 100),
    am ~ dnorm(178, 100),
    sigma ~ dunif(0, 50)
  ), 
  data = d)
precis(m5.15b)
precis(m5.15)

# R code 5.48
data(milk)
d <- milk
unique(d$clade)
?unique

# R code 5.49, to create a dummy variable for the New World Monkey category
d$clade.NWM <- ifelse(d$clade=="New World Monkey", 1, 0)
?ifelse

# R code 5.50, make two more dummy variables
d$clade.OWM <- ifelse(d$clade == "Old World Monkey", 1, 0)
d$clade.S <- ifelse(d$clade=="Strepsirrhine", 1, 0)

d$clade.OWM
d$clade.S

# R code 5.51, fit the model
m5.16 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + b.NWM*clade.NWM + b.OWM*clade.OWM + b.S*clade.S,
    a ~ dnorm(0.6, 10),
    b.NWM ~ dnorm(0, 1),
    b.OWM ~ dnorm(0, 1),
    b.S ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
  data = d)
precis(m5.16)

# R code 5.52, get the posterior distribution of the average milk energy in each category
# sample posterior
post <- extract.samples(m5.16)

# compute averages for each category
mu.ape <- post$a
mu.NWM <- post$a + post$b.NWM
mu.OWN <- post$a + post$b.OWM
mu.S <- post$a + post$b.S

# summarize using precis
precis(data.frame(mu.ape, mu.NWM, mu.OWN, mu.S))

# R code 5.53, differences between the two monkey groups
diff.NWM.OWM <- mu.NWM - mu.OWN
quantile(diff.NWM.OWM, probs=c(0.025, 0.5, 0.975))

# R code 5.54
d$clade_id <- coerce_index(d$clade)

# R code 5.55
m5.16_alt <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm(0.6, 10),
    sigma ~ dunif(0, 10)
  ),
  data = d)
precis(m5.16_alt, depth = 2)

# R code 5.62 
data("cars")
glimmer(dist ~ speed, data = cars)
?glimmer

#################### For 05/09/2016 #############
# regulatory prior & information criteria 
# R code 6.1

sppnames <- c("afarensis","africanus","habilis","boisei",
              "rudolfensis","ergaster","sapiens")
brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)
masskg <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)
d <- data.frame(species=sppnames, brain=brainvolcc, mass=masskg)
d

# R code 6.2
m6.1 <- lm(brain ~ mass, data = d)

# R code 6.3
1 - var(resid(m6.1))/var(d$brain)
summary(m6.1)

# R code 6.4 
m6.2 <- lm(brain ~ mass + I(mass^2), data = d)
?I

# R code 6.5
m6.3 <- lm(brain ~ mass + I(mass^2) + I(mass^3), data = d)
m6.4 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4), data = d)
m6.4 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5), data = d)
m6.4 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6), data = d)

# R code 6.6
m6.7 <- lm(brain ~ 1, data = d)

# R code 6.7
d.new <- d[-i, ]

library(rethinking)
# R code 6.8 
plot(brain ~ mass, d, col="slateblue")
for (i in 1:nrow(d)){
  d.new <- d[-i, ]
  m0 <- lm(brain ~ mass, d.new)
  abline(m0, col=col.alpha("black", 0.5))
}

# R code 6.9
p <- c(0.7, 0.15, 0.15)
-sum(p*log(p))

# R code 6.10
# fit model with lm
m6.1 <- lm(brain ~ mass, d)

# compute deviance by cheating
(-2) * logLik(m6.1)
? logLik

############## read the below codes laters... ################ 
# R code 6.11 ## try to understand these codes afterwards... 
# standardize the mass before fitting
d$mass.s <- (d$mass- mean(d$mass))/sd(d$mass)
m6.8 <- map(
  alist(
    brain ~ dnorm(mu, sigma),
    mu <- a + b*mass.s
  ), 
  data = d,
  start = list(a=mean(d$brain), b=0, sigma=sd(d$brain)),
  method = "Nelder-Mead")

# extract MAP estimates
theta <- coef(m6.8)

# compute deviance # read these code later... 
dev <- (-2)*sum(dnorm(
  d$brain, 
  mean = theta[1]+theta[2]*d$mass.s,
  sd = theta[3],
  log = TRUE))
dev
?dnorm


# R code 6.12 # read 6.12, 6.13, 6.14 afterwards... 
N <- 20
kseq <- 1:5
dev <- sapply(kseq, function(k){
  print(k);
  r <- replicate(1e4, sim.train.test(N=N, k=k));
  c(mean(r[1,]), mean(r[2,]), sd(r[1,]), sd(r[2,]))
})

# R code 6.13 
r <- mcreplicate(1e4, sim.train.test(N=N, k=k), mc.cores = 4)

# R code 6.14
plot(1:5, dev[1,], ylim=c(min(dev[1:2,])-5), max(dev[1:2,]+10),
    xlim=c(1,5.1), xlab="number of parameters", ylab="deviance",
    pch=16, col=rangi2)

mtext(concat("N=", N))
points((1:5)+0.1, dev[2,])

for (i in kseq){
  pts_in <- dev[1,i]+c(-1,+1)*dev[3,i]
  pts_out <- dev[2,i]+c(-1,+1)*dev[4,i]
  lines(c(i,i), pts_in, col=rangi2)
  line(c(i,i)+0.1, pts_out)
}
?sim.train.test

################ For 05/16/2016 ######################### 
# R code 6.15 WAIC calculation 
library(rethinking)
data(cars)
m <- map(
  alist(
    dist ~ dnorm(mu, sigma),
    mu <- a + b*speed,
    a ~ dnorm(0, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 30)
  ), data = cars)
post <- extract.samples(m, n = 1000)
head(post)
# R code 6.16 need the log-likelihood of each observation i at each sample s from the posterior
n_samples <- 1000
ll <- sapply(1:n_samples, function(s){
 mu <- post$a[s] + post$b[s]*cars$speed
 dnorm(cars$dist, mu, post$sigma[s], log = TRUE)
})
head(cars)
dim(cars)
?dnorm
dim(ll)
head(ll)
nrow(cars)
# R code 6.17
n_cases <- nrow(cars)
lppd <- sapply(1:n_cases, function(i) log_sum_exp(ll[i,]-log(n_samples)))
sum(lppd)
# R code 6.18
pWAIC <- sapply(1:n_cases, function(i) var(ll[i,]))

# R code 6.19 
-2 * (sum(lppd) - sum(pWAIC))

# R code 6.20
waic_vec <- -2*(lppd-pWAIC)
sqrt(n_cases*var(waic_vec))

################# For R club 05/23/2016 ##############################
# R code 6.21 
library(rethinking)

data(milk)
d <- milk[complete.cases(milk),]
d$neocortex <- d$neocortex.perc / 100
dim(d)

# R code 6.22
head(d)
a.start <- mean(d$kcal.per.g) 
a.start
sigma.start <- log(sd(d$kcal.per.g))
sigma.start
?map # find mode of posterior distribution for arbitrary fixed effect models
?alist
?list # 
?dnorm # 
m6.11 <- map(
  alist(
    kcal.per.g ~ dnorm(a, exp(log.sigma)) # I forget how to intepret this... 
  ), 
  data = d, start = list(a=a.start, log.sigma = sigma.start))
m6.12 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bn*neocortex
  ), 
  data = d, start = list(a=a.start, bn=0, log.sigma = sigma.start))
m6.13 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bm*log(mass)
  ), data = d, start = list(a=a.start, bm=0, log.sigma=sigma.start))
m6.14 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bn*neocortex + bm*log(mass)
  ), data = d, start = list(a=a.start, bn=0, bm=0, log.sigma=sigma.start))

# R code 6.23
library(rethinking)
WAIC(m6.14)

# R code 6.24
(milk.models <- compare(m6.11, m6.12, m6.13, m6.14))

# R code 6.25
plot(milk.models, SE=TRUE, dSE=TRUE)

# R code 6.26
diff <- rnorm(1e5, 6.7, 7.26) # generate 1e5 numbers w/ mean of 6.7 and stdv of 7.26
sum(diff<0)/1e5 
?rnorm # generates random deviates 
rnorm(1e5, 6.7, 7.26)
length(diff) 

# R code 6.27
coeftab(m6.11, m6.12, m6.13, m6.14)
?coeftab # returns a table of model coefficients in ros and models in columns

# R code 6.28
plot(coeftab(m6.11, m6.12, m6.13, m6.14)) # something wrong!!! 
?coeftab_plot
# Error in as.double(y) : 
# cannot coerce type 'S4' to vector of type 'double'

# R code 6.29
# compute counterfactual predictions
# neocortex from 0.5 to 0.8
nc.seq <- seq(0.5, 0.8, length.out = 30)
d.predict <- list(
  kcal.per.g = rep(0, 30), # empty outcome
  neocortex = nc.seq, # sequence of neocortex
  mass=rep(4.5, 30) # average mass
)
d.predict

pred.m6.14 <- link(m6.14, data = d.predict)
?link # compute model values for map samples
dim(pred.m6.14)
head(pred.m6.14)

mu <- apply(pred.m6.14, 2, mean)
mu.PI <- apply(pred.m6.14, 2, PI)
mu
length(mu)
mu.PI

# plot it all
plot(kcal.per.g ~ neocortex, d, col=rangi2)
lines(nc.seq, mu, lty=2)
lines(nc.seq, mu.PI[1,], lty=2) # x and y lengths differ??? 
lines(nc.seq, mu.PI[2,], lty=2)
shade(mu.PI, nc.seq)
mu.PI

# R code 6.30
milk.ensemble <- ensemble(m6.11, m6.12, m6.13, m6.14, data = d.predict)
?ensemble # use link & sim for a list of map model fit to construct Akaike weighted ensemble of predictions
dim(milk.ensemble$link)
head(milk.ensemble$link)

mu <- apply(milk.ensemble$link, 2, mean)
mu.PI < apply(milk.ensemble$link, 2, PI)
lines(nc.seq, mu)
shade(mu.PI, nc.seq)

# markdown file output options: ... cache=TRUE, dependency on the previous chunk of code... 

######################## For 06/06/2016 ######################################

# R code 7.1 
library(rethinking)
data(rugged)
d <- rugged

head(d)
dim(d) #234 countries w/ 51 sets of data 
# make log version of outcome
d$log_gdp <- log(d$rgdppc_2000)
dim(d) #234 countries w/ 52 sets of data 
# extract countries w/ GDP data
dd <- d[complete.cases(d$rgdppc_2000), ]
dim(dd) # 170 countries 

# split countries into Africa & non-Africa 
d.A1 <- dd[dd$cont_africa==1, ] # Africa
dim(d.A1) # 49 countries
d.A0 <- dd[dd$cont_africa==0, ] # not Africa
dim(d.A0) #121 countries 

# R code 7.2 
# African nations
m7.1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d.A1)

# Non African nations
m7.2 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d.A0)

# plot the posterior predictions 
# m7.1
rugged.seq <- seq(0, 8, by=1)
mu <- link(m7.1, data = data.frame(rugged = rugged.seq))
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)

plot(log_gdp ~ rugged, data=d.A1, col=col.alpha(rangi2, 0.5))
lines(rugged.seq, mu.mean)
shade(mu.HPDI, rugged.seq)

# m7.2 
rugged.seq <- seq(0, 8, by=1)
mu <- link(m7.2, data = data.frame(rugged = rugged.seq))
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)

plot(log_gdp ~ rugged, data=d.A0, col=col.alpha(rangi2, 0.5))
lines(rugged.seq, mu.mean)
shade(mu.HPDI, rugged.seq)

# R code 7.3 
m7.3 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = dd)

# R code 7.4
m7.4 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa, # go back to understand dummy variable... 
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = dd)

# R code 7.5 
compare(m7.3, m7.4) # understand the result, especially the standard error part... 

# R code 7.6
rugged.seq <- seq(-1, 8, by = 0.25)
length(rugged.seq) # 37 
# compute mu over samples, fixing cont_africa = 0
mu.NotAfrica <- link(m7.4, data = data.frame(cont_africa=0, rugged==rugged.seq))

# compute mu over samples, fixing cont_africa = 1
mu.Africa <- link(m7.4, data = data.frame(cont_africa=1, rugged==rugged.seq))

# summarize to means & intervals 
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
length(mu.NotAfrica.mean) # 234, why 234 
mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI, prob=0.97)
mu.Africa.mean <- apply(mu.Africa, 2, mean)
length(mu.Africa.mean) # 234 
mu.Africa.PI <- apply(mu.Africa, 2, PI, prob=0.97)

### how to plot figure 7.3 
plot(log_gdp ~ rugged, data=dd, col=col.alpha(rangi2, 0.5))

lines(rugged.seq, mu.Africa.mean)
shade(mu.Africa.PI, rugged.seq)

lines(rugged.seq, mu.NotAfrica.mean)
shade(mu.NotAfrica.PI, rugged.seq)

# R code 7.7 # using all data, add dummy variable, add linear interaction effect 
m7.5 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + gamma * rugged + bA*cont_africa,
    gamma <- bR + bAR * cont_africa,
    a ~ dnorm(8, 100),
    bA ~ dnorm(0, 1),
    bR ~ dnorm(0, 1),
    bAR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = dd)

# R code 7.8
compare(m7.3, m7.4, m7.5)

# R code 7.9
m7.5b <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bAR*rugged*cont_africa + bA*cont_africa,
    a ~ dnorm(8, 100),
    bA ~ dnorm(0, 1),
    bR ~ dnorm(0, 1),
    bAR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = dd)

# R code 7.10, calcualte predicted mean and PI using just subset of the data (Africa/NonAfrica)
rugged.seq <- seq(-1, 8, by = 0.25)

mu.Africa <- link(m7.5, data = data.frame(cont_africa=1, rugged= rugged.seq))
mu.Africa.mean <- apply(mu.Africa, 2, mean)
mu.Africa.PI <- apply(mu.Africa, 2, PI, prob= 0.97)

mu.NotAfrica <- link(m7.5, data = data.frame(cont_africa=0, rugged= rugged.seq))
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI, prob= 0.97)

# R code 7.11 
# plot Afican nations w/ regression, plot using subset of data (Africa/NonAfrica)
d.A1 <- dd[dd$cont_africa==1,]

plot(log(rgdppc_2000) ~ rugged, data=d.A1,
     col=rangi2, ylab="log GDP year 2000",
     xlab="Terrain Ruggedness Index")
mtext("African nations", 3)
lines(rugged.seq, mu.Africa.mean, col=rangi2)
shade(mu.Africa.PI, rugged.seq, col = col.alpha(rangi2, 0.3))

# plot non-Afican nations w/ regression
d.A0 <- dd[dd$cont_africa==0,]

plot(log(rgdppc_2000) ~ rugged, data=d.A0,
     col="black", ylab="log GDP year 2000",
     xlab="Terrain Ruggedness Index")
mtext("Non-African nations", 3)
lines(rugged.seq, mu.NotAfrica.mean, col=rangi2)
shade(mu.NotAfrica.PI, rugged.seq)

# R code 7.12 
precis(m7.5)

# R code 7.13, compute the posterior distribution of gamma
post <- extract.samples(m7.5)
gamma.Africa <- post$bR + post$bAR*1
length(gamma.Africa)
gamma.notAfrica <- post$bR + post$bAR*0

# R code 7.14, get mean of gamma  
mean(gamma.Africa)
mean(gamma.notAfrica)

# R code 7.15, full distribution of the slope within and outside of Africa
dens(gamma.Africa, xlim=c(-0.5, 0.6), ylim=c(0, 5.5),
     xlab="gamma", col=rangi2)
dens(gamma.notAfrica, add = TRUE)

# R code 7.16
diff <- gamma.Africa - gamma.notAfrica
sum(diff<0)/length(diff) 

# R code 7.17
# get min and max rugged values
q.rugged <- range(dd$rugged)
q.rugged
q.rugged[1] 
data.frame(rugged=q.rugged[1], cont_africa=0:1)
# compute lines and confidence intervals
mu.ruggedlo <- link(
  m7.5, data = data.frame(rugged=q.rugged[1], cont_africa=0:1)) # just on 2 predictors? 
head(mu.ruggedlo)
dim(mu.ruggedlo) # 1000 2 
mu.ruggedlo.mean <- apply(mu.ruggedlo, 2, mean)
mu.ruggedlo
head(mu.ruggedlo)
mu.ruggedlo.mean
mu.ruggedlo.PI <- apply(mu.ruggedlo, 2, PI)

mu.ruggedi <- link(m7.5,
                   data = data.frame(rugged=q.rugged[2], cont_africa=0:1))
mu.ruggedi.mean <- apply(mu.ruggedi, 2, mean)
mu.ruggedi.PI <- apply(mu.ruggedi, 2, PI)

# plot it all, splitting points at median 
med.r <- median(dd$rugged) # median 
med.r
ox <- ifelse(dd$rugged > med.r, 0.05, -0.05) # assign values to each country based on their 
# rugged value compared to the median 
?ifelse 
ox
length(ox)
dim(dd)
plot(dd$cont_africa + ox, log(dd$rgdppc_2000), # adding ox to each value differentiate 
# them not only by african or not but also by whether their rugged value is below or above the median 
     col=ifelse(dd$rugged>med.r, rangi2, "black"),
     xlim=c(-0.25, 1.25), xaxt="n", ylab="log GDP year 2000",
     xlab="Continent")
dd$cont_africa + ox
dd$cont_africa
?plot # xaxt 

axis(1, at=c(0,1), labels = c("others", "Africa"))
?axis
lines(0:1, mu.ruggedlo.mean, lty=2)
?lines
mu.ruggedlo.mean
shade(mu.ruggedlo.PI, 0:1)
lines(0:1, mu.ruggedi.mean, col=rangi2)
shade(mu.ruggedi.PI, 0:1, col = col.alpha(rangi2, 0.25))

# R code 7.18
library(rethinking)
data(tulips)
d <- tulips
str(d)

# R code 7.19, build model using two predictors w/ and w/o interaction term
m7.6 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water + bS*shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), data = d)

m7.7 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water + bS*shade + bWS*water*shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), data = d)

# R code 7.20, fix the problem in the last the code 
m7.6 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water + bS*shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data = d,
  method = "Nelder-Mead",
  control=list(maxit=1e4)
  )

m7.7 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water + bS*shade + bWS*water*shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data = d,
  method = "Nelder-Mead",
  control=list(maxit=1e4)
  )

# R code 7.21 
coeftab(m7.6, m7.7)
?coeftab # return a table of model coeffecients in row and models in columns

# R code 7.22
compare(m7.6, m7.7)

# R code 7.23, make centered versions of shade and water, just subtract the mean of the original
# from each value
d$shade.c <- d$shade - mean(d$shade)
d$water.c <- d$water - mean(d$water)

range(d$shade)
range(d$shade.c)

# R code 7.24, re-estimate the two regression models, using the new centered variables. 
# add start list, because the very flat priors provide terrible random starting locations. 
m7.8 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water.c + bS*shade.c,
    a ~ dnorm(130, 100), # why change to 130 from 0? 
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data = d,
  start = list(a=mean(d$blooms), bS=0, bS=0, sigma=sd(d$blooms)) # start value VS prior, need to understand
  # the contribution of the two 
)

m7.9 <- map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water.c + bS*shade.c + bWS*water.c*shade.c,
    a ~ dnorm(130, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data = d,
  start = list(a=mean(d$blooms), bS=0, bS=0, bWS=0, sigma=sd(d$blooms))
)
coeftab(m7.8, m7.9)

# R code 7.25
k <- coef(m7.7) # uncentered model
k
k[1] + k[2]*2 + k[3]*2 + k[4]*2*2

# R code 7.26
k <- coef(m7.9) # centered model 
k
k[1] + k[2]*0 + k[3]*0 + k[4]*0*0

# R code 7.27
precis(m7.9)

# R code 7.28, plot the implied predictions
# make a plot window with three panels in a single row 
par(mfrow=c(1,3)) # 1 row, 3 columns

# loop over values of water.c and plot predictions
shade.seq <- -1:1
for ( w in -1:1){
  dt <- d[d$water.c==w,]
  plot(blooms ~ shade.c, data=dt, col=rangi2,
       main=paste("water.c=", w), xaxp=c(-1,1,2), ylim=c(0, 362),
       xlab="shade (centered)")
  mu <- link(m7.9, data = data.frame(water.c=w, shade.c=shade.seq))
  mu.mean <- apply(mu, 2, mean)
  mu.PI <- apply(mu, 2, PI, prob= 0.97)
  lines(shade.seq, mu.mean)
  lines(shade.seq, mu.PI[1,], lty=2)
  lines(shade.seq, mu.PI[2,], lty=2)
}

# R code 7.29
m7.x <- lm(y ~ x + z + x*z, data = d)

# R code 7.30
m7.x <- lm(y ~ x*z, data = d)

# R code 7.31, subtract one main effect
m7.x <- lm(y~x + x*z -z, data = d) # can we apply this to our analysis? just consider the interaction effect? 

# R code 7.32, three way interactions model design
m7.x <- lm(y ~ x*z*w, data = d)

# R code 7.33, expand a three-way interaction into a full set of terms
x <- z <- w <- 1
colnames(model.matrix(~x*z*w))
?model.matrix # create a design matrix 

# note from R club
# how to set the prior: plot the actual data, mean as the alpha and stdv from the plot as the stdv, beta as 0 makes more sense. 
# regularizing prior, using broad prior 

# x:z interaction  x*z main effects + interaction 

?try # try an expression allowing error recorvery, can knit always.. "try-error"
library(rethinking)

########################### For 06/13/2016 #####################################
library(rethinking)

# R code 8.1
num_weeks <- 1e5
positions <- rep(0, num_weeks)
current <- 10
for (i in 1:num_weeks){
  # record current position
  positions[i] <- current
  
  # flip coin to generate proposal
  proposal <- current + sample(c(-1,1), size = 1) # sampling to decide clock or counterclockwise
  # now make sure he loops around the archipelago
  if (proposal < 1) proposal <- 10  
  if (proposal > 10) proposal < 1
  
  # move?
  prob_move <- proposal/current # 
  current <- ifelse(runif(1) < prob_move, proposal, current) 
}

runif(1) # pick a number from 0 to 1 randomly w/ a uniform distribution
?ifelse # 

########################## chapter 8 part 2 ###################################

# R code 8.2 
library(rethinking)
data("rugged")
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000), ]

# R code 8.3, fit an interaction model with map
m8.1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data = dd)
precis(m8.1)
# Mean StdDev  5.5% 94.5%
#   a      9.22   0.14  9.00  9.44
# bR    -0.20   0.08 -0.32 -0.08
# bA    -1.95   0.22 -2.31 -1.59
# bAR    0.39   0.13  0.19  0.60
# sigma  0.93   0.05  0.85  1.01

# R code 8.4 
dd.trim <- dd[, c("log_gdp", "rugged", "cont_africa")]
str(dd.trim)

library('BH')
library('rstan')

# R code 8.5 get samples from posterior distribution using rstan
m8.1stan <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 2)
  ), data = dd.trim) 
?map2stan

# R code 8.6
precis(m8.1stan)

# R code 8.7, run four independent MC for the model above, and to distribute them across 
# seperate processors in your computer 
m8.1stan_4chains <- map2stan(m8.1stan, chains = 4, cores = 4)
precis(m8.1stan_4chains)

# R code 8.8, pull out samples
post <- extract.samples(m8.1stan)
str(post)

# R code 8.9 show correlations between parameters
pairs(post)

# R code 8.10
pairs(m8.1stan)

# R code 8.11
show(m8.1stan) # extract model info include model, DIC, and WAIC 

# R code 8.12 trace plot
plot(m8.1stan)
stancode(m8.1stan) # should print out the stan code for the ruggedness model 

# R code 8.13, something wrong that I could not build the model... 
y <- c(-1,1)
m8.2 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha
  ), 
  data = list(y=y), start = list(alpha=0, sigma=1),
  chains = 2, iter = 4000, wwarmup = 1000
)

library(rethinking)
library('RcppEigen')

# R code 8.14
precis(m8.2)

# R code 8.15
m8.3 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha,
    alpha ~ dnorm(1, 10),
    sigma ~ dcauchy(0, 1)
  ), 
  data = list(y=y), start = list(alpha=0, sigma=1),
  chains = 2, iter = 4000, warmup = 1000)
precis(m8.3)

# R code 8.16
y <- rcauchy(1e4, 0, 5)
mu <- sapply(1:length(y), function(i) sum(y[1:1])/i)
plot(mu, type="l")

# R code 8.17 
y <- rnorm(100, mean = 0, sd = 1) 

# R code 8.18
m8.4 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a1+a2,
    sigma ~ dcauchy(0, 1)
  ), 
  data = list(y=y), start = list(a1=0, a2=0, sigma=1),
  chains = 2, iter = 4000, warmup = 1000)
precis(m8.4)

# R code 8.19
m8.5 <- map2stan(
  alist(
    y~dnorm(mu, sigma),
    mu <- a1 + a2,
    a1 ~ dnorm(0, 10),
    a2 ~dnorm(0, 10),
    sigma ~ dcauchy(0, 1)
  ), 
  data = list(y=y), start = list(a1=0, a2=0, sigma=1),
  chains = 2, iter = 4000, warmup = 1000)
precis(m8.5)

# get C++ compiler for Rstan installation. 
Sys.setenv(MAKEFLAGS = "-j4") 

fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
    return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
                           ' )
fx( 2L, 5 ) # should be 10


















