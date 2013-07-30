## HIDDEN MARKOV MODEL practice...

library(RHmm)

# Sim up some financial market data...

bear <- rnorm(100, -0.01, 0.20)
bull1 <- rnorm(100, 0.1, 0.10)
bull2 <- rnorm(100, 0.11, 0.08)

# How many discrete states do we want to test??
nStates=2

y <- c(bull1, bear, bull2)
fit <- HMMFit(y, nStates=nStates)
states <- viterbi(fit, y)$states
probs <- forwardBackward(fit, y)

# Plot it up using run length encoding for some nice polygons on the plot
library(RColorBrewer)
plot(y, type='n', las=1, ylab='Data values')
breaks <- rle(states)
mypal <- brewer.pal(length(unique(breaks$values)), 'Pastel1')

switch.xpos <- c(0, cumsum(breaks$lengths))
switch.ypos <- rep(c(min(y)-10, max(y)+10), length.out=length(switch.xpos))   # Arbitrary 10 here
rect(switch.xpos[1:length(switch.xpos)-1], switch.ypos[1:length(switch.ypos)-1],
     switch.xpos[2:length(switch.xpos)], switch.ypos[2:length(switch.ypos)],
     col=mypal[breaks$values])
points(y, type='l', col='black', lwd=1.5)
legend('topleft', title='State', legend=sort(unique(states)), fill=mypal, bg='gray90')
box('plot')


######### TRY a multivariate example:
# Sim up data:
n <- 250
truestates <- rep(c(1,2,1,2), each=n)
x <- c(rnorm(n, 3, 1.5), rnorm(n, 5, 1), rnorm(n, 3, 1.1), rnorm(n, 5.3, 1.5))
y <- c(runif(n, 5, 10), runif(n, 8, 14), runif(n, 7, 13), runif(n, 7.5, 15))

# run the model:
nStates <- 3
fit <- HMMFit(data.frame(x=x, y=y), nStates=nStates)
states <- viterbi(fit, data.frame(x=x, y=y))$states
probs <- forwardBackward(fit, data.frame(x=x, y=y))

# Plot it up using run length encoding for some nice polygons on the plot
library(RColorBrewer)
plot(y, type='n', las=1, ylab='Data values', ylim=c(min(c(x,y)), max(c(x,y))))
breaks <- rle(states)
mypal <- brewer.pal(length(unique(breaks$values)), 'Spectral')

switch.xpos <- c(0, cumsum(breaks$lengths), length(x))
switch.ypos <- rep(c(min(c(x,y))-10, max(c(x,y))+10), length.out=length(switch.xpos))   # Arbitrary 10 here
rect(switch.xpos[1:length(switch.xpos)-1], switch.ypos[1:length(switch.ypos)-1],
     switch.xpos[2:length(switch.xpos)], switch.ypos[2:length(switch.ypos)],
     col=mypal[breaks$values])
points(x, type='l', col='red', lwd=1.5)
points(y, type='l', col='blue', lwd=1.5)

lines(probs$Delta[,1]*15, col='gray70', lty=3, lwd=2)
lines(probs$Delta[,2]*15, col='gray20', lty=3, lwd=2)
axis(4, at=c(0,15), labels=c(0,1), las=1)

legend('topright', title='State', legend=sort(unique(states)), fill=mypal, bg='gray90')
legend('topleft', title='var', legend=c('x', 'y', 'Prob 1', 'Prob 2'), lwd=2,
       col=c('red', 'blue', 'gray70', 'gray20'), lty=c(1,1,3,3))
box('plot')

# abline(v=(n * 1:length(truestates)), col='black', lwd=3, lty=2, lend=1)