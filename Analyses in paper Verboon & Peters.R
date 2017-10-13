
## Analyses in paper: Applying the generalised logistic model in single case designs to deal with ceiling effects
## Authors: Peter Verboon & Gjalt-Jorn Peters
## Date: 22-9-2017 

require(minpack.lm)     # contains function nlsLM for nonlinear fitting
require(ggplot2)
require(dplyr)
require(userfriendlyscience)

options(digits=4);

# Parameters definitions:
# x0 = center of x values in the crossover area: point at which y = 0.5(At + Ab)
# Ab = asymptotic base of the function
# At = asymptotic top of the function
# B  = scaling factor, controls steepness of curve (growth rate)
# v  = not used in this study (fixed at v = 1)

# Define formula for S-curve: generalized logistic 

GLF <- "y ~ Ab + (At - Ab)/ (1 + exp(-B*(x-x0)))**(1/v)"    # formula S-curve with scaling factor B and v




####  Definition Generalized Logistic function with scaling factor

genlogf <- function(x, x0 = 10, Ab = 1, At=7, B = 0.5, v = 1) {
  
  y = Ab + ((At - Ab)/ (1 + exp(-B*(x-x0)))**(1/v)) 
  return(y)
  
}    # end function



## Data construction for Example1 


nA <- 5;          # nA : number of measurement in phase A
nB <- 30;         # nB : number of measurement in phase B

x <- c(1:nB)
y <- genlogf(x, x0=10, Ab=1.5, At=6.5, B = 0.3, v = 1) 

yA <- rnorm(nA, 1.5, 0.5)                                   # the first observations in A phase are without effect

y <- c(yA,y)
sdy <- sd(y)

x <- c(1:(nA+nB))
y <- y + rnorm(length(x),0, 0.5*sdy)                        # add random error to curve: dependent varable
y[y < 1] <- 1;  y[y > 7] <- 7                               # add hard floor and ceiling effects


dat <- data.frame(x=x,y=y)
a <- data.frame(x = (31:35),y = rnorm(5, 6.5, 0.2) )        # add aditional random error in last five points
dat1 <- rbind(dat,a)

dat1$Phase <- c(rep(1,5),rep(2,30))

meanDiff(x=dat1$Phase, y= dat1$y)                  ## simple test of phase difference

write.table(dat1, "datExample1.txt", sep="\t")     ## save example outside R


######## End data construction for example



dat1 <- read.table("datExampl1.txt",sep="\t")   
dat1$Phase <- dat1$Phase - 1 


# Figure 1 from paper 

g <- ggplot(dat1) + geom_point(aes(x=x,y=y))                
g  

### Example: linear fit 

lmout <- lm(dat1$y ~ dat1$x)
coef(lmout)
deviance(lmout)
summary(lmout)
acf(residuals(lmout))[1]
dat1$res <-residuals(lmout)

# Figure 2 from paper

g <- ggplot(dat1,aes(x=x,y=y))                   
g <- g + geom_point()
g <- g + labs(x = "measurements points", y = "score", title = " ")
g <- g + geom_smooth(method = lm, size = 1.0, se = TRUE)
g


# Figure 3 from paper

g <- qplot(data = dat1, x, res, xlab = "measurements points", ylab = "residuals")
g <- g + geom_smooth(method = "loess", size = 1.0)
g


### End example: linear fit



### Example Piecewise Regression

# dat1$Phase <- c(rep(1,5),rep(2,30))
# ypre <- dat1[c(1:5),"y"]
# ypost <- dat1[c(6:35),"y"]

meanDiff(x=dat1$Phase, y= dat1$y)                 ## simple test of phase difference

dat1$x0 <- dat1$x - 1                             ## first observation index is set to zero 
nA <- 5
out <- PWreg(dat1[,c("x0","y")],nA = 5, robust=FALSE)        ## piecewise regression, uses function PWreg
yfit <- out[[2]]$fitted.values
ypre <- out[[2]]$coefficients[1] + out[[2]]$coefficients[3]*dat1[c(1:6),"x0"]

out[[2]]$coefficients
confint(out[[2]], c("D","Tc","X3"), level = 0.95)


# Figure 4 from paper

g <- qplot(data = dat1, x0, y, xlab = "measurements points", ylab = "scores")
g <- g + geom_line(aes(x0, y=yfit, group=Phase))
g <- g + geom_vline(xintercept= (nA-.5), colour="red", size = 1.5) 
g <- g + geom_segment(aes(x=nA, xend=nA, y=yfit[(nA+1)], yend=ypre[(nA+1)]),linetype=1, colour = "green", size=1)
g <- g + geom_segment(aes(x=(nA-1), xend=nA, y=ypre[nA], yend=ypre[(nA+1)]),colour="blue", linetype = "dashed")
g <- g + geom_smooth(method="lm", aes(group = Phase))
g

### Linear regression per phase

out0 <- lm(dat1[c(1:5),"y"] ~ dat1[c(1:5),"x"])
coef(out0)
confint(out0, level = 0.95)
dat1$x2 <- dat1$x - 6
out1 <- lm(dat1[c(6:35),"y"] ~ dat1[c(6:35),"x2"])
coef(out1)
confint(out1, level = 0.95)


### Example to show phase effect

a <- rnorm(5, 1.5, 0.8)
b <- rnorm(15, 3.0, 0.8)
y <- c(a,b)
x0 <- c(0:19)
dat2 <- data.frame(cbind(x0,y))
dat2$phase <- c(rep(0,5), rep(1,15))
nA <- 5
ypre <- dat2[c(1:5),"y"]
ypost <- dat2[c(6:35),"y"]

meanDiff(x=dat2$phase, y= dat2$y)

out <- PWreg(dat2[,c("x0","y")],nA = 5, robust=FALSE)  ## piecewise regression
yfit <- out[[2]]$fitted.values
ypre <- out[[2]]$coefficients[1] + out[[2]]$coefficients[3]*dat2[c(1:6),"x0"]

confint(out[[2]], c("D","Tc","X3"), level = 0.95)

# Figure 5 from paper 

g <- qplot(data = dat2, x0, y, xlab = "measurements points", ylab = "scores" )
g <- g + geom_line(aes(x0, y=yfit, group=phase))
g <- g + geom_vline(xintercept= (nA-.5), colour="red", size = 1.5) 
g <- g + geom_segment(aes(x=nA, xend=nA, y=yfit[(nA+1)], yend=ypre[(nA+1)]),linetype=1, colour = "green", size=1)
g <- g + geom_segment(aes(x=(nA-1), xend=nA, y=ypre[nA], yend=ypre[(nA+1)]),colour="blue", linetype = "dashed")
g <- g + geom_smooth(method="lm", aes(group = phase))
g <- g + scale_y_continuous(limits = c(0,6))
g <- g + annotate("text", x = 6.5, y = 2.6, label = "level effect", colour = "black", size=4)
g



###  Example genlog function


# Figure 6 paper

genlog(dat1, Xs=10, Bs=.5, ABs = 1, ATs = 7 , range=8,plot=TRUE)




