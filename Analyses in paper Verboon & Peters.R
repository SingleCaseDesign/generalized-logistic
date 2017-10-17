## Analyses in paper: Applying the generalised logistic model in single case designs to deal with ceiling effects
## Authors: Peter Verboon & Gjalt-Jorn Peters
## Date: 22-9-2017 

### Load required packages
require(userfriendlyscience)
safeRequire('ggplot2');

### Set color and size for plots
defaultLineColor <- "#35B779FF"; ### viridis(4)[3]
defaultWidth = 20;
defaultHeight = 16;

### Set seed to ensure replicable results
set.seed(20171017);

########################################################################
### Data construction for Example1
###
### (retained solely for documentation purposes; data is read from disk)
########################################################################
# 
# nA <- 5;          # nA : number of measurement in phase A
# nB <- 30;         # nB : number of measurement in phase B
# 
# x <- c(1:nB)
# y <- genlogf(x, x0=10, Ab=1.5, At=6.5, B = 0.3, v = 1) 
# 
# yA <- rnorm(nA, 1.5, 0.5)                                   # the first observations in A phase are without effect
# 
# y <- c(yA,y)
# sdy <- sd(y)
# 
# x <- c(1:(nA+nB))
# y <- y + rnorm(length(x),0, 0.5*sdy)                        # add random error to curve: dependent varable
# y[y < 1] <- 1;  y[y > 7] <- 7                               # add hard floor and ceiling effects
# 
# 
# dat <- data.frame(x=x,y=y)
# a <- data.frame(x = (31:35),y = rnorm(5, 6.5, 0.2) )        # add aditional random error in last five points
# dat1 <- rbind(dat,a)
# 
# ### Changed '30' to '35' (2017-10-16, Gjalt-Jorn)
# dat1$Phase <- c(rep(1,5),rep(2,35))
# 
# meanDiff(x=dat1$Phase, y= dat1$y)                  ## simple test of phase difference
# 
# write.table(dat1, "datExample1.txt", sep="\t")     ## save example outside R

########################################################################
## Example 1: linear fit
########################################################################

### Load data
dat1 <- read.table("datExample1.txt",sep="\t")   
dat1$Phase <- dat1$Phase - 1 

### Figure 1 from paper
figure1 <- ggplot(dat1) +
  geom_point(aes(x=x,y=y)) +
  coord_cartesian(ylim=c(0,8)) +
  theme_minimal() +
  labs(x = "measurements points", y = "score", title = NULL);

print(figure1);

### Store to disk
ggsave(plot = figure1,
       filename="figure-1.png",
       type='cairo',
       width = defaultWidth,
       height = defaultHeight,
       units='cm');

### Example: linear fit 
lmout <- lm(dat1$y ~ dat1$x)
coef(lmout)
deviance(lmout)
summary(lmout)
acf(residuals(lmout))[1]
dat1$res <-residuals(lmout)

### Figure 2 from paper
figure2 <- ggplot(dat1, aes(x=x,y=y)) +
  geom_point() +
  geom_smooth(method = "lm",
              size = 1,
              se = TRUE,
              color=defaultLineColor,
              fill=defaultLineColor) +
  coord_cartesian(ylim=c(0,8)) +
  theme_minimal() +
  labs(x = "measurements points", y = "score", title = NULL);

print(figure2);

### Store to disk
ggsave(plot = figure2,
       filename="figure-2.png",
       type='cairo',
       width = defaultWidth,
       height = defaultHeight,
       units='cm');

### Figure 3 from paper

figure3 <- ggplot(dat1, aes(x=x, y=res)) +
  geom_point() +
  geom_smooth(method = "loess",
              size = 1,
              color=defaultLineColor,
              fill=defaultLineColor) +
  theme_minimal() +
  labs(x = "measurements points",
       y = "residuals");

print(figure3);

### Store to disk
ggsave(plot = figure3,
       filename="figure-3.png",
       type='cairo',
       width = defaultWidth,
       height = defaultHeight,
       units='cm');

########################################################################
## Example 2: Piecewise regression without phase effect
########################################################################

# dat1$Phase <- c(rep(1,5),rep(2,30))
# ypre <- dat1[c(1:5),"y"]
# ypost <- dat1[c(6:35),"y"]

meanDiff(x=dat1$Phase, y= dat1$y)                 ## simple test of phase difference

piecewiseRegr(dat1,
              timeVar = 'x',
              yVar = 'y',
              phaseVar = 'Phase',
              outputFile = "figure-4.png",
              outputWidth = defaultWidth,
              outputHeight = defaultHeight);

### Linear regression per phase

out0 <- lm(dat1[c(1:5),"y"] ~ dat1[c(1:5),"x"])
coef(out0)
confint(out0, level = 0.95)
dat1$x2 <- dat1$x - 6
out1 <- lm(dat1[c(6:35),"y"] ~ dat1[c(6:35),"x2"])
coef(out1)
confint(out1, level = 0.95)

########################################################################
## Example 3: Piecewise regression with phase effect
########################################################################

dat5 <- data.frame(x = 0:19,
                   y = c(rnorm(5, 1.5, .8),
                         rnorm(15, 3, .8)),
                   phase = c(rep(0,5),
                             rep(1,15)));

piecewiseRegr(dat5,
              timeVar = 'x',
              yVar = 'y',
              phaseVar = 'phase',
              outputFile = "figure-5.png",
              outputWidth = defaultWidth,
              outputHeight = defaultHeight);

### T-test for difference between means
meanDiff(x=dat2$phase, y= dat2$y);

########################################################################
## Example 4: Generalized Logistic Analysis
########################################################################

genlog(dat1,
       timeVar = 'x',
       yVar = 'y',
       phaseVar = 'Phase',
       outputFile = "figure-6.png",
       outputWidth = defaultWidth,
       outputHeight = defaultHeight);

