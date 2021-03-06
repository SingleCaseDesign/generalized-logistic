
  
 
install.packages('userfriendlyscience');
require('userfriendlyscience');

getDat();




This function loads the Singh data into a dataframe called Singh. This data concerns three participants, and we will only work with one participant in this example. We will therefore extract those lines from the `Signh` dataframe and store that selection in a dataframe called `dat`:
  
  ```{r tutorial-3}
dat <- Singh[Singh$id=='Jason', ];
```

## Piecewise regression

To conduct the piecewise regression analysis, we use function `piecewiseRegr`, where we specify the name of the dataframe to use (`dat`), and in that datarame, the variable names (or column names) of the variables specifying the measurement moment (`timeVar='time'`), the dependent variable (`yVar='score_physical'`), and the variable indicating in which phase that datapoint was measured (`phaseVar='phase'`). This last variable should consist of only two values: the measurements preceding the start of the intervention should have the lowest value, and the measurements following the start of the intervention should have the highest value.

```{r tutorial-4}
piecewiseRegr(dat,
              timeVar='time',
              yVar='score_physical',
              phaseVar='phase');
```

Instead of using the `phaseVar` argument to specify a variable that indicates each measurement moments' phase, it is also possible to specify manually at which moment the intervention too place. To do this, use argument `baselineMeasurements` to specify how many baseline measurements there are. For example, we could 'pretend' that the intervention only started after five measurements (instead of after three measurements):

```{r tutorial-5}
piecewiseRegr(dat,
timeVar='time',
yVar='score_physical',
baselineMeasurements=5);
```

## Generalized Logistic Analysis

The generalized logistic analysis is conducted by using the function `genlog`. The arguments have the same name as in function `piecewiseRegr`, that is, `timeVar`, `yVar`, and `phaseVar`, and it is again possible to use `baselineMeasurements` to manually specify when the intervention took place.

```{r tutorial-6}
genlog(dat,
timeVar='time',
yVar='score_physical',
phaseVar='phase');
```

## Final thoughts and customization

A datafile used for `piecewiseRegr` and/or `genlog` should at the minimum contain two columns: one containing the measurements of the dependent variable, and one containing the measurement moments. This last column can either contain the dates of the measurements or an index ('rank') of the dates. If the measurement moments were roughly equally spaced over time, it doesn't matter much which is used; if the interval between consecutive measurement moments is varied, it is better to specify the exact dates.

It is possible to easily store the produced plot to a file. To do this, provide a path and filename to argument `outputFile`. The size of the exported figure can be specified in `outputWidth` and `outputHeight`, and parameters to tweak the export can be specified in `ggsaveParams`.

For `piecewiseRegr`, the colors used to draw the plot can be changed by providing a `colors` parameter, which must be a list containing five values specifying the desired colors for the regression `pre` intervention, `post` intervention, the `diff`erence between the two phases, the `intervention`, and the `points`.

For `genlog`, the colors used to draw the plot can be changed by providing a `colors` parameter, which must be a list containing five values specifying the desired colors for the upper and lower `bounds`, the sigmoid `curve`, the vertical line showing where the change is maximal (`mid`), the vertical line showing then the `intervention` started, and the `points`.

For both functions, further customization is possible by specifying the ggplot2 `theme` to use, the `pointSize` and `pointAlpha` (the transparency, or rather, opaqueness, of the points),  the `lineSize`. The `genlog` function has an additional parameter `curveMultiplier`, used to multiply the size of the sigmoid curve compared to the other lines.

To use different starting values and bounds when estimating the sigmoid, manually specify values for the `yRange`, `startX`, `startBase`, `startTop`, `startGrowthRate`, and `startV` to set the starting values, and `changeInitiationBounds`, `growthRateBounds`, `baseBounds`, `topBounds`, and `vBounds` to set the bounds. Instead of specifying `baseBounds` and `topBounds`, it's also possible to use `baseMargin` and `topMargin` to specify margins that will then be combined with the range of the dependent variable to set the constraints.


# Part 2: Appendix

This section reproduces the Figures used in the paper.

```{r appendix-setup, include=FALSE}

### Show all R commands
knitr::opts_chunk$set(echo = FALSE);

### Load required packages
require(userfriendlyscience)
safeRequire('ggplot2');
safeRequire('grid');
safeRequire('gridExtra');
safeRequire('viridis');

### Set color and size for plots
defaultLineColor <- "#35B779FF";   ### viridis(4)[3]
defaultBoundsColor <- "#FDE725FF"; ### viridis(4)[4]
defaultCurvecolor <- "#35B779FF";  ### viridis(4)[3]
defaultMidColor <- "#31688EFF";    ### viridis(4)[2]
defaultWidth = 20;
defaultHeight = 16;

```

# Data generation

```{r data-generation}

### Set seed to ensure replicable results
set.seed(20171102);

nA <- 5;          # nA : number of measurement in phase A
nB <- 30;         # nB : number of measurement in phase B

x <- c(1:nB)
y <- genlogFunction(x, x0=10, Ab=1.5, At=6.5, B = 0.3, v = 1)

yA <- rnorm(nA, 1.5, 0.5)                                   # the first observations in A phase are without effect

y <- c(yA,y)
sdy <- sd(y)

x <- c(1:(nA+nB))
y <- y + rnorm(length(x),0, 0.5*sdy)                        # add random error to curve: dependent varable
y[y < 1] <- 1;  y[y > 7] <- 7                               # add hard floor and ceiling effects


dat <- data.frame(x=x,y=y)
a <- data.frame(x = (31:35),y = rnorm(5, 6.5, 0.2) )        # add aditional random error in last five points
dat1 <- rbind(dat,a)

dat1$Phase <- c(rep(0,5),rep(1,35));

meanDiff(x=dat1$Phase, y= dat1$y)                  ## simple test of phase difference

```

# Figure 1

```{r fig1}

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

```

# Example: linear fit

```{r example-linear-fit}

lmout <- lm(dat1$y ~ dat1$x)

paste0("Model deviance: ", formatC(deviance(lmout),format="f",digits=2))
summary(lmout)

paste0("Autocorrelation by lag 1: ", formatC(as.numeric(acf(residuals(lmout), plot=FALSE)[1][[1]]),format="f",digits=2))
dat1$res <-residuals(lmout)

```

# Figure 2

```{r fig2}

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

```

# Figure 3

```{r fig3}

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

```

# Figure 4

```{r fig4}

piecewiseRegr(dat1,
timeVar = 'x',
yVar = 'y',
phaseVar = 'Phase',
outputFile = "figure-4.png",
outputWidth = defaultWidth,
outputHeight = defaultHeight);

```

# Figure 5

```{r fig5}

dat5 <- data.frame(x = 0:19,
y = c(rnorm(5, 1.5, .8),
rnorm(15, 6, .8)),
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
meanDiff(x=dat5$phase, y= dat5$y);

```

# Figure 6

```{r fig6}

genlog(dat1,
timeVar = 'x',
yVar = 'y',
phaseVar = 'Phase',
outputFile = "figure-6.png",
outputWidth = defaultWidth,
outputHeight = defaultHeight);

```

# Figure 7

```{r fig7}

fig7_plots <- lapply(1:6, function(i, x = 1:35, Ab = 1, At = 7,
x0 = 10,
Blist = c(-0.2,-0.5,-1, 0.2, .5, 1)) {
return(ggplot() +
geom_hline(yintercept=Ab, colour=defaultBoundsColor) +
geom_hline(yintercept=At, colour=defaultBoundsColor) +
geom_vline(xintercept=x0, colour=defaultMidColor) +
geom_line(data=data.frame(x=x,
y=genlogFunction(x,
x0=x0,
Ab=Ab,
At=At,
B=Blist[i],
v=1)),
aes(x=x, y=y),
color=defaultCurvecolor,
size = 1) +
theme_minimal() +
scale_y_continuous(breaks=1:7, labels=1:7) +
labs(x = "Measurement points",
y = "Score",
title = paste("Growth rate =", Blist[i])));
});

### Combine plots
figure7 <- grid.arrange(grobs = fig7_plots,
ncol = 2,
as.table=FALSE)

### Draw
grid.newpage();
grid.draw(figure7);

### Store to disk
ggsave(plot = figure7,
filename="figure-7.png",
type='cairo',
width = defaultWidth+6,
height = defaultHeight,
units='cm');

```

# Figure 8

```{r fig8}

data(Singh);

fig8_plots <- lapply(1:6, function(i, dat = Singh) {
tier <- ceiling(i/2);
dv <- names(dat)[5+is.even(i)];
dvLabel <- c("Physical aggression", "Verbal aggression")[1+is.even(i)];
dat <- dat[dat$tier == tier, c('time', 'phase', dv, 'id')];
interventionX <- mean(c(max(dat[dat$phase==min(dat$phase), 'time']),
min(dat[dat$phase==max(dat$phase), 'time'])));
return(ggplot(data =  dat,
aes_string(x = 'time', y=dv)) +
geom_vline(xintercept=interventionX) +
geom_point() +
geom_line(aes(group=phase)) +
theme_minimal() +
labs(x='Weeks',
y = 'Score',
title = paste0(unique(dat$id),
": ",
dvLabel)));
});


### Combine plots
figure8 <- grid.arrange(grobs = fig8_plots,
ncol = 2);

### Draw
grid.newpage();
grid.draw(figure8);

### Store to disk
ggsave(plot = figure8,
filename="figure-8.png",
type='cairo',
width = defaultWidth+6,
height = defaultHeight,
units='cm');

```

# Figure 9

```{r fig9}

fig9_plots <- lapply(1:6, function(i, dat = Singh) {
tier <- ceiling(i/2);
dv <- names(dat)[5+is.even(i)];
dvLabel <- c("Physical aggression", "Verbal aggression")[1+is.even(i)];
dat <- dat[dat$tier == tier, c('time', 'phase', dv, 'id')];

print(dat);

return(genlog(data = dat,
timeVar = 'time',
yVar = dv,
phaseVar = 'phase')$output$plot +
labs(x='Weeks',
y = 'Score',
title=paste0(unique(dat$id),
": ",
dvLabel)));
});


### Combine plots
figure9 <- grid.arrange(grobs = fig9_plots,
ncol = 2);

### Draw
grid.newpage();
grid.draw(figure9);

### Store to disk
ggsave(plot = figure9,
filename="figure-9.png",
type='cairo',
width = defaultWidth+6,
height = defaultHeight,
units='cm');

```

