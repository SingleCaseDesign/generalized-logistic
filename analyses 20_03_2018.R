
dat <- subset(Singh, tier==3)

genlog(data=dat, yVar = "score_physical", timeVar = "time", baselineMeasurements = 6)
genlog(data=dat, yVar = "score_verbal", timeVar = "time", baselineMeasurements = 6)


piecewiseRegr(data=dat, yVar = "score_physical", timeVar = "time", baselineMeasurements = 6)
piecewiseRegr(data=dat, yVar = "score_verbal", timeVar = "time", baselineMeasurements = 6)




## Deviances (respectively: GENLOG PW)

Jason, PH      2.37      6.03
JAson, Verb   44.07     24.77

Michael, PH    3.32     6.41
Michael, Verb 20.31    24.10

Tim, PH       23.44    16.79
Tim, Verb     75.38    65.43




n <- nrow(dat1)
D <- c(rep(0,nA),rep(1,n-nA))                # dummy to indicate intervention phase
Tc <- (dat1[,1])  - nA
X3 <- D*(Tc-nA-1)                                  # trend term for phase B  (see Huitema & Kean, 2000)
D0 <- (!D)*1

weights <- rep(1,n)

      
# fit piecewise model

out <- lm(dat1[,2] ~ D + Tc + X3, weights = weights)
out1 <- lm(dat1[,2] ~  Tc + X3, weights = weights)
out1
out
