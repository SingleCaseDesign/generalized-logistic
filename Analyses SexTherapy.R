
require('userfriendlyscience');
require(ggplot2)
library(plyr)

SexTherapy <- dat[,c(1,81:88)]

SexTherapy <- rename(SexTherapy,c("Tijdstempel" = "time",
                    "positief_affect" = "positiveAffect",
                    "negatief_affect" = "negativeAffect", 
                    "zelfwaardering" = "selfEsteem",
                    "intimiteit" = "intimacy", 
                    "erectie_masturbatie" = "erectionMasturbation",
                    "erectie_partnerseks" = "erectionPartnersex" ,
                    "beleving_masturbatie"= "experienceMasturbation",
                    "beleving_partnerseks" = "experiencePartnerSex"))
SexTherapy$measurementNumber <- c(1:38)
SexTherapy <- SexTherapy[,c(1,10,2:9)]



examine(SexTherapy$positiveAffect)
a <- genlog (SexTherapy2,
             timeVar='measurementNumber',
             yVar='positiveAffect',
             baselineMeasurements = 6,
             startBase = 3,
             startTop = 6)
a$output$plot + labs(x = "Measurement points",y = "positiveAffect")
a$output$dat


examine(SexTherapy$negativeAffect)
genlog(SexTherapy2,
       timeVar='measurementNumber',
       yVar='negativeAffect',
       baselineMeasurements = 6,
       startBase = 2,
       startTop = 6)
a$output$plot + labs(x = "Measurement points",y = "negativeAffect")
a$output$dat


## analyseer ik deze met function genlog raw dat is het resultaat anders, R2 = .26

examine(SexTherapy$selfEsteem)
a <- genlog (SexTherapy2,
       timeVar='measurementNumber',
       yVar='selfEsteem',
       baselineMeasurements = 6,
       startBase = 2,
       startTop = 6,
       baseBounds = c(1,4),
       topBounds = c(4,7))
a$output$plot + labs(x = "Measurement points",y = "selfEsteem")
a$output$dat



examine(SexTherapy$intimacy)
a <- genlog (SexTherapy2,
             timeVar='measurementNumber',
             yVar='intimacy',
             baselineMeasurements = 6,
             startBase = 4,
             startTop = 7)
a$output$plot + labs(x = "Measurement points",y = "intimacy")
a$output$dat

# This is a nice example

examine(SexTherapy$erectionMasturbation)
a <- genlog (SexTherapy2,
             timeVar='measurementNumber',
             yVar='erectionMasturbation',
             baselineMeasurements = 6,
             startBase = 3,
             startTop = 6)
a$output$plot + labs(x = "Measurement points",y = "erectionMasturbation")
a$output$dat

# only three points in A phase

examine(SexTherapy$erectionPartnersex)
a <- genlog (SexTherapy2,
             timeVar='measurementNumber',
             yVar='erectionPartnersex',
             baselineMeasurements = 6,
             startBase = 2.5,
             startTop = 6)
a$output$plot + labs(x = "Measurement points",y = "erectionPartnersex")
a$output$dat

# convergence problems

examine(SexTherapy$experienceMasturbation)
a <- genlog (SexTherapy,
             timeVar='measurementNumber',
             yVar='experienceMasturbation',
             baselineMeasurements = 6,
             startBase = 3,
             startTop = 5)
a$output$plot + labs(x = "Measurement points",y = "experienceMasturbation")
a$output$dat

# only three points in A phase

examine(SexTherapy$experiencePartnerSex)
a <- genlog (SexTherapy,
             timeVar='measurementNumber',
             yVar='experiencePartnerSex',
             baselineMeasurements = 6,
             startBase = 3,
             startTop = 6)
a$output$plot + labs(x = "Measurement points",y = "experiencePartnerSex")
a$output$dat


########
# change positiion of case 7 to right temporal position if date is correct (time: july 5th)

SexTherapy2 <- SexTherapy[c(1:6,8:13,7,14:38),]
SexTherapy2$measurementNumber <- c(1:38)

#######

## There is a strong positive correlation between PosAff and NegAff ????


