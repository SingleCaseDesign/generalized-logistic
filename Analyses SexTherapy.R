
require('userfriendlyscience');
require(ggplot2)
library(plyr)

### Laadt het 'googlesheets' package, en, als het nog niet is
### geinstalleerd, download en installeer het dan eerst. Doe hetzelfde met
### het 'httpuv' package, dat het gebruik van 'googlesheets' net iets
### gebruikersvriendelijker maakt.
safeRequire('googlesheets');
safeRequire('httpuv');

###########################################################################
### Lees de spreadsheet van Google Docs waar Google Forms de antwoorden
### heeft opgeslagen
###########################################################################

### Open de spreadsheet met de titel "TherapyMonitor voorbeeld formulier
### (Responses)", download deze, en sla hem op in dataframe 'dat'.
### Het 'googlesheets' package opent, indien nodig, eerst een web-browser
### zodat er ingelogt kan worden met een Google account die toegang heeft
### tot de betreffende spreadsheet (om handmatig van accounts te switchen,
### mocht dat nodig zijn, kan 'gs_auth(new_user=TRUE)' gebruikt worden).

dat <- data.frame(gs_read(gs_url('https://docs.google.com/spreadsheets/d/1w1KkBJF5kHbITacpCrFMTKrFm4pHBSpnGOc6qV3TsiA/edit?usp=sharing')));

### Bekijk de variabelenamen (kolom namen) in de dataframe (die hetzelfde
### zijn als die van de spreadsheet).
names(dat);

###########################################################################
### Bereid de data voor op de verwerking
###########################################################################

### Inverteer vragen die gespiegeld zijn, zodat alle vragen in de juiste
### richting staan.
dat <- invertItems(dat, c(22, 25, 28, 31));

### Specificeer welke vragen samen variabelen vormen.
schalen <- list(positief_affect = c(3, 6, 9, 10, 13),
                negatief_affect = c(4, 5, 7, 8, 11, 12, 14),
                zelfwaardering = c(21, 22, 23),
                intimiteit = 24:31,
                erectie_masturbatie = 47:49,
                erectie_partnerseks = 56:58,
                beleving_masturbatie = c(50, 52, 53),
                beleving_partnerseks = c(59, 61, 61));

### Voeg de schalen toe aan de dataframe
dat <- makeScales(dat, schalen);


## Rename the data

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

# add new combined variabele

SexTherapy <- makeScales(SexTherapy, list(erectionCombined = 7:8))

########
# change position of case 7 to right temporal position if date is correct (time: july 5th)

SexTherapy2 <- SexTherapy[c(1:6,8:13,7,14:38),]
SexTherapy2$measurementNumber <- c(1:38)

#######

## Positive Affect

examine(SexTherapy2$positiveAffect)
a <- genlog (SexTherapy2,
             timeVar='measurementNumber',
             yVar='positiveAffect',
             baselineMeasurements = 6,
             startBase = 3,
             startTop = 6,
             baseBounds = c(1,3),
             topBounds = c(5,7))

a$output$plot + labs(x = "Measurement points",y = "positiveAffect")
a$output$dat

b <- piecewiseRegr(data = SexTherapy2,
                   yVar = "positiveAffect", 
                   timeVar='measurementNumber', 
                   baselineMeasurements = 6)
b$output


## Negative Affect

examine(SexTherapy$negativeAffect)
a <- genlog(SexTherapy2,
           timeVar='measurementNumber',
           yVar='negativeAffect',
           baselineMeasurements = 6,
           startBase = 1,
           startTop = 5,
           baseBounds = c(1,3),
           topBounds = c(4,6))

a$output$plot + labs(x = "Measurement points",y = "negativeAffect")
a$output$dat

b <- piecewiseRegr(data = SexTherapy2,
                   yVar = "negativeAffect", 
                   timeVar='measurementNumber', 
                   baselineMeasurements = 6)
b$output


# Self Esteem 

# in paper

examine(SexTherapy2$selfEsteem)
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

b <- piecewiseRegr(data = SexTherapy2,
                   yVar = "selfEsteem", 
                   timeVar='measurementNumber', 
                   baselineMeasurements = 6)
b$output

# Intimacy

examine(SexTherapy$intimacy)
a <- genlog (SexTherapy2,
             timeVar='measurementNumber',
             yVar='intimacy',
             startBase = 4,
             startTop = 6,
             baselineMeasurements = 6,
             startGrowthRate = 2,
             changeInitiationBounds = NULL,
             baseBounds = c(2,5),
             topBounds = c(4,7))
             
a$output$plot + labs(x = "Measurement points",y = "intimacy")
a$output$dat

b <- piecewiseRegr(data = SexTherapy2,
                   yVar = "intimacy", 
                   timeVar='measurementNumber', 
                   baselineMeasurements = 6)
b$output$coef
b$output$ES
b$output$Rsq.model
b$output$deviance
b$output$plot

# This is a nice example

# Erection Masturbation

examine(SexTherapy$erectionMasturbation)
a <- genlog (SexTherapy2,
             timeVar='measurementNumber',
             yVar='erectionMasturbation',
             baselineMeasurements = 6,
             startBase = 3,
             startTop = 6)
a$output$plot + labs(x = "Measurement points",y = "erectionMasturbation")
a$output$dat


b <- piecewiseRegr(data = SexTherapy2,
                   yVar = "erectionMasturbation", 
                   timeVar='measurementNumber', 
                   baselineMeasurements = 6)
b$output$coef
b$output$ES
b$output$Rsq.model
b$output$deviance
b$output$plot


# erection Partner sex

# only three points in A phase

examine(SexTherapy$erectionPartnersex)
a <- genlog (SexTherapy2,
             timeVar='measurementNumber',
             yVar='erectionPartnersex',
             baselineMeasurements = 6,
             startGrowthRate = NULL,
             changeInitiationBounds = NULL,
             baseBounds = c(2,3),
             topBounds = c(4,6))
a$output$plot + labs(x = "Measurement points",y = "erectionPartnersex")
a$output$dat


# Experience Masturbation

# remove missings

SexTherapy3 <- subset(SexTherapy2,!is.na(SexTherapy2$experienceMasturbation))

examine(SexTherapy$experienceMasturbation)
a <- genlog (SexTherapy3,
             timeVar='measurementNumber',
             yVar='experienceMasturbation',
             baselineMeasurements = 6,
             startGrowthRate = 2,
             startBase = 3.5,
             startTop = 5,
             baseBounds = c(3,5),
             topBounds = c(4,6))
a$output$plot + labs(x = "Measurement points",y = "experienceMasturbation")
a$output$dat


b <- piecewiseRegr(data = SexTherapy3,
                   yVar = "experienceMasturbation", 
                   timeVar='measurementNumber', 
                   baselineMeasurements = 6)
b$output$coef
b$output$ES
b$output$Rsq.model
b$output$deviance
b$output$plot


# only three points in A phase

examine(SexTherapy$experiencePartnerSex)
a <- genlog (SexTherapy2,
             timeVar='measurementNumber',
             yVar='experiencePartnerSex',
             baselineMeasurements = 6,
             startBase = 3,
             startTop = 6)
a$output$plot + labs(x = "Measurement points",y = "experiencePartnerSex")
a$output$dat

# Erection Combined

SexTherapy2 <- subset(SexTherapy2,  !is.na(SexTherapy2$erectionCombined))

examine(SexTherapy$erectionCombined)
a <- genlog (SexTherapy2,
             timeVar='measurementNumber',
             yVar='erectionCombined',
             baselineMeasurements = 6,
             startBase = 3,
             startTop = 4,
             baseBounds = c(2,3),
             topBounds = c(3,5))
a$output$plot + labs(x = "Measurement points",y = "erectionCombined")
a$output$dat


b <- piecewiseRegr(data = SexTherapy2,
                   yVar = "erectionCombined", 
                   timeVar='measurementNumber', 
                   baselineMeasurements = 6)
b$output




## There is a strong positive correlation between PosAff and NegAff ????


