###########################################################################
###########################################################################
### Dit scriptje hoort bij het artikel "" (van Lankveld, Peters, ...,
### 2016). Het wordt vrijgegeven onder de Creative Commons Naamsvermelding
### 4.0 licentie (http://creativecommons.org/licenses/by/4.0/).
### 
### Het artikel en gerelateerde bronnen (zoals dit scriptje) zijn
### beschikbaar in het Open Science Framework repository op
### https://osf.io/tpxa9/
### 
###########################################################################
###########################################################################

### Downloaden en installeer het R package 'userfriendlyscience', waar de
### therapyMonitor functies deel van uitmaken (deze regel hoeft maar
### één keer te worden uitgevoerd, en daarna kan er een hekje voor worden
### geplaatst om hem voortaan over te slaan).
#install.packages('userfriendlyscience');

### Laadt het 'userfriendlyscience' package in het geheugen
require('userfriendlyscience');

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

###########################################################################
### Start de analyses
###########################################################################

### De therapie startte na sessie 6, en stopte na sessie 34.
### We gaan daarom regels 1:34 analyseren, en we stellen de
### conditiewisseling in vanaf regel 7.

### Kijk eerst alleen naar een enkele variabele, in dit geval positief
### affect.
therapyMonitor(dat, conditionMoment = 7,
               lines = 1:34,
               variableColumn = 'positief_affect',
               timeColumn = 'Tijdstempel');

### Analyseer dan ineens alle variabelen, en schrijf de plotjes en de
### resultaten weg naar een gespecificeerde directory (folder, map).
therapyMonitor.multi(dat, lines=1:34, conditionMoment=7,
                     variableColumn=names(schalen),
                     timeColumn = 'Tijdstempel', outputFiles=TRUE,
                     outputFilePath=
                       'B:/Data/research/therapyMonitor (TvS)/tmp');