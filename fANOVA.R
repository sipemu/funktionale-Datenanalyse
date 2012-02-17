## @knitr GR_DIET

# R-Code zum Vortrag fANOVA
data(seabird)

# Code von Ramsay und Silverman 
# http://ego.psych.mcgill.ca/misc/fda/downloads/FDAfuns/R/inst/scripts/fdarm-ch10.R

# Auswahl der beiden Orte
orte <- c('Uganik', 'Uyak')
sel <- seabird$Bay %in% orte
UU <- seabird[sel, ]

# Sortiere Vögel mit mehr als 2 NAs aus
NAs <- sapply(UU, function(x) sum(is.na(x)))
NAs. <- which(NAs > 2)
birdindex <- (1:15)[-NAs.]
birds <- names(UU)[birdindex]

#  Berechne den Mittelwert über die Orte und Profile
meanCounts <- matrix(NA, 20, 13)
dimnames(meanCounts) <- list(1986:2005, birds)
for(i in 1:20) {
  sel <- (UU$Year == rownames(meanCounts)[i])
  meanCounts[i, ] <- sapply(UU[sel, birds], mean, na.rm = TRUE)
}

selYear <- !is.na(meanCounts[ , 1])
logCounts <- log10(meanCounts[selYear, ])

# Zeitvektoren in Jahren und Indizes 1:20
yearObs <- as.numeric(rownames(logCounts))
yearCode <- (1:20)[selYear]

# Aufteilung in die jeweiligen Ernährung
shellfishindex <- c(1,2,5,6,12,13) # Krustentiere
fishindex <- (1:13)[-shellfishindex] # Fische

# Berechnung der Mittelwerte nach Ernährung
meanShellfish <- apply(meanCounts[ , shellfishindex], 1, mean)
meanShellfish <- log10(meanShellfish[!is.na(meanShellfish)])

meanFish <- apply(meanCounts[ , fishindex], 1, mean)
meanFish <- log10(meanFish[!is.na(meanFish)])

meltmean <- data.frame(year = rep(yearObs, 2), 
                       logCount = c(meanShellfish, meanFish), 
                       diet = rep(c("Krustentiere", "Fische"), each = length(yearObs)))

# Vorbereitung für die Grafiken
dflogCounts <- data.frame(t(logCounts))
dflogCounts$species <- row.names(dflogCounts)
dflogCounts$diet[shellfishindex] <- "Krustentiere"
dflogCounts$diet[fishindex] <- "Fische"
names(dflogCounts) <- c(as.character(yearObs), "species", "diet")
meltlogCounts <- melt(dflogCounts, id = c("diet", "species"), measure.vars=1:19)
meltlogCounts <- meltlogCounts[order(meltlogCounts$species, meltlogCounts$diet), ]
row.names(meltlogCounts) <- NULL
names(meltlogCounts)[3] <- "year"
names(meltlogCounts)[4] <- "logCount"
meltlogCounts$year <- rep(yearObs, 13) 

# Grafiken 
## @knitr GR_Krustentier
theme_set(theme_bw())
ggplot(subset(meltlogCounts, diet=="Krustentiere")) + 
  geom_line(aes(x=year, y=logCount, group=species, lty=species), size=1) + 
  geom_line(aes(x=year, y=logCount), data=subset(meltmean, diet=="Krustentiere"), size=1,col='RoyalBlue') +
  opts(legend.position="none", title="Ernährung durch Krustentiere") +
  xlab("") + ylab("") + ylim(c(-1.5, 1.7))

## @knitr GR_Fisch
theme_set(theme_bw())
ggplot(subset(meltlogCounts, diet=="Fische")) + 
  geom_line(aes(x=year, y=logCount, group=species, lty=species), size=1) + 
  geom_line(aes(x=year, y=logCount), data=subset(meltmean, diet=="Fische"), size=1, col='RoyalBlue') +
  opts(legend.position="none", title="Ernährung durch Fische") +
  xlab("") + ylab("") + ylim(c(-1.5, 1.7))

## @knitr GR_beides
theme_set(theme_bw())
ggplot(meltlogCounts) + 
  geom_line(aes(x=year, y=logCount, group=species, lty=species)) + 
  geom_point(aes(x=year, y=logCount, group=species), shape=2, size=1) + 
  geom_line(aes(x=year, y=logCount, col=diet), data=meltmean, size=1) +
  facet_grid(.~diet) + opts(legend.position="none") +
  scale_colour_hue("", l=40) + xlab("") + ylab("") + ylim(c(-1.5, 1.7))
