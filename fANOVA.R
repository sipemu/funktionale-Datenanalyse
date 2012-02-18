###
#
# R-Code zum Vortrag fANOVA
#
# Code übernommen von Ramsay und Silverman: 
# http://ego.psych.mcgill.ca/misc/fda/downloads/FDAfuns/R/inst/scripts/fdarm-ch10.R
# mit Änderungen bei der Grafik
#
# Das Ganze ist aufgeteilt nach knitr-Abschnitten
#
###

################################################################################
################################################################################
################################################################################

## @knitr CA_diet

data(seabird)

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

meanall <- apply(meanCounts, 1, mean)
meanall <- log10(meanall[!is.na(meanall)])

meltmean <- data.frame(year = rep(yearObs, 3), 
                       logCount = c(meanShellfish, meanFish, meanall), 
                       diet = rep(c("Krustentiere", "Fische", "Gesamt"), 
                                  each = length(yearObs)))

# Vorbereitung für die Grafiken
dflogCounts <- data.frame(t(logCounts))
dflogCounts$species <- row.names(dflogCounts)
dflogCounts$diet[shellfishindex] <- "Krustentiere"
dflogCounts$diet[fishindex] <- "Fische"
names(dflogCounts) <- c(as.character(yearObs), "species", "diet")
meltlogCounts <- melt(dflogCounts, id=c("diet", "species"), measure.vars=1:19)
meltlogCounts <- meltlogCounts[order(meltlogCounts$species, meltlogCounts$diet), ]
row.names(meltlogCounts) <- NULL
names(meltlogCounts)[3] <- "year"
names(meltlogCounts)[4] <- "logCount"
meltlogCounts$year <- rep(yearObs, 13) 

################################################################################
################################################################################
################################################################################

# Grafiken 
## @knitr GR_Krustentier
theme_set(theme_bw())
ggplot(subset(meltlogCounts, diet=="Krustentiere")) + 
  geom_line(aes(x=year, y=logCount, group=species, lty=species), 
            size=2) + 
  geom_line(aes(x=year, y=logCount), 
            data=subset(meltmean, diet=="Krustentiere"), 
            size=2, col='RoyalBlue') +
  geom_line(aes(x=year, y=logCount), 
            data=subset(meltmean, diet=="Gesamt"), 
            size=2, col='#E69F00') +
  opts(legend.position="none", 
       title="Faktorstufe: Krustentier/Weichtier",
       axis.title.y = theme_text(size = 12, angle=90), 
       axis.title.x = theme_text(size = 12, vjust=0),
       axis.text.x = theme_text(size = 12),
       axis.text.y= theme_text(size = 12),
       plot.title=theme_text(size=18)) +
  xlab("") + ylab("") + ylim(c(-1.5, 1.7))

################################################################################
################################################################################
################################################################################

## @knitr GR_Fisch
theme_set(theme_bw())
ggplot(subset(meltlogCounts, diet=="Fische")) + 
  geom_line(aes(x=year, y=logCount, group=species, lty=species), 
            size=2) + 
  geom_line(aes(x=year, y=logCount), 
            data=subset(meltmean, diet=="Fische"), 
            size=2, col='RoyalBlue') +
  geom_line(aes(x=year, y=logCount), 
            data=subset(meltmean, diet=="Gesamt"), 
            size=2, col='#E69F00') +
  opts(legend.position="none", 
       title="Faktorstufe: Fisch",
       axis.title.y = theme_text(size = 12, angle=90), 
       axis.title.x = theme_text(size = 12, vjust=0),
       axis.text.x = theme_text(size = 12),
       axis.text.y= theme_text(size = 12),
       plot.title=theme_text(size=18)) +
  xlab("") + ylab("") + ylim(c(-1.5, 1.7))

################################################################################
################################################################################
################################################################################

## @knitr GR_beides
theme_set(theme_bw())
ggplot(meltlogCounts) + 
  geom_line(aes(x=year, y=logCount, group=species, lty=species)) + 
  geom_point(aes(x=year, y=logCount, group=species), 
             shape=2, size=1) + 
  geom_line(aes(x=year, y=logCount, col=diet), 
            data=meltmean, 
            size=1) +
  facet_grid(.~diet) + 
  opts(legend.position="none") +
  scale_colour_hue("", l=40) + 
  xlab("") + ylab("") + ylim(c(-1.5, 1.7))


################################################################################
################################################################################
################################################################################

## @knitr CA_fANOVA

###
#
# fANOVA
#
###

###
#
#  Hier wird der Mittelwert über die Gebiet nach Bucht berechnet,
#  so dass man pro Vogelart 2 Beobachtungen pro Jahr erhält
#  Zwei von diesen sind 0; wird ersetzt durch 1/(2n)
#
###

meanCounts2 <- matrix(NA, 20, 26)

for(i in 1:20) {
  for (j in 1:2) {
    sel <- (UU$Year == rownames(meanCounts)[i] & as.character(UU$Bay) == orte[j])
    meanCountsij = sapply(UU[sel, birds], mean, na.rm=TRUE)
    n <- sum(sel)
    if (n > 0) {
      meanCountsij[meanCountsij == 0] <- 1/(2*n)
    }
    meanCounts2[i, (j-1)*13+(1:13)] <- meanCountsij
  }
}

###
#
# Aufzeichnungnen von 1986 bis 2005 ohne 1998
#
###
selYear2 <- !is.na(meanCounts2[, 1])
yearCode <- (1:20)[selYear2]
all.equal(yearCode, c(1:12, 14:20))

logCounts2 <- log10(meanCounts2[selYear2, ])

###
#
#  Exakte Darstellung der Kurven mittels einer polygonalen Basis
#
###
birdbasis <- create.polygonal.basis(yearCode)
birdlist2 <- smooth.basis(yearCode, logCounts2, birdbasis)

birdfd2 <- birdlist2$fd

###
#
# Aufstellen der Design-Matrix
#
###

Zmat0 <- matrix(0, 26, 15)

#  \mu (t)

Intercept <- rep(1, 26)

#  Nahrungseffekt:  

foodindex <- c(1, 2, 5, 6, 12, 13)
fooddummy <- c(2*rep(1:13 %in% foodindex, 2)-1)

# Vogelarteffekt

birddummy <- diag(rep(1,13))
birdvarbl <- rbind(birddummy, birddummy)

#  Fülle die Design-Matrix

Zmat0[ , 1] <- Intercept
Zmat0[ , 2] <- fooddummy
Zmat0[ , 3:15] <- birdvarbl

#  Zwei dummy Beobachtungen werden zu den funktionalen Daten hinzugefügt
#  zusätzliche Spalten werden in der Design-Matrix hinzugefügt, um die 
#  Vogelarteneffekte auf die jeweilige Bucht zu beschränken um den Effekt
#  innerhalb der Gruppe auf Null zu beschränken

birdfd3 <- birdfd2
birdfd3$coefs <- cbind(birdfd3$coefs, matrix(0, 19, 2))

Zmat <- rbind(Zmat0, matrix(0, 2, 15))
Zmat[27, shellfishindex+2] <- 1
Zmat[28, fishindex+2] <- 1

p <- 15
xfdlist <- vector("list", p)
names(xfdlist) <- c("const", "diet", birds)
betalist <- xfdlist
for (j in 1:p) {
  xfdlist[[j]] = Zmat[ , j]
} 

#  set up the functional parameter object for (the regression fns.
#  use cubic b-spline basis for intercept and food coefficients

betabasis1 <- create.bspline.basis(c(1,20), 21, 4, yearCode)
lambda <- 10
betafdPar1 <- fdPar(betabasis1, 2, lambda)
betalist[[1]] <- betafdPar1
betalist[[2]] <- betafdPar1
betabasis2 <- create.constant.basis(c(1, 20))
betafdPar2 <- fdPar(betabasis2)
for (j in 3:15) {
  betalist[[j]] <- betafdPar2
} 

birdRegress <- fRegress(birdfd3, xfdlist, betalist)
betaestlist <- birdRegress$betaestlist


betafdPar1$lambda <-  10^0.5
for (j in 1:2) betalist[[j]] <-  betafdPar1

#  carry out the functional regression analysis
fitShellfish.5 <- fRegress(birdfd3, xfdlist, betalist)
birdYhatmat <- eval.fd(yearCode, fitShellfish.5$yhatfdobj$fd[1:26])
rmatb <- logCounts2 - birdYhatmat
SigmaEb <- var(t(rmatb))

# FRes <- Fperm.fd(birdfd3, xfdlist, betalist) Dauert ewig

y2cMap.bird <- birdlist2$y2cMap

birdStderrList <- fRegress.stderr(fitShellfish.5, y2cMap.bird, SigmaEb)
birdBeta.sdList <- birdStderrList$betastderrlist


################################################################################
################################################################################
################################################################################

## @knitr GR_ANOVA_mu
mu <- eval.fd(yearCode, betaestlist$const$fd)
sd <- eval.fd(yearCode, birdBeta.sdList[[1]]) 
sd.lo <- mu-2*sd
sd.up <- mu+2*sd
regcoeffmu <- data.frame(year=yearObs,
                         mu=mu,
                         sd.lo=sd.lo,
                         sd.up=sd.up)
# warum auch immer der die namen ändert?
theme_set(theme_bw())
ggplot(regcoeffmu) +
  geom_line(aes(x=year, y=mu), size=2) +
  geom_line(aes(x=year, y=rep1), lty=4, size=2) +
  geom_line(aes(x=year, y=rep1.1), lty=4, size=2) +
  opts(legend.position="none", 
       axis.title.y = theme_text(size = 12, angle=90), 
       axis.title.x = theme_text(size = 12, vjust=0),
       axis.text.x = theme_text(size = 16),
       axis.text.y= theme_text(size = 16)) +
  xlab("") + ylab("")



################################################################################
################################################################################
################################################################################

## @knitr GR_ANOVA_alpha
alpha <- eval.fd(yearCode, betaestlist$diet$fd)
sd <- eval.fd(yearCode, birdBeta.sdList[[2]])
sd.lo <- alpha-2*sd
sd.up <- alpha+2*sd
regcoeffalpha <- data.frame(year=yearObs,
                            alpha=alpha,
                            sd.lo=sd.lo,
                            sd.up=sd.up)

# warum auch immer der die namen ändert?
theme_set(theme_bw())
ggplot(regcoeffalpha) +
  geom_line(aes(x=year, y=alpha), size=2) +
  geom_line(aes(x=year, y=rep1), lty=4, size=2) +
  geom_line(aes(x=year, y=rep1.1), lty=4, size=2) +
  geom_hline(aes(yintercept=0), lty=2, size=1) +
  opts(legend.position="none", 
       axis.title.y = theme_text(size = 12, angle=90), 
       axis.title.x = theme_text(size = 12, vjust=0),
       axis.text.x = theme_text(size = 16),
       axis.text.y= theme_text(size = 16)) +
         xlab("") + ylab("")

