################################################################################
################################################################################
################################################################################

## @knitr GR_NIRspektrum
data(Moisturespectrum) 
set.seed(1)
ind <- sample(1:100, 5)
fat.y <- Moisturespectrum$y[ , ind]
fat.df <- as.data.frame(fat.y)
fat.df$x <- Moisturespectrum$x
fat.df <- melt(fat.df, id="x")

colours <- rainbow_hcl(5, start=30, end=300)
theme_set(theme_bw())
ggplot(fat.df) + 
  geom_point(aes(x=x, y=value, group=variable, col=variable), size=2) +
  scale_color_manual("", values=colours) +
  xlab("") + ylab("") +
  opts(legend.position="none",
       axis.title.y = theme_text(size = 12, angle=90), 
       axis.title.x = theme_text(size = 12, vjust=0),
       axis.text.x = theme_text(size = 16),
       axis.text.y= theme_text(size = 16))

################################################################################
################################################################################
################################################################################

## @knitr CA_canada
smallbasis  <- create.fourier.basis(c(0, 365), 65)
harmaccelLfd365 <- vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))
index <- (1:35)[CanadianWeather$place == "Vancouver"]

VanPrec <- CanadianWeather$dailyAv[ , index, "Precipitation.mm"]
lambda <- 1e4
dayfdPar <- fdPar(smallbasis, harmaccelLfd365, lambda)
sm <- smooth.pos(day.5, VanPrec, dayfdPar)
VanPrecPosFit1 <- eval.posfd(day.5, sm$Wfdobj)

################################################################################
################################################################################
################################################################################

## @knitr GR_canada

temp.df <- data.frame(x=rep(1:365, 2), 
                      y=c(VanPrec, VanPrecPosFit1), 
                      id=rep(c("1", "2"), each=365))

theme_set(theme_bw())
ggplot() + 
  geom_point(aes(x=x, y=y), data=subset(temp.df, id=="1"),
             size=2, col="RoyalBlue") +
  geom_line(aes(x=x, y=y), data=subset(temp.df, id=="2"),
            size=2, col="Maroon") +
  xlab("") + ylab("") +
  opts(legend.position="none",
       axis.title.y = theme_text(size = 12, angle=90), 
       axis.title.x = theme_text(size = 12, vjust=0),
       axis.text.x = theme_text(size = 16),
       axis.text.y= theme_text(size = 16))

################################################################################
################################################################################
################################################################################

## @knitr GR_speech
data(aa) 
set.seed(123)
ind <- sample(1:150, 1)
speech.y <- aa$y[ , ind]
speech.df <- as.data.frame(speech.y)
speech.df$x <- aa$x
speech.df <- melt(speech.df, id="x")

theme_set(theme_bw())
ggplot(speech.df) + 
  geom_line(aes(x=x, y=value, group=variable), size=2, col="RoyalBlue") +
  xlab("") + ylab("") +
  opts(legend.position="none",
       axis.title.y = theme_text(size = 12, angle=90), 
       axis.title.x = theme_text(size = 12, vjust=0),
       axis.text.x = theme_text(size = 16),
       axis.text.y= theme_text(size = 16))

################################################################################
################################################################################
################################################################################

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

## @knitr GR_kodiak
map <- GetMap(center = c(57.486308980380834,-153.396606445312),
              zoom=4, 
              markers = "&markers=color:blue|label:K|57.486308980380834,-153.396606445312",
              destfile = "kodiak_insel.png", maptype="terrain");


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


################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
################################################################################


###
#
# R-Code zum Vortrag functional Response with functional Predictors
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

#  The Swedish mortality data are proprietary, and therefore we do not
#  provide them with the fda package.  In order to use the following
#  code for their analysis, you must first obtain the data.  The following
#  details are provided to help you to obtain them.

#  Mortality data for 37 countries can be obtained from the 
#  Human Mortality Database (http://www.mortality.org/). 
#  For example, the Swedish mortality data can be found at 
#  (http://www.mortality.org/cgi-bin/hmd/country.php?cntr=SWE&level=1).

#  Citation:

#  Human Mortality Database. University of California, Berkeley (USA), 
#  and Max Planck Institute for Demographic Research (Germany). 

#   Two data objects are required for these analyses:

#  SwedeMat:  a dataframe object with 81 rows and 144 columns
#             containing the log hazard values for ages 0 through 80
#             and years 1751 through 1884
#  Swede1920: a vector object containing log hazard values for 1914

#  Han Lin Shang at Monash University in Australia has kindly provided
#  the following R function to assist you.  It requires a username and
#  password for accessing the data, which you must obtain from the above
#  web site.

## @knitr CA_mort
# Lade Daten
load("Sweden.Rdata")
SwedeLogHazard <- as.matrix(SwedeMat)
dimnames(SwedeLogHazard)[[2]] <- paste(1751:1894, sep='')

fig.dat <- cbind(SwedeLogHazard[, c('1751', '1810', '1860')], Swede1920)

SwedeTime <- 0:80;

rangval <- c(0,80)
nbasis <- 23
SwedeRng <- c(0,80)
norder <- 4
SwedeBasis <- create.bspline.basis(SwedeRng, nbasis, norder)
bbasis <- create.bspline.basis(rangval, norder=4, nbasis=23)
D2fdPar <- fdPar(SwedeBasis, Lfdobj=2, lambda=0.01)

SwedeLog20fd <- smooth.basis(SwedeTime, Swede1920, D2fdPar)$fd
SwedeLogHazfd <- smooth.basis(SwedeTime, SwedeLogHazard, D2fdPar)$fd

# Set up for the list of regression coefficient fdPar objects
nbasis <- 23
SwedeRng <- c(0,80)
SwedeBetaBasis <- create.bspline.basis(SwedeRng, nbasis)
SwedeBeta0Par <- fdPar(SwedeBetaBasis, 2, 1e-5)
SwedeBeta1fd <- bifd(matrix(6,23,23), SwedeBetaBasis, SwedeBetaBasis)
SwedeBeta1Par <- bifdPar(SwedeBeta1fd, 2, 2, 1e-3, 1e-3)
SwedeBetaList <- list(SwedeBeta0Par, SwedeBeta1Par)

#  Define the dependent and independent variable objects
NextYear <- SwedeLogHazfd[2:144]
LastYear <- SwedeLogHazfd[1:143]

Swede.linmod <- linmod(NextYear, LastYear, SwedeBetaList)
#plot(Swede.linmod$beta0estfd)

#persp(Swede.ages, Swede.ages, Swede.beta1mat, 
#      xlab="age", ylab="age",zlab="beta(s,t)",
#      theta=30, phi=30)


betafdPar1$lambda <-  10^0.5
for (j in 1:2) betalist[[j]] <-  betafdPar1

#  carry out the functional regression analysis
fitShellfish.5 <- fRegress(birdfd3, xfdlist, betalist)
birdYhatmat <- eval.fd(yearCode, fitShellfish.5$yhatfdobj$fd[1:26])
rmatb <- logCounts2 - birdYhatmat
SigmaEb <- var(t(rmatb))

y2cMap.bird <- birdlist2$y2cMap

birdStderrList <- fRegress.stderr(fitShellfish.5, y2cMap.bird, SigmaEb)
birdBeta.sdList <- birdStderrList$betastderrlist
>>>>>>> 83c75652f9b3d7672759fe61749e65be9b384124

################################################################################
################################################################################
################################################################################

## @knitr GR_bspmort
fig.df <- as.data.frame(fig.dat)
fig.df$year <- SwedeTime
fig.df <- melt(fig.df, id="year")
levels(fig.df$variable) <- c(levels(fig.df$variable)[1:3], "1920")
colours <- rainbow_hcl(4, start = 30, end = 300)
theme_set(theme_bw())
ggplot(fig.df) + 
  geom_line(aes(x=year, y=value, group=variable, col=variable), size=2) +
  scale_color_manual("", values=colours) +
  xlab("") + ylab("") +
  opts(legend.text = theme_text(size=16),
       axis.title.y = theme_text(size = 12, angle=90), 
       axis.title.x = theme_text(size = 12, vjust=0),
       axis.text.x = theme_text(size = 16),
       axis.text.y= theme_text(size = 16))

################################################################################
################################################################################
################################################################################

## @knitr GR_res
Swede.ages <- seq(0, 80, 1)
Swede.beta1mat <- eval.bifd(Swede.ages, Swede.ages, Swede.linmod$beta1estbifd)
dff <- data.frame(x1=Swede.ages,
                  val=Swede.beta1mat)
dff <- melt(dff, id="x1")
dff$x2 <- rep(0:80, each=81)
theme_set(theme_bw())
ggplot(dff) + stat_contour(aes(x1, x2, z=value, colour = ..level..), bins=50) +
  xlab("s") + ylab("t") +
  opts(legend.position="none",
       axis.title.y = theme_text(size = 16, angle=90), 
       axis.title.x = theme_text(size = 16, vjust=0),
       axis.text.x = theme_text(size = 16),
       axis.text.y= theme_text(size = 16))


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
  geom_line(aes(x=year, y=rep1), lty=2, size=2) +
  geom_line(aes(x=year, y=rep1.1), lty=2, size=2) +
  opts(legend.position="none", 
       title="$\\mu(t)$",
       axis.title.y = theme_text(size = 12, angle=90), 
       axis.title.x = theme_text(size = 12, vjust=0),
       axis.text.x = theme_text(size = 12),
       axis.text.y= theme_text(size = 12),
       plot.title=theme_text(size=18)) +
  xlab("") + ylab("Reg. Koef.")

################################################################################
################################################################################
################################################################################

## @knitr GR_res_org
Swede.ages <- seq(0, 80, 1)
dff <- data.frame(x1=Swede.ages,
                  val=SwedeLogHazard)
dff <- melt(dff, id="x1")
dff$x2 <- rep(1757:1900, each=81)
theme_set(theme_bw())
ggplot(dff) + 
  stat_contour(aes(x1, x2, z=value, colour = ..level..), bins=17) +
  xlab("") + ylab("") +
  opts(legend.position="none",
       axis.title.y = theme_text(size = 12, angle=90), 
       axis.title.x = theme_text(size = 12, vjust=0),
       axis.text.x = theme_text(size = 16),
       axis.text.y= theme_text(size = 16))


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
       title="$\\alpha(t)$",
       axis.title.y = theme_text(size = 12, angle=90), 
       axis.title.x = theme_text(size = 12, vjust=0),
       axis.text.x = theme_text(size = 12),
       axis.text.y= theme_text(size = 12),
       plot.title=theme_text(size=18)) +
  xlab("") + ylab("Reg. Koef.")

