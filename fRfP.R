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

nbasis = 85
SwedeRng   = c(0,80)
norder = 6
SwedeBasis = create.bspline.basis(SwedeRng, nbasis, norder)

D2fdPar = fdPar(SwedeBasis, lambda=1e-7)

SwedeLog20fd <- smooth.basis(SwedeTime, Swede1920, D2fdPar)$fd
SwedeLogHazfd = smooth.basis(SwedeTime, SwedeLogHazard, D2fdPar)$fd

# Set up for the list of regression coefficient fdPar objects
nbasis     = 23
SwedeRng   = c(0,80)
SwedeBetaBasis = create.bspline.basis(SwedeRng,nbasis)
SwedeBeta0Par = fdPar(SwedeBetaBasis, 2, 1e-5)
SwedeBeta1fd  = bifd(matrix(0,23,23), SwedeBetaBasis, SwedeBetaBasis)
SwedeBeta1Par = bifdPar(SwedeBeta1fd, 2, 2, 1e3, 1e3)
SwedeBetaList = list(SwedeBeta0Par, SwedeBeta1Par)

#  Define the dependent and independent variable objects
NextYear = SwedeLogHazfd[2:144]
LastYear = SwedeLogHazfd[1:143]

Swede.linmod = linmod(NextYear, LastYear, SwedeBetaList)
plot(Swede.linmod$beta0estfd)
Swede.ages = seq(0, 80, 1)
Swede.beta1mat = eval.bifd(Swede.ages, Swede.ages, Swede.linmod$beta1estbifd)

persp(Swede.ages, Swede.ages, Swede.beta1mat, 
      xlab="age", ylab="age",zlab="beta(s,t)",
      theta=30, phi=30)

ggplot(xxx, aes(x=X1, y=X2)) + geom_point(aes(col = value, size=value))  + scale_color_continuous(breaks=1:3, low="lightgreen", high="darkgreen")

################################################################################
################################################################################
################################################################################

## @knitr GR_bsp
# Grafiken
fig.df <- as.data.frame(fig.dat)
fig.df$year <- SwedeTime
fig.df <- melt(fig.df, id="year")
levels(fig.df$variable) <- c(levels(fig.df$variable)[1:3], "1914")
require(colorspace)
colours <- rainbow_hcl(4, start = 30, end = 300)
ggplot(fig.df) + 
  geom_line(aes(x=year, y=value, group=variable, col=variable), size=2) +
  scale_color_manual("", values=colours) +
  xlab("") + ylab("")



