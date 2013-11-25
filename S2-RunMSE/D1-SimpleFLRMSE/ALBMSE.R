# ALBMSE.R - DESC
# ALBMSE.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(FLash)
library(plyr)
library(FLAssess)
library(ggplotFL)

# LOAD OM
load('data/om.RData')

# PARAMETERS
NITER <- dims(om)$iter
YEARS <- seq(2011, 2025)
BETA <- 0.05
SLOPEYRS <- 8

# BUG: setSR messes up dimnames$iter
om <- qapply(om, function(x) {dimnames(x)$iter <- seq(NITER); return(x)})
sr <- qapply(sr, function(x) {dimnames(x)$iter <- seq(NITER); return(x)})
dimnames(params(sr))$iter <- seq(NITER)

# EXTRACT SR residuals
sresid <- residuals(sr)[, ac(sample(1970:2010, length(YEARS), replace=TRUE))] + 
	# + B0 variability
	log(c(params(sr)['v'])/mean(c(params(sr)['v'])))
dimnames(sresid) <- list(year=YEARS)

# EXTEND om for future years
om <- stf(om, length(YEARS))

# RUN, RUN! {{{
for (i in YEARS) {

	# Get catch data, no error
	tc <- catch(om)[,ac(i-1)]

	# Get CPUE, perfect knowledge!
	cpue <- window(ssb(om), end=i-1)/1000

	# HCR 1: cpue-based indicator
	xcpue <- tail(cpue, SLOPEYRS)
	xcpue <- (xcpue %/% xcpue[,1])-1
	inp <- as.data.frame(xcpue[,,,,,])
	slope <- ddply(inp, .(iter), summarise, slope=coef(lm(data~year))[2])
	
	# SET catch
	idx <- slope$slope > 0
	nc <- tc * (1 + BETA * slope$slope)
	# nc[,,,,,idx] <- tc[,,,,,idx] * 0.75
	
	cat("[", i, "] NC = ", mean(nc), "\t", "SSB = ", mean(ssb(om)[, ac(i)]), "\n", sep="")

	# Project
	slopes <- array(NA, dim=c(1,3,NITER), dimnames=list(1, c("min", "val", "max"),
		iter=1:NITER))
	slopes[1,'val',] <- nc

	ctrl <- fwdControl(data.frame(year=i, quantity="catch", val=c(nc)[1]))
	ctrl@trgtArray <- slopes
	# BUG
	dimnames(ctrl@trgtArray)$iter <- dimnames(sresid)$iter
	
	# SR variability
	om <- fwd(om, ctrl=ctrl, sr=ab(sr), sr.residuals=exp(sresid), sr.residuals.mult=TRUE)
	# No SR var
	# om <- fwd(om, ctrl=ctrl, sr=ab(sr))

} # }}}

# OUTPUT

