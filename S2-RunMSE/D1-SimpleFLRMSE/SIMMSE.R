# SIMMSE.R - DESC
# SIMMSE.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(FLash)
library(plyr)
library(FLAssess)
library(ggplotFL)

# LOAD OM
load('data/rc.RData')

# PARAMETERS
NITER <- dims(om)$iter
YEARS <- seq(61, 80)
BETA <- 0.2
SLOPEYRS <- 5

# EXTRACT SR residuals
sresid <- residuals(sr[, sample(1:60, length(YEARS))])
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
	nc <- tc * (1 + BETA * slope$slope)
	
	# PROGRESS
	cat("[", i, "] NC = ", median(c(nc)), "  ",
			"TC = ", median(c(tc)), "  ",
			"SSB = ", median(c(ssb(om)[, ac(i-1)])), "\n", sep="")

	# PROJECT stock
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
	# MEAN SR
	# om <- fwd(om, ctrl=ctrl, sr=list(model='mean', params=FLPar(a=1500)))

} # }}}

# OUTPUT

plot(om)

# TS by year & iter
res <- as.data.frame(FLQuants(SSB=ssb(om), CATCH=catch(om)), drop=TRUE)

# PROBS by year

# FINAL risks
