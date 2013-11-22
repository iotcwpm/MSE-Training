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

NITER <- 200
YEARS <- seq(2011, 2025)

# LOAD OM
load('data/om.RData')

# SELECT random NITERs
idx <- sample(dims(om)$iter, NITER)
om <- om[,,,,,idx]
sr <- sr[,,,,,idx]
params(sr) <- params(sr)[,idx]

# BUG: setSR messes up dimnames$iter
om <- qapply(om, function(x) {dimnames(x)$iter <- seq(NITER); return(x)})
sr <- qapply(sr, function(x) {dimnames(x)$iter <- seq(NITER); return(x)})
dimnames(params(sr))$iter <- seq(NITER)

# SR residuals
sresid <- residuals(sr)[, ac(sample(1970:2010, length(YEARS), replace=TRUE))]
dimnames(sresid) <- list(year=YEARS)

# EXTEND om
om <- stf(om, length(YEARS))

# SETUP simulation grid
grid <- list(
	Btrigger=c(0.90, 0.85, 0.95),
	timeRec=c(5, 8, 10),
	errorC=c(0.10, 0.20, 0.30),
	beta=c(0.01, 0.02, 0.05))

# RUN, RUN! {{{
for (i in YEARS) {

	# Get catch data, no error
	tc <- catch(om)[,ac(i-1)]

	# Get CPUE, perfect knowledge!
	cpue <- window(stock(om), end=i-1)/1000

	# HCR 2: cpue-based indicator
	xcpue <- tail(cpue, 5)
	xcpue <- (xcpue %/% xcpue[,1])-1
	inp <- as.data.frame(xcpue[,,,,,])
	slope <- ddply(inp, .(iter), summarise, slope=coef(lm(data~year))[2])
	beta <- 0.01
	nc <- tc * (1 + beta * slope$slope)
	nc <- 15000
	cat("nc[", i, "] = ", mean(nc), "\n")

	# Project
	slopes <- array(NA, dim=c(1,3,NITER), dimnames=list(1, c("min", "val", "max"),
		iter=1:NITER))
	slopes[1,'val',] <- nc

	ctrl <- fwdControl(data.frame(year=i, quantity="catch", val=c(nc)[1]))
	ctrl@trgtArray <- slopes
	# BUG
	dimnames(ctrl@trgtArray)$iter <- dimnames(sresid)$iter
	
	# MEAN rec: om <- fwd(om, ctrl=ctrl, sr=list(model='mean', params=FLPar(a=18000)))
	om <- fwd(om, ctrl=ctrl, sr=ab(sr), sr.residuals=exp(sresid), sr.residuals.mult=TRUE)

} # }}}

# OUTPUT

tsi <- as.data.frame(FLQuants(
	ssb=ssb(om),
	rec=rec(om),
	catch=catch(om)))

# tsq <- 

# per <- 

ggplot(data=ts, aes(year, data)) + geom_line() + facet_wrap(~qname, scales="free", nrow=3)

- P(SSB_y >= SSB@MSY)
- P(SSB_end >= SSB@MSY)
- P(F_y >= F@MSY)
- P(F_end >= F@MSY)
- Median C, CV C

