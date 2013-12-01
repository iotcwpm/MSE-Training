# demoMSE.R - DESC
# demoMSE.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# demoMSE {{{
demoMSE <- function(om, sr, years=seq(dims(om)$maxyear+1, length=20), beta=0.2, 
	cpueNoise=0, SResid=40, slopeYears=5) {
	
	niter <- dims(om)$iter

	# EXTEND om for future years
	om <- stf(om, length(years))

	# EXTRACT SR residuals for last SResid years
	sryears <- dims(sr)$minyear:dims(sr)$maxyear
	sryears <- sryears[-seq(2:(60-SResid))]
	sresid <- residuals(sr)[, ac(sample(sryears, length(years), replace=TRUE))]
	dimnames(sresid) <- list(year=years)

	# RUN, RUN!
	for (i in years) {

		# Get catch data, no error
		tc <- catch(om)[,ac(i-1)]

		# Get CPUE, perfect knowledge!
		cpue <- window(ssb(om), end=i-1)/1000
		cpue <- apply(cpue, 1:6, function(x) rnorm(1, x, cpueNoise))

		# HCR 1: cpue-based indicator
		xcpue <- tail(cpue, slopeYears)
		xcpue <- (xcpue %/% xcpue[,1])-1
		inp <- as.data.frame(xcpue[,,,,,])
		slope <- ddply(inp, .(iter), summarise, slope=coef(lm(data~year))[2])
	
		# SET catch
		nc <- tc * (1 + beta * slope$slope)
	
		# PROGRESS
		cat("[", i, "] NC = ", median(c(nc)), "  ",
				"TC = ", median(c(tc)), "  ",
				"SSB = ", median(c(ssb(om)[, ac(i-1)])), "\n", sep="")

		# PROJECT stock
		slopes <- array(NA, dim=c(1,3,niter), dimnames=list(1, c("min", "val", "max"),
			iter=1:niter))
		slopes[1,'val',] <- nc

		ctrl <- fwdControl(data.frame(year=i, quantity="catch", val=c(nc)[1]))
		ctrl@trgtArray <- slopes
		# BUG
		dimnames(ctrl@trgtArray)$iter <- dimnames(sresid)$iter
	
		# SR variability
		om <- fwd(om, ctrl=ctrl, sr=ab(sr), sr.residuals=exp(sresid), sr.residuals.mult=TRUE)
	} 

	# RESULTS
	chc <- catch(om)[,years[-1]] / catch(om)[,years[-length(years)]]
	
	res <- as.data.frame(FLQuants(
		# FLStock: SSB, CATCH, REC, F
		SSB=ssb(om), CATCH=catch(om), REC=rec(om), F=fbar(om),
		# deltaCATCH
		deltaCATCH=catch(om)[,years[-1]] / catch(om)[,years[-length(years)]]
		), drop=TRUE)

	res <- cbind(res, beta=beta, cpueNoise=cpueNoise, SResid=SResid)

	return(res)
		
} # }}}
