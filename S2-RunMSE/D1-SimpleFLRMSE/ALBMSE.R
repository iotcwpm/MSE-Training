# ALBMSE.R - DESC
# ALBMSE.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(FLBRP)
library(FLAssess)
library(biodyn)
library(aspic)

# tail {{{
setMethod("tail", signature(x="FLQuant"),
	function(x, n=1, dim=2, ...) {

		# dim of length 1
		if(length(dim) > 1)
			stop("tail(FLQuant) can allow apply to a single dim(ension)")

		# character dim
		if(is(dim, 'character'))
			dim <- which(dim == names(x))


		# named list of dimension vectors
		idx <- lapply(as.list(dim(x)), seq)
		names(idx) <- c('i','j','k','l','m','n')

		# tail dimension set by dim
		idx[[dim]] <- tail(idx[[dim]], n=n)
		
		# apply '['
		return(do.call('[', c(list(x=x), idx)))
	}
) # }}}

NITER <- 100
YEARS <- seq(2011, 2020)

# LOAD OM

load('data/om.RData')

# ONE iter TEST
om <- om[,,,4,,sample(648, 25)]

# BUG:
catch.wt(om)[1,] <- 0.1
m(om) <- 0.25

# SR
sr <- as.FLSR(om, model="bevholtSV")
sr <- fmle(sr, fixed=list(s=0.8, spr0=spr0(om)), method='Brent', lower=c(1e-8), upper=c(1e20))

#
om <- stf(om, length(YEARS))

# SETUP simulation grid

grid <- list(
	Btrigger=c(0.90, 0.85, 0.95),
	timeRec=c(5, 8, 10),
	errorC=c(0.10, 0.20, 0.30),
	beta=c(0.01, 0.02, 0.05)
	)

# RUN, RUN!
for (i in YEARS) {

	# Get catch data
	tc <- catch(om)[,ac(i-1)] # + OBS. ERROR

	# Get CPUE
	# Perfect knowledge!
	cpue <- window(stock(om), end=i-1)/1000
	# CPUE = C_LL / E_LL

	# HCR 2: CPUE-based indicator HCR
	# slope <- coef(lm(data~year, as.data.frame(tail(cpue, n=5))))['year']
	Xcpue <- tail(cpue, 5)
	Xcpue <- (Xcpue %/% Xcpue[,1])-1
	inp <- as.data.frame(Xcpue[,,,,,])
	slope <- ddply(inp, .(iter), summarise, slope=coef(lm(data~year))[2])
	
	#
	beta <- 0.5
	nc <- tc * (1 + beta * (slope$slope * 5))

	# print(paste(i, slope, mean(c(tc)), mean(c(nc)), sep=" : "))

	# Project
	slopes <- array(NA, dim=c(1,3,25), dimnames=list(1, c("min", "val", "max"),
		iter=1:25))
	slopes[1,'val',] <- nc

	ctrl <- fwdControl(data.frame(year=i, quantity="catch", val=c(nc)[1]))
	ctrl@trgtArray <- slopes
	om <- fwd(om, ctrl=ctrl, sr=ab(sr))

} # }}}

# Apply HCR

- Biomass dynamics SA
- IF SSB_y < 0.90 * SSB@MSY
	- F_y+1 = F(SSB y+5 > SSB@MSY)
- F_y+1 -> C_y+1
- E(C_y+1)


# OUTPUT

- P(SSB_y >= SSB@MSY)
- P(SSB_end >= SSB@MSY)
- P(F_y >= F@MSY)
- P(F_end >= F@MSY)
- Median C, CV C

# HCR 1 {{{

# Run SA
bd <- biodyn(model="pellat", catch=catch(om))
catch(om)[,1] <- 0.1

# BUG: quant and stock dims {{{
quant(catch(bd)) <- 'quant'
bd@stock <- window(catch(bd), end=2011) # }}}

# control
setParams(bd) <- cpue
setControl(bd) <- cpue
bd@control['r', c('min', 'val', 'max')] <- c(0.1, 0.8, 0.9)
bd@control['p', c('min', 'val', 'max')] <- c(2,2,2)
bd@control[c("p","b0", "r"),"phase"] <- -1
bd@control['k', c('min', 'val', 'max')] <- c(min(catch(bd)), max(catch(bd))*1000, max(catch(bd))*1e6)

# biodyn
bd <- fit(bd, index=cpue)

# aspic
bd <- fit(aspic(om))

# }}}

