# ALBMSE.R - DESC
# ALBMSE.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(FLa4a)
library(FLBRP)
library(biodyn)
library(aspic)

NITER <- 100

# LOAD OM

load('data/alb.RData')
om <- iter(om, sample(1:648, 100))
om <- om[,,,4,,1]

# TEST OM: ple4 w/ SA estimation error only {{{

data(ple4)
data(ple4.indices)

## SA OM conditioning
sa <- a4a(stock = ple4, indices = ple4.indices, fit = "assessment")
ple4 <- ple4 + sa

## REF PTS
rfp <- refpts(brp(FLBRP(ple4)))
msy <- rfp['msy', c('harvest', 'yield', 'ssb')]

## OM
om <- propagate(ple4, NITER) + sa # }}}

# SR
sr <- as.FLSR(om, model="bevholtSV")
sr <- fmle(sr, fixed=list(s=0.8, spr0=spr0(om)))

# SETUP simulation grid

grid <- list(
	Btrigger=c(0.90, 0.85, 0.95),
	timeRec=c(5, 8, 10),
	errorC=c(0.10, 0.20, 0.30))

# RUN, RUN!

# Get total catch

# Get CPUE

# Run SA
bd <- biodyn(model="pellat", catch=catch(om))
catch(om)[,1] <- 0.1

# BUG: quant and stock dims {{{
quant(catch(bd)) <- 'quant'
bd@stock <- window(catch(bd), end=2011) # }}}

# control
setControl(bd) <- tsb(om)
bd@control['r', c('min', 'val', 'max')] <- c(0.1, 0.8, 0.9)
bd@control['p', c('min', 'val', 'max')] <- c(2,2,2)
bd@control[c("p","b0", "r"),"phase"] <- -1
bd@control['k', c('min', 'val', 'max')] <- c(min(catch(bd)), max(catch(bd))*1000, max(catch(bd))*1e6)
																						 
bd <- fit(bd, index=stock(om)/1000)
bd <- fit(aspic(om))

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

