# TEST.R - DESC
# TEST.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(FLa4a)

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
