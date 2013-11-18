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
