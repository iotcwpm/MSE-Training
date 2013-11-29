# SIMMSE.R - DESC
# SIMMSE.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(FLBRP)
library(plyr)
library(FLAssess)

source('demoMSE.R')

# SIM grid

grid <- list(
	SResid=c(10, 20, 40),
	beta=c(0.1, 0.2, 0.4),
	cpueNoise=c(0, 10, 20))

gridf <- expand.grid(grid, KEEP.OUT.ATTRS=FALSE)

# LOAD OMs

# RC
load('data/rc.RData')

# 1
rc <- do.call(demoMSE, c(list(om=om, sr=sr, years=61:80), as.list(gridf[1,])))

for(i in seq(nrow(gridf))[-1]) {
	cat("--", i, "\n")
	rc <- rbind(rc, do.call(demoMSE, c(list(om=om, sr=sr, years=61:80), as.list(gridf[i,]))))
}

res <- cbind(rc, OM='rc')

rp <- data.frame(OM='rc', qname=c('SSB', 'CATCH', 'REC', 'F'),
		msy=c(rps['msy',c('ssb', 'yield', 'rec', 'harvest')]))

# OW
load('data/ow.RData')

# 1
ow <- do.call(demoMSE, c(list(om=om, sr=sr, years=61:80), as.list(gridf[1,])))

for(i in seq(nrow(gridf))[-1]) {
	cat("--", i, "\n")
	ow <- rbind(ow, do.call(demoMSE, c(list(om=om, sr=sr, years=61:80), as.list(gridf[i,]))))
}

res <- rbind(rc, cbind(ow, OM='rc'))

rp <- rbind(rp, data.frame(OM='ow', qname=c('SSB', 'CATCH', 'REC', 'F'),
		msy=c(rps['msy',c('ssb', 'yield', 'rec', 'harvest')])))

# ED
load('data/ed.RData')

# 1
ed <- do.call(demoMSE, c(list(om=om, sr=sr, years=61:80), as.list(gridf[1,])))

for(i in seq(nrow(gridf))[-1]) {
	cat("--", i, "\n")
	ed <- rbind(ed, do.call(demoMSE, c(list(om=om, sr=sr, years=61:80), as.list(gridf[i,]))))
}

res <- rbind(ed, cbind(ed, OM='ed'))

rp <- rbind(rp, data.frame(OM='ed', qname=c('SSB', 'CATCH', 'REC', 'F'),
		msy=c(rps['msy',c('ssb', 'yield', 'rec', 'harvest')])))

# SAVE res
save(res, file='res.RData')
save(rp, file='rp.RData')

# RSQLite
con <- dbConnect(SQLite(), "res.SQLite")
dbWriteTable(con, "res", res)

# PLOTS by run

# PLOTS across runs
