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

res <- rbind(res, cbind(ow, OM='ow'))

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

res <- rbind(res, cbind(ed, OM='ed'))

rp <- rbind(rp, data.frame(OM='ed', qname=c('SSB', 'CATCH', 'REC', 'F'),
		msy=c(rps['msy',c('ssb', 'yield', 'rec', 'harvest')])))

# SAVE res
save(res, rp, file='res.RData')

# RSQLite
library(RSQLite)
con <- dbConnect(SQLite(), "res.SQLite")
dbWriteTable(con, "res", res)


# -----------

# PLOT grid
pgrid <- list(
	OM=as.character(c('rc', 'ow', 'ed')),
	SResid=c(10, 20, 40),
	beta=c(0.1, 0.2, 0.4),
	cpueNoise=c(0, 10, 20))

pgridf <- expand.grid(pgrid, KEEP.OUT.ATTRS=FALSE)
pgridf$number <- seq(1, nrow(pgridf))

# PLOT res
pres <- vector('list', length=nrow(pgridf))

for(i in seq(nrow(pgridf))) {
	pres[[i]] <- as.list(pgridf[i,])
	pres[[i]][['sub']] <- subset(res, 
		OM==pgridf[i,'OM'] & 
		SResid==pgridf[i,'SResid'] &
		beta==pgridf[i,'beta'] &
		cpueNoise==pgridf[i,'cpueNoise'])
}

# plotONE - FLStock w/MSY
for(i in seq(nrow(pgridf))) {

	sta <- subset(pres[[i]]$sub, qname!='deltaCATCH')
	sta <- ddply(sta, .(qname,year), summarise, median = median(data),
		q1u = quantile(data, 0.85, na.rm=TRUE),
		q1l = quantile(data, 0.15, na.rm=TRUE),
		q2u = quantile(data, 0.75, na.rm=TRUE),
		q2l = quantile(data, 0.25, na.rm=TRUE),
		it1 = subset(data, iter==9),
		it2 = subset(data, iter==25),
		it3 = subset(data, iter==187)
	)
	rpa <- rp[rp$OM==pres[[i]]$OM,]

	pres[[i]]$plotONE <- 
		ggplot(sta, aes(year, median)) + geom_line() +
		facet_grid(qname~., scales='free') + xlab("") + ylab("") + expand_limits(y=0) +
		# 85 and 75% quantiles
		geom_ribbon(aes(year, ymin=q2l, ymax=q2u), fill="red", alpha = .25) +
		geom_ribbon(aes(x=year, ymin=q1l, ymax =q1u),  fill="red", alpha = .10) +
		# MSY
		geom_hline(data=rpa, aes(yintercept=msy), colour="blue", linetype=2) +
		# Plots of individual iterations
		geom_line(aes(year, it1), colour='orange1', linetype=5, size=0.4) +
		geom_line(aes(year, it2), colour='orange2', linetype=5, size=0.4) +
		geom_line(aes(year, it3), colour='orange', linetype=5, size=0.4) + 	
		# Start year of simulation
		geom_vline(xintercept=60, colour='red')
}

# plotTWO
# P(SSB < SSB_lim)
		
for(i in seq(nrow(pgridf))) {

	prob <- expand.grid(qname=c('SSB', 'F'), year=c(65, 70, 80),
		rp=c('MSY', 'LIM'), prob=NA)
	sta <- subset(pres[[i]]$sub, qname!='deltaCATCH')

	for(y in c(65,70,80)) {
		# SSB_MSY
		idx <- subset(sta, year==y)$data > subset(rpa, qname=='SSB')$msy
		prob[prob$qname=='SSB' & prob$year==y & prob$rp == 'MSY','prob'] <-
			sum(idx)/length(idx)
		# SSB_LIM
		idx <- subset(sta, year==y)$data < (subset(rpa, qname=='SSB')$msy * 0.40)
		prob[prob$qname=='SSB' & prob$year==y & prob$rp == 'LIM','prob'] <-
			sum(idx)/length(idx)
		# F_MSY
		idx <- subset(sta, year==y)$data < subset(rpa, qname=='F')$msy
		prob[prob$qname=='F' & prob$year==y & prob$rp == 'MSY','prob'] <-
			sum(idx)/length(idx)
		# F_LIM
		idx <- subset(sta, year==y)$data > (subset(rpa, qname=='F')$msy * 0.40)
		prob[prob$qname=='F' & prob$year==y & prob$rp == 'LIM','prob'] <-
			sum(idx)/length(idx)
	}
	pres[[i]]$plotTWO <- ggplot(prob, aes(year, prob)) + geom_point() +
		facet_grid(qname~rp) + 	xlab("") + ylab("") + expand_limits(y=1)
}
	
# plotTHREE

# plotFOUR - Percentage change in catch by year
for(i in seq(nrow(pgridf))) {
	
	sta <- subset(pres[[i]]$sub, qname=='deltaCATCH')
	sta <- ddply(sta, .(qname,year), summarise, median = median(data),
		q1u = quantile(data, 0.85, na.rm=TRUE),
		q1l = quantile(data, 0.15, na.rm=TRUE),
		q2u = quantile(data, 0.75, na.rm=TRUE),
		q2l = quantile(data, 0.25, na.rm=TRUE)
	)
	cva <-(mean(sta$median)-1)*100 
	
	pres[[i]]$plotFOUR <- ggplot(sta, aes(year, median)) + geom_line() +
		xlab("") + ylab("") + 
		geom_ribbon(aes(year, ymin=q2l, ymax=q2u), fill="red", alpha = .25) +
		geom_ribbon(aes(x=year, ymin=q1l, ymax =q1u),  fill="red", alpha = .10) +
		geom_hline(aes(yintercept=1), colour="blue", linetype=2) +
		annotate("text", x = 78, y = 1, label = paste0(format(cva, digits=2), "%"),
			size=15, alpha=0.60)
}

save(pgridf, pres, file='pres.RData')

# PLOTS list
plots <- lapply(pres, function(x) x[6:8])
save(plots, pgridf, file='shiny/plots.RData')

# PLOTS across runs
pgrid <- list(
	OM=as.character('rc'),
	SResid=c(10),
	beta=c(0.1, 0.2, 0.4),
	cpueNoise=c(0, 10, 20))


for(i in seq(nrow(pgridf))) {
	sta <- subset(pres[[i]]$sub, qname=='SSB')
}
