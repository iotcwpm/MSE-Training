# plotsMSE.R - DESC
# plotsMSE.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# GRID
grid <- list(
	OM=c('rc', 'ow', 'ed'),
	SResid=c(10, 20, 40),
	beta=c(0.1, 0.2, 0.4),
	cpueNoise=c(0, 10, 20))

gridf <- expand.grid(grid, KEEP.OUT.ATTRS=FALSE)

pgrid <- vector('list', length=nrow(gridf))

for(i in seq(nrow(gridf)))
		pgrid[[i]] <- c(as.list(gridf[i,]), list(pONE=NULL, pTWO=NULL, pTHREE=NULL, pFOUR=NULL))



# input TEST

input <- list(om='rc', sresid='10', beta='0.10', cpuenoise='0')

# sub

# plotONE - plot(FLStock) w/refpts & projection years

plotONE <- function(sub, rp) {
}

	sta <- ddply(sub, .(qname,year), summarise, median = median(data),
		q1u = quantile(data, 0.85, na.rm=TRUE),
		q1l = quantile(data, 0.15, na.rm=TRUE),
		q2u = quantile(data, 0.75, na.rm=TRUE),
		q2l = quantile(data, 0.25, na.rm=TRUE)
		)
	sta <- sta[sta$qname != 'deltaCATCH',]
	rpa <- rp[rp$OM=='rc',]

	ggplot(sta, aes(year, median)) + geom_line() + facet_grid(qname~., scales='free') +
		xlab("") + ylab("") + expand_limits(y=0) +
		geom_ribbon(aes(year, ymin=q2l, ymax=q2u), fill="red", alpha = .25) +
		geom_ribbon(aes(x=year, ymin=q1l, ymax =q1u),  fill="red", alpha = .10) +
		# MSY
		geom_hline(data=rpa, aes(yintercept=msy), colour="blue", linetype=2)
	
# plotTWO - 

# plotTHREE - 

# plotFOUR -

# P(SSB < SSB_lim) @ 5, 10, 20 years
# P(SSB > SSB_tar) @ 5, 10, 20 years
# P(F < F_lim) @ 5, 10, 20 years
# P(F > F_tar) @ 5, 10, 20 years

# Percentage change in catch by year
