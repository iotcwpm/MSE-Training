# extract.R - DESC
# extract.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:


# PKGS
library(FLBRP)

# FAO sim output file
load('/home/imosqueira/Work/Projects/SimFAO/stockStatusFAO/simulated/out/0.6/sims06201306072204.RData')

# RC
rc <- sims$LP_ID0_AR_RC_SELF_UR0_TS60
om <- rc$stock

# rps
rps <- refpts(rc$brp)

# sr
sr <- as.FLSR(om, model=bevholtSV)
params(sr) <- FLPar(s=0.80, v=1000, spr0=spr0(om))
pre <- predict(sr)
quant(pre) <- 'age'
fitted(sr) <- pre
residuals(sr) <- log(rec(sr) / fitted(sr))

save(om, sr, rps, file='rc.RData')

# OW
ow <- sims$LP_ID0_AR_OW_SELF_UR0_TS60
om <- ow$stock

# rps
rps <- refpts(ow$brp)

# sr
sr <- as.FLSR(om, model=bevholtSV)
params(sr) <- FLPar(s=0.80, v=1000, spr0=spr0(om))
pre <- predict(sr)
quant(pre) <- 'age'
fitted(sr) <- pre
residuals(sr) <- log(rec(sr) / fitted(sr))

save(om, sr, rps, file='ow.RData')

# ED
ed <- sims$LP_ID0_AR_ED0_SELF_UR0_TS60
# ed <- sims$LP_ID0_AR_ED0.6_SELF_UR0_TS60
om <- ed$stock

# rps
rps <- refpts(ed$brp)

# sr
sr <- as.FLSR(om, model=bevholtSV)
params(sr) <- FLPar(s=0.80, v=1000, spr0=spr0(om))
pre <- predict(sr)
quant(pre) <- 'age'
fitted(sr) <- pre
residuals(sr) <- log(rec(sr) / fitted(sr))

save(om, sr, rps, file='ed.RData')
