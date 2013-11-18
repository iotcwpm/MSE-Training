library(devtools)

load_all("FLRcppAdolc")
document("FLRcppAdolc")
#library(FLCore)
#library(FLRcppAdolc)

#***************************************************************************

nzrl <- read.csv("NZRL.csv")
saa <-  read.csv("SAA.csv")
nnh <-  read.csv("NNH.csv")

#flspCpp(SEXP C_sexp, SEXP I_sexp, SEXP r_sexp, SEXP p_sexp, SEXP k_sexp)

r <- 0.0659
p <- 1
k <- 129000
nz <- flspCpp(nzrl$catch, nzrl$cpue, r, p, k)
nz$ll

r <- 0.328
p <- 1
k <- 239.6
sa <- flspCpp(saa$catch, saa$cpue, r, p, k)


flsp_wrapper <- function(log_params,catch,cpue){
    #browser()
    r <- exp(log_params["r"])
    k <- exp(log_params["k"])
    sp <- flspCpp(catch, cpue, r, 1, k)
    cat("r: ", r, " k: ", k, "ll: ", sp$ll, "\n")
    return(-sp$ll)
}

flsp_wrapper_grad <- function(log_params,catch,cpue){
    #browser()
    r <- exp(log_params["r"])
    k <- exp(log_params["k"])
    sp <- flspCpp(catch, cpue, r, 1, k)
    #cat("r: ", r, " k: ", k, "ll: ", sp$ll, "\n")
    return(-c(sp$ll_grad_r, sp$ll_grad_k))
}

# Without gradient
optim_nz <- optim(log(c(r=0.07,k=100000)),fn=flsp_wrapper,method="BFGS", catch = nzrl$catch, cpue=nzrl$cpue)

# With gradient
optim_nz <- optim(log(c(r=0.07,k=100000)),fn=flsp_wrapper, gr=flsp_wrapper_grad, method="BFGS", catch = nzrl$catch, cpue=nzrl$cpue)

# Nelder-Mead
optim_nz <- optim(log(c(r=0.07,k=300)),fn=flsp_wrapper,method="Nelder-Mead", catch = nzrl$catch/1000, cpue=nzrl$cpue)


optim_sa <- optim(log(c(r=0.3,k=240)),fn=flsp_wrapper,method="Nelder-Mead", catch = saa$catch, cpue=saa$cpue)
optim_sa <- optim(log(c(r=0.3,k=240)),fn=flsp_wrapper,method="BFGS", catch = saa$catch, cpue=saa$cpue)


library(FLCore)
load("om.RData")
om_c <- c(catch(om))
om_i <- c(stock(om))

r <- 0.7
p <- 1
k <- 100000
test <- flspCpp(om_c, om_i, r, p, k)
optim_om <- optim(log(c(r=r,k=k)),fn=flsp_wrapper,method="Nelder-Mead", catch = om_c, cpue=om_i)
optim_om <- optim(log(c(r=r,k=k)),fn=flsp_wrapper,method="BFGS", catch = om_c, cpue=om_i)
optim_om <- optim(log(c(r=r,k=k)),fn=flsp_wrapper, gr=flsp_wrapper_grad, method="BFGS", catch = om_c, cpue=om_i)

nlom <- nlminb(log(c(r=r, k=k)), objective = flsp_wrapper, gradient=flsp_wrapper_grad, catch = om_c, cpue=om_i)
nlom2 <- nlminb(log(c(r=r, k=k)), objective = flsp_wrapper, catch = om_c, cpue=om_i)

supp_fun <- function(pars){
    r <- pars[1]
    k <- pars[2]
    if (r<0.6|| r > 0.8 || k < 1e-5 || k > 1e9)
        return(FALSE)
    return(TRUE)
}

flsp_run_wrapper <- function(params){
    #browser()
    r <- (params[1])
    k <- (params[2])
    sp <- flspCpp(om_c, om_i, r, 1, k)
    cat("r: ", r, " k: ", k, "ll: ", sp$ll, "\n")
    return(-sp$ll)
}


test <- flspCpp(om_c, om_i, exp(nlom2$par["r"]), 1, exp(nlom2$par["k"]))
exp(nlom$par)
exp(nlom2$par)

library(Rtwalk)
runom <- Runtwalk(5000, dim = 2, Obj = flsp_run_wrapper, x0=c(0.7,100000), xp0=c(0.6,150000), Supp=supp_fun)

