# parameter definitions for the capelin operating model

#-------------------
# general stock info
#-------------------
stockname <- "capelin"
minage <- 1
maxage <- 5
minlength <- 1
maxlength <- 25
dl <- 1

#--------
# growth
#--------

# vb growth
linf <- 20
k <- 0.4
t0 <- -0.33

# length-weight relationship
lw_alpha <- 7.1e-06
lw_beta <- 3.15

# size-at-age variance
minage_sd <- 1
init_sd <- c(c(minage_sd,1.5,2), rep(2, 50))

#----------
# spawning
#----------
bh_mu <- 6e08
bh_lambda <- 5e06
mat_alpha <- -2
mat_l50 <- 12.5

#------------------
# natural mortality
#------------------
stock_m <- c(rep(1, (maxage - minage)), 1.5)

#-------------
# selectivity
#-------------

# fishery
#--------

# asymptotic
# comm_alpha <- 1
# comm_l50 <- 16
# fleet_sel_params <- list(alpha = comm_alpha, l50 = comm_l50)

# dome-shaped
comm_alpha <- 17
comm_beta <- 0.04
comm_gamma <- 35
fleet_sel_params <- 
    list(alpha = comm_alpha, 
         beta = comm_beta, 
         gamma = comm_gamma)