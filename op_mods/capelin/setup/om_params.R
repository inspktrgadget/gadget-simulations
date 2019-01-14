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
gr_beta <- 1000
gr_mlgg <- 5

# length-weight relationship
lw_alpha <- 7.1e-06
lw_beta <- 3.15

# size-at-age variance
minage_sd <- 0.5
init_sd <- c(c(minage_sd,0.75,1), rep(1, 50))
#minage_sd <- 1
#init_sd <- c(c(minage_sd,1.5,2), rep(2, 50))

#----------
# spawning
#----------
bh_mu <- 8e10
bh_lambda <- 5e07
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

if (sel_type == "log") {
    # asymptotic
    comm_alpha <- 1
    comm_l50 <- 9
    fleet_sel_params <- list(alpha = comm_alpha, l50 = comm_l50)
} else if (sel_type == "dome") {
    # dome-shaped
    comm_alpha <- 12
    comm_beta <- 0.08
    comm_gamma <- 15
    fleet_sel_params <- 
        list(alpha = comm_alpha, 
             beta = comm_beta, 
             gamma = comm_gamma)
} else {
    stop("You have to choose logistic or gamma selectivity or change your ",
         "code in om_params.R")
}

