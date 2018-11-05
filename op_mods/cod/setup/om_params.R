# parameter definitions for the cod operating model

#-------------------
# general stock info
#-------------------
stockname <- "cod"
minage <- 1
maxage <- 20
minlength <- 1
maxlength <- 150
dl <- 1

#--------
# growth
#--------

# vb growth
linf <- 125
k <- 0.15
t0 <- -0.35

# length-weight relationship
lw_alpha <- 6.7e-06
lw_beta <- 3.1

# size-at-age variance
minage_sd <- 2
init_sd <- c(seq(minage_sd, 10, length.out = 5), rep(10, 50))

#----------
# spawning
#----------
bh_mu <- 4e8
bh_lambda <- 1.067e08
mat_alpha <- -0.2
mat_l50 <- 64

#------------------
# natural mortality
#------------------
stock_m <- rep(0.2, length(minage:maxage))

#-------------
# selectivity
#-------------

# fishery
#--------

if (sel_type == "log") {
    # asymptotic
    comm_alpha <- 0.1
    comm_l50 <- 60
    fleet_sel_params <- list(alpha = comm_alpha, l50 = comm_l50)
} else if (sel_type == "dome") {
    # dome-shaped
    comm_alpha <- 10
    comm_beta <- 0.16
    comm_gamma <- 65
    fleet_sel_params <- 
        list(alpha = comm_alpha, 
             beta = comm_beta, 
             gamma = comm_gamma)
} else {
    stop("You have to choose logistic or gamma selectivity or change your ",
         "code in om_params.R")
}