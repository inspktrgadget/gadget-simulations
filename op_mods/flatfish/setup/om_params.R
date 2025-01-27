# parameter definitions for the flatfish operating model

#-------------------
# general stock info
#-------------------
stockname <- "flatfish"
minage <- 1
maxage <- 30
minlength <- 1
maxlength <- 100
dl <- 1

#--------
# growth
#--------

# vb growth
linf <- 55
k <- 0.14
t0 <- -1.3
gr_beta <- 30
gr_mlgg <- 15

# length-weight relationship
lw_alpha <- 9.6e-06
lw_beta <- 3.0

# size-at-age variance
minage_sd <- 2
init_sd <- c(c(minage_sd,4,6), rep(6, 50))

#----------
# spawning
#----------
bh_mu <- 2e8
bh_lambda <- 1.067e08
mat_alpha <- -0.25
mat_l50 <- 30

#------------------
# natural mortality
#------------------
stock_m <- rep(0.1, length(minage:maxage))

#-------------
# selectivity
#-------------

# fishery
#--------

if (sel_type == "log") {
    # asymptotic
    comm_alpha <- 0.2
    comm_l50 <- 30
    fleet_sel_params <- list(alpha = comm_alpha, l50 = comm_l50)
} else if (sel_type == "dome") {
    # dome-shaped
    comm_alpha <- 11
    comm_beta <- 0.1
    comm_gamma <- 45
    fleet_sel_params <- 
        list(alpha = comm_alpha, 
             beta = comm_beta, 
             gamma = comm_gamma)
} else {
    stop("You have to choose logistic or gamma selectivity or change your ",
         "code in om_params.R")
}
