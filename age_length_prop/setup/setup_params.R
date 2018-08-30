# script to setup params for estimation models

# get known params from operating model to use as a guide for bounds
linf <- as.numeric(gr_params[1])
k <- as.numeric(gr_params[2])
bh_mu <- as.numeric(op_mod$stocks$spawnfile$recruitment[2])
bh_lam <- as.numeric(op_mod$stocks$spawnfile$recruitment[3])
fleet_sel_alpha <- as.numeric(op_mod$fleets[[1]]$suitability[4])
fleet_sel_l50 <- as.numeric(op_mod$fleets[[1]]$suitability[5])

# define functions for setting start values and bounds
init_val <- function(val) {
    return(val * runif(1, 0.9, 1.1))
}

low_bnd <- function(val, scalar = 0.1) {
    return(val - abs(val * scalar))
}

up_bnd <- function(val, scalar = 0.1) {
    return(val + abs(val * scalar))
}


# create params file for estimation model
params <-
    make_gadget_params() %>%
    init_params(paste(stockname, "linf", sep = "."),
                init_val(linf),
                low_bnd(linf),
                up_bnd(linf)) %>%
    init_params(paste(stockname, "k", sep = "."),
                init_val(k),
                low_bnd(k),
                up_bnd(k)) %>%
    init_params(paste(stockname, "t0", sep = "."), init_val(-0.5), -2, 0) %>%
	init_params("bbin", 1, 1, 1e8) %>%
	init_params("bbin.mult", 1, 1, 1e8) %>%
    init_params(paste(stockname, "init.age.mult", sep = "."), 10, 1, 1e6) %>%
    init_params(paste(stockname, "init.area.mult", sep = "."), 10, 1, 1e6) %>%
    init_params(paste(stockname, "bh.mu", sep = "."),
                init_val(bh_mu),
                low_bnd(bh_mu),
                up_bnd(bh_mu)) %>%
    init_params(paste(stockname, "bh.lam", sep = "."),
                init_val(bh_lam),
                low_bnd(bh_lam),
                up_bnd(bh_lam)) %>%
    init_params(paste(stockname, "lin.alpha", sep = "."),
                init_val(fleet_sel_alpha),
                low_bnd(fleet_sel_alpha),
                up_bnd(fleet_sel_alpha)) %>%
    init_params(paste(stockname, "lin.l50", sep = "."),
                init_val(fleet_sel_l50),
                low_bnd(fleet_sel_l50),
                up_bnd(fleet_sel_l50)) %>%
    init_params(paste(stockname, "spr.alpha", sep = "."), 0.1, 1e-05, 3) %>%
    init_params(paste(stockname, "spr.l50", sep = "."), 15, 1, 60) %>%
    init_params(paste(stockname, "aut.alpha", sep = "."), 0.1, 1e-05, 3) %>%
    init_params(paste(stockname, "aut.l50", sep = "."), 15, 1, 60)
