# script to setup params for estimation models

# define fleet selectivity
fleet_sel_alpha <- fleet_sel_params$alpha
fleet_sel_l50 <- fleet_sel_params$l50

# define functions for setting start values and bounds
init_val <- function(val, scale = 0.1) {
    return(val * runif(1, 1 - scale, 1 + scale))
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
	init_params(paste(stockname, "bbin", sep = "."), 1, 1, 1e8) %>%
	init_params(paste(stockname, "bbin.mult", sep = "."), 1, 1, 1e8) %>%
    init_params(paste(stockname, "init.age.mult", sep = "."), 10, 1, 1e6) %>%
    init_params(paste(stockname, "init.area.mult", sep = "."), 10, 1, 1e6) %>%
    init_params(paste(stockname, "bh.mu", sep = "."),
                init_val(bh_mu),
                low_bnd(bh_mu),
                up_bnd(bh_mu)) %>%
    init_params(paste(stockname, "bh.lambda", sep = "."),
                init_val(bh_lambda),
                low_bnd(bh_lambda),
                up_bnd(bh_lambda)) %>%
    init_params(paste(stockname, "mat.alpha", sep = "."),
                init_val(mat_alpha),
                low_bnd(mat_alpha),
                up_bnd(mat_alpha)) %>%
    init_params(paste(stockname, "mat.l50", sep = "."),
                init_val(mat_l50),
                low_bnd(mat_l50),
                up_bnd(mat_l50)) %>%
    init_params(paste(stockname, "comm.alpha", sep = "."),
                init_val(fleet_sel_alpha),
                low_bnd(fleet_sel_alpha),
                up_bnd(fleet_sel_alpha)) %>%
    init_params(paste(stockname, "comm.l50", sep = "."),
                init_val(fleet_sel_l50),
                low_bnd(fleet_sel_l50),
                up_bnd(fleet_sel_l50)) %>%
    init_params(paste(stockname, "spr.alpha", sep = "."), 0.1, 1e-05, 3) %>%
    init_params(paste(stockname, "spr.l50", sep = "."), 15, 1, 60) %>%
    init_params(paste(stockname, "aut.alpha", sep = "."), 0.1, 1e-05, 3) %>%
    init_params(paste(stockname, "aut.l50", sep = "."), 15, 1, 60)


if (lik_type %in% c("mvn", "mvlog")) {
    params <- 
        params %>%
        init_params(paste(stockname, "spr.ldist.sigma", sep = "."), 
                    1, 1e-02, 20) %>%
        init_params(paste(stockname, "aut.ldist.sigma", sep = "."), 
                    1, 1e-02, 20) %>%
        init_params(paste(stockname, "comm.ldist.sigma", sep = "."), 
                    1, 1e-02, 20) %>%
        init_params(paste(stockname, "spr.aldist.sigma", sep = "."), 
                    1, 1e-02, 20) %>%
        init_params(paste(stockname, "aut.aldist.sigma", sep = "."), 
                    1, 1e-02, 20) %>%
        init_params(paste(stockname, "comm.aldist.sigma", sep = "."), 
                    1, 1e-02, 20)
}
