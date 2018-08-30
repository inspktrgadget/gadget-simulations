# find Fmsy and other limit values based on stock parameters
# must either source om_setup.R or source this file from there

# these are still needed to compute F_msy
bh_mu <- 6e08
bh_lambda <- 5e06
mat_alpha <- 2
mat_l50 <- 12.5

f_args <- list(fish_mort = seq(0, 2, 0.01), nat_mort = stock_m, ages = minage:maxage,
               growth_params = list(linf = linf, k = k, t0 = t0),
               lw_params = list(alpha = alpha, beta = beta),
               rec_params = list(mu = bh_mu, lambda = bh_lambda),
               mat_params = list(alpha = mat_alpha, l50 = mat_l50),
               sel_params = exp_sel_params)

fmsy <- do.call(f_msy, f_args)
f_limits <- do.call(f_lim, c(f_args, list(lim = 0.85)))
f_low <- f_limits[1]
f_high <- f_limits[2]
