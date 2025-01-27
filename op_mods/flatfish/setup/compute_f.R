# find Fmsy and other limit values based on stock parameters
# must either source om_setup.R or source this file from there

f_args <- list(fish_mort = seq(0, 2, 0.01), nat_mort = stock_m, 
               ages = minage:maxage,
               growth_fun = vb,
               growth_params = list(linf = linf, k = k, t0 = t0),
               lw_params = list(alpha = lw_alpha, beta = lw_beta),
               rec_fun = bev_holt,
               rec_params = list(mu = bh_mu, lambda = bh_lambda),
               mat_fun = logistic_selectivity,
               mat_params = list(alpha = -mat_alpha, l50 = mat_l50),
               sel_fun = if (sel_type == "log") {
                 logistic_selectivity
                 } else if (sel_type == "dome") {
                   gamma_selectivity
                 },
               sel_params = fleet_sel_params)

fmsy <- do.call(f_msy, f_args)
f_limits <- do.call(f_lim, c(f_args, list(lim = 0.85)))
f_low <- f_limits[1]
f_high <- f_limits[2]
