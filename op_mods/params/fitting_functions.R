# function definitions to fit various curves to icelandic assessment data

#-------------------------------------------------------------------------------
# bev-holt spawning
bh_opt <- function(params, ssb) {
    mu <- params[1]
    lambda <- params[2]
    return(bev_holt(ssb, mu, lambda))
}

bh_sse <- function(params, ssb, rec) {
    rec_hat <- bh_opt(params, ssb)
    return(sum((rec - rec_hat)^2))
}

#-------------------------------------------------------------------------------
# logistic selectivity
log_sel_opt <- function(params, lengths) {
    alpha <- params[1]
    l50 <- params[2]
    max_prop <- ifelse(length(params) == 3, params[3], 1)
    return(logistic_selectivity(lengths, alpha, l50, max_prop))
}

log_sel_sse <- function(params, lengths, vals) {
    y_hat <- log_sel_opt(params, lengths)
    return(sum((vals - y_hat)^2))
}

#-------------------------------------------------------------------------------
# dome-shaped selectivity
ds_sel_opt <- function(params, lengths) {
    alpha <- params[1]
    beta <- params[2]
    gamma <- params[3]
    return(gamma_selectivity(lengths, alpha, beta, gamma))
}

ds_sel_sse <- function(params, lengths, vals) {
    f_hat <- ds_sel_opt(params, lengths)
    return(sum((vals - f_hat)^2))
}
