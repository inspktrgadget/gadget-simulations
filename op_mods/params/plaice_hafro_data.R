# script to source some information on stock from hafro data and assessments
library(gadgetSim)
library(tidyverse)
source("~/gadget/models/simulations/op_mods/params/fitting_functions.R")


paste_path <- function(...) {
    paste(list(...), collapse = "/")
}
hafro_path <- "http://data.hafro.is/assmt/2018/plaice"

# read in data
stock_sum <- read.csv(paste_path(hafro_path, "summary.csv"),
                      header = TRUE)

# spawning
bh_params <- nlm(bh_sse, p = c(200, 200), 
                 ssb = stock_sum$SSB, rec = stock_sum$Rec)

# maturity
linf <- 55
k <- 0.14
t0 <- -1.35
plaice_mat <- 
    read.csv(paste_path(hafro_path, "maturity.csv"), header = TRUE) %>%
    rename_at(vars(X3:X10), funs(gsub("X", "", .))) %>%
    gather(key = age, value = mat, `3`:`10`) %>%
    mutate(age = as.numeric(as.character(age)),
           length = vb(linf, k, t0, age))

mat_params <- nlm(log_sel_sse, p = c(0.25, 80), 
                  lengths = plaice_mat$length, vals = plaice_mat$mat)

# selectivity
plaice_sel <- 
    read.csv(paste_path(hafro_path, "catage.csv"), header = TRUE) %>%
    rename_at(vars(X3:X16), funs(gsub("X", "", .))) %>%
    gather(key = age, value = sel, `3`:`16`, -Year) %>%
    mutate(age = as.numeric(as.character(age)),
           length = vb(linf, k, t0, age))

asym_sel_params <- nlm(log_sel_sse,  p = c(0.25, 60, 1),
                       lengths = plaice_sel$length, vals = plaice_sel$sel)

ds_sel_params <- nlm(ds_sel_sse, p = c(0.5, 5, 20),
                     lengths = plaice_sel$lengths, vals = plaice_sel$sel)