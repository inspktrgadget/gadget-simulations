# script to source some information on stock from hafro data and assessments
library(gadgetSim)
library(tidyverse)
source("~/gadget/models/simulations/op_mods/params/fitting_functions.R")


paste_path <- function(...) {
    paste(list(...), collapse = "/")
}
hafro_path <- "http://data.hafro.is/assmt/2018/greenland_halibut"

# read in data
stock_sum <- read.csv(paste_path(hafro_path, "summary.csv"),
                      header = TRUE)

# spawning
bh_params <- nlm(bh_sse, p = c(200, 200), 
                 ssb = stock_sum$SSB, rec = stock_sum$Rec)

# maturity
linf <- 125
k <- 0.15
t0 <- -0.35
cod_mat <- 
    read.csv(paste_path(hafro_path, "maturity.csv"), header = TRUE) %>%
    rename_at(vars(X3:X14), funs(gsub("X", "", .))) %>%
    gather(key = age, value = mat, `3`:`14`, -Year) %>%
    mutate(age = as.numeric(as.character(age)),
           length = vb(linf, k, t0, age))

mat_params <- nlm(log_sel_sse, p = c(0.25, 80), 
                  lengths = cod_mat$length, vals = cod_mat$mat)

# selectivity
cod_sel <- 
    read.csv(paste_path(hafro_path, "fmat.csv"), header = TRUE) %>%
    rename_at(vars(X3:X14), funs(gsub("X", "", .))) %>%
    gather(key = age, value = sel, `3`:`14`, -Year) %>%
    mutate(age = as.numeric(as.character(age)),
           length = vb(linf, k, t0, age))

asym_sel_params <- nlm(log_sel_sse,  p = c(0.25, 60, 1),
                       lengths = cod_sel$length, vals = cod_sel$sel)

ds_sel_params <- nlm(ds_sel_sse, p = c(0.5, 5, 20),
                     lengths = cod_sel$lengths, vals = cod_sel$sel)