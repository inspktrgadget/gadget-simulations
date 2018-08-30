# script to source some information on stock from hafro data and assessments
library(gadgetSim)
library(tidyverse)
source("~/gadget/models/simulations/op_mods/params/fitting_functions.R")


paste_path <- function(...) {
    paste(list(...), collapse = "/")
}
hafro_path <- "http://data.hafro.is/assmt/2018/herring"

# read in data
stock_sum <- read.csv(paste_path(hafro_path, "summary.csv"),
                      header = TRUE)

# spawning
bh_params <- nlm(bh_sse, p = c(200, 200), 
                 ssb = stock_sum$SSB, rec = stock_sum$Rec)

# maturity
linf <- 30
k <- 0.33
t0 <- -1.35
herring_mat <- 
    read.csv(paste_path(hafro_path, "maturity.csv"), header = TRUE) %>%
    rename_at(vars(X2:X13.), funs(gsub("X", "", .))) %>%
    gather(key = age, value = mat, `2`:`13.`) %>%
    mutate(age = as.numeric(as.character(age)),
           length = vb(linf, k, t0, age))

mat_params <- nlm(log_sel_sse, p = c(0.1, 24), 
                  lengths = herring_mat$length, vals = herring_mat$mat)

# selectivity
herring_mort <- 
    read.csv(paste_path(hafro_path, "natmort.csv"), header = TRUE) %>%
    rename_at(vars(X2:X13.), funs(gsub("X", "", .))) %>%
    gather(key = age, value = mort, `2`:`13.`, -Year) %>%
    mutate(age = as.numeric(as.character(age))) %>%
    group_by(age) %>%
    summarize(mean_m = mean(mort))
