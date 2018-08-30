library(parallel)
library(Rgadget)

paste_path <- function(...) {
    paste(list(...), collapse = "/")
}

base_dir <- "/users/work/pnf1/gadget/simulations/age_data_var_rec/model_type/spp_type"
scenarios <- c("two_way_trip", "fish_down", "flat_msy")

for (i in scenarios) {
    home_wd <- getwd()
    em_dir <- paste_path(base_dir, i)
    setwd(em_dir)
    mclapply(1:100, function(x) {
        gadget.iterative(rew.sI = TRUE,
                         main = sprintf("reps/rep%s/main", x),
                         grouping = list(len1 = c("spr.si.1", "aut.si.1"),
                                         len2 = c("spr.si.2", "aut.si.2"),
                                         len3 = c("spr.si.3", "aut.si.3"),
                                         len4 = c("spr.si.4", "aut.si.4"),
                                         len5 = c("spr.si.5", "aut.si.5")),
                         wgts = sprintf("reps/rep%s/WGTS", x),
                         run.serial = TRUE)
    }, mc.cores = 25)
    setwd(home_wd)
}
