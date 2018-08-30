library(Rgadget)
library(gadgetSim)

paste_path <- function(...) {
    return(paste(list(...), collapse = "/"))
}

proj_dir <- "~/gadget/models/simulations/msd_up_advr"

age_props <- sprintf("age_prop_%s", c(1, 0.5, 0.2, 0.1, 0.01))
age_samples <- sprintf("age_samples_%s", c(1000, 500, 200, 100, 50))

test <- 
    lapply(c(age_props, age_samples), function(x) {
        parallel::mclapply(c("cod", "flatfish", "capelin"), function(y) {
            parallel::mclapply(c("two_way_trip", "fish_down", "flat_msy"), function(z) {
                lapply(1:10, function(i) {
                    mod_num <- sprintf("reps/rep%s", i)
                    tmp <- 
                        get_stock_std(main = paste_path(mod_num, "WGTS",
                                                        "main.final"),
                                      params_file = paste_path(mod_num, 
                                                               "WGTS", 
                                                               "params.final"),
                                      path = paste_path(proj_dir, x, y, z),
                                      fit_dir = paste_path(mod_num, "WGTS"))
                    tmp <- 
                        tmp[[y]] %>%
                        filter(step == 1, year > 20) %>%
                        mutate(rep = i)
                    return(tmp)
                }) %>%
                do.call("rbind", .) %>%
                mutate(scenario = z)
            }, mc.cores = 3) %>%
            do.call("rbind", .) %>%
            mutate(spp = y)
        }, mc.cores = 3) %>%
        do.call("rbind", .) %>%
        mutate(model = x)
    }) %>%
    do.call("rbind", .)

