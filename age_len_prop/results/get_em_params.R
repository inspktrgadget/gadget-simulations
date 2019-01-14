library(parallel)
library(gadgetSim)
library(tidyverse)

paste_path <- function(...) {
  paste(list(...), collapse = "/")
}

model_dir <-"~/gadget/simulations/age_len_prop"
models <- dir(model_dir)[grep("^age", dir(model_dir))]
spp <- "capelin"
#spp <- c("cod", "capelin", "flatfish")
scenarios <- c("fish_down", "flat_msy", "two_way_trip")


params <- 
  lapply(models, function(i) {
    lapply(spp, function(j) {
      lapply(scenarios, function(l) {
        on.exit(print(paste_path(i,j,l)))
        sel_type <- "log"
        params2keep <-
          c("linf", "k", "t0", "mat_alpha", "mat_l50", "bh_mu", "bh_lambda",
            "spr_alpha", "spr_l50", "aut_alpha", "aut_l50")
        em_path <- 
          paste_path(model_dir, i, j, l)
        om_path <- paste_path("~/gadget/simulations/op_mods", j, l)
        reps <- 1:100
        # now get the parameters for both estimation and operating models
        em_params <- 
          lapply(reps, function(x) {
            rep_id <- sprintf("rep%s", x)
            params <- 
              read.table(paste_path(em_path, "reps", rep_id, 
                                    "WGTS/params.final"),
                         comment.char = ";", header = TRUE,
                         stringsAsFactors = FALSE) %>%
              separate(switch, into = c("spp", "switch"), sep = "\\.",
                       extra = "merge") %>%
              mutate(switch = gsub("\\.", "_", switch)) %>%
              filter(switch %in% params2keep) %>%
              mutate(rep = x) %>%
              select(spp, rep, switch, value)
          }) %>% do.call("rbind", .)
        # create new environment to place the om params
        om_params <- new.env()
        source(paste_path("~/gadget/simulations/op_mods", j, 
                            "setup", "om_params.R"), local = om_params)
        params2keep <-
          c("linf", "k", "t0", "mat_alpha", "mat_l50", "bh_mu", "bh_lambda",
            "comm_alpha", "comm_l50")
        rm(list=ls(om_params)[!(ls(om_params) %in% params2keep)], 
           envir = om_params)
        om_params_df <- 
          lapply(ls(om_params), function(x) {
            return(data.frame(spp = j, rep = 0, switch = x, 
                              value = get(x, envir = om_params)))
          }) %>% do.call("rbind", .)
        params <- 
          rbind(om_params_df, em_params) %>%
          mutate(model = i,
                 scenario = l) %>%
          filter(!grepl("aut|spr", switch)) %>%
          group_by(switch) %>%
          mutate(true_value = value[rep == 0]) %>%
          select(model, spp, scenario, rep, switch, value, true_value)
        return(params)
      }) %>% do.call("rbind", .)
    }) %>% do.call("rbind", .)
  }) %>% 
  do.call("rbind", .)


write.csv(params, file = paste_path(model_dir, "results", "em_params.csv"),
          quote = FALSE, row.names = FALSE)



