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

mod_list <- 
  lapply(models, function(i) {
    lapply(spp, function(j) {
      lapply(scenarios, function(k) {
	on.exit(print(paste_path(i,j,k)))
        em_path <- 
          paste_path(model_dir, i, j, k)
        om_path <- paste_path(model_dir, "op_mods", "var_rec", j, k)
        reps <- 1:100
        # read in StockStd for each EM and the OM
        em_std <- 
          parallel::mclapply(reps, function(x) {
            get_stock_std(main = sprintf("reps/rep%s/WGTS/main.final", x),
                          params_file = sprintf("reps/rep%s/WGTS/params.final", 
                                                x),
                          fit_dir = sprintf("reps/rep%s/WGTS", x),
                          path = em_path) %>%
              mutate(rep = x) %>%
              return()
          }, mc.cores = 8) %>%
          do.call("rbind", .) %>%
          mutate_all(funs(as.numeric))
        om_std <- get_stock_std(path = om_path)
        # group to total numbers
        em_summary <- 
          em_std %>%
          group_by(rep, year, step, area) %>%
          summarize(number = sum(number),
                    biomass = sum(number * weight)) %>%
          ungroup()
        om_summary <- 
          om_std %>%
          group_by(year, step, area) %>%
          summarize(number = sum(number),
                    biomass = sum(number * weight)) %>%
          ungroup() %>%
          mutate(rep = 0)
        model_output <- 
          rbind(om_summary, em_summary) %>%
          mutate(model = i, spp = j, scenario = k)
	return(model_output)
      }) %>% do.call("rbind", .)
    }) %>% do.call("rbind", .)
  }) %>% 
  do.call("rbind", .) %>%
  select(model, spp, scenario, rep, year, step, area, number, biomass)


save.image(file = paste_path(model_dir, "results", "model_output.RData"))

# # now get the parameters for both estimation and operating models
# em_params <- 
#   lapply(reps, function(x) {
#     rep_id <- sprintf("rep%s", x)
#     params <- 
#       read.table(paste_path(em_path, "reps", rep_id, 
#                             "WGTS/params.final"),
#                  comment.char = ";", header = TRUE) %>%
#       separate(switch, into = c("spp", "switch"), sep = "\\.",
#                extra = "merge") %>%
#       select(spp, switch, value)
#   })
