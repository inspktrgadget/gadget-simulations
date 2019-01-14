library(parallel)
library(gadgetSim)
library(tidyverse)

paste_path <- function(...) {
  paste(list(...), collapse = "/")
}

model_dir <-"~/gadget/simulations/age_len_prop"
models <- dir(model_dir)[grep("^age_", dir(model_dir))]
spp <- "capelin"
#spp <- c("cod", "capelin", "flatfish")
scenarios <- c("fish_down", "flat_msy", "two_way_trip")

depletion <- 
  lapply(models, function(i) {
    lapply(spp, function(j) {
      lapply(scenarios, function(k) {
        on.exit(print(paste_path(i,j,k)))
        em_path <- 
          paste_path(model_dir, i, j, k)
        om_path <- paste_path(model_dir, "op_mods", "var_rec", j, k)
        # not all replicants finished; get the ones that did
        finito_reps <- 
          vapply(1:100, function(x) {
            reps_path <- paste_path(em_path, "reps", sprintf("rep%s", x))
            wgts <- dir(reps_path)
            if ("WGTS" %in% wgts) {
              wgts_path <- paste_path(reps_path, "WGTS")
              params_ <- dir(wgts_path)
              if ("params.final" %in% params_) {
                params <- 
                  readLines(paste_path(em_path, "reps", 
                                       sprintf("rep%s/WGTS/params.final", x)))
                if (length(params) > 0) {
                  return(x)
                } else {
                  return(0)
                }
              } else {
                return(0)
              }
            } else {
              return(0)
            }
          }, numeric(1))
        reps <- finito_reps[finito_reps > 0]
        # read in StockStd for each EM and the OM
        em_std <- 
          parallel::mclapply(reps, function(x) {
            tmp <- 
              tryCatch({
              get_stock_std(main = sprintf("reps/rep%s/WGTS/main.final", x),
                            params_file = sprintf("reps/rep%s/WGTS/params.final", 
                                                  x),
                            fit_dir = sprintf("reps/rep%s/WGTS", x),
                            path = em_path)
              }, error = function(e) return(NULL))
            if (!is.null(tmp)) {
              tmp <- mutate(tmp, rep = x)
            }
            return(tmp)
          })
        if (!all(vapply(em_std, is.null, logical(1)))) {
          em_std <- 
						em_std %>%
            do.call("rbind", .) %>%
            mutate_all(funs(as.numeric))

        om_std <- get_stock_std(path = om_path)
        # group to total numbers
        em_summary <- 
          em_std %>%
          group_by(rep, year, step, area, age) %>%
          summarize(number = sum(number),
                    biomass = sum(number * weight)) %>%
          ungroup()
        om_summary <- 
          om_std %>%
          group_by(year, step, area, age) %>%
          summarize(number = sum(number),
                    biomass = sum(number * weight)) %>%
          ungroup() %>%
          mutate(rep = 0)
        # bind the two data.frames together to calculate depletion and 
        # terminal year biomass
        mat_age <- 
          switch(j,
                 cod = 4,
                 capelin = 2,
                 flatfish = 8)
        bm_depletion <- 
          rbind(om_summary, em_summary) %>%
          filter(year %in% c(40, 120),
                 step == 4,
                 age >= mat_age) %>%
          group_by(rep, year) %>%
          summarize(biomass = sum(biomass)) %>%
          summarize(terminal_bm = biomass[year == 120],
                    depletion = biomass[year == 120] / biomass[year == 40]) %>%
          mutate(true_terminal_bm = terminal_bm[rep == 0],
                 true_depletion = depletion[rep == 0]) %>%
          filter(rep > 0) %>%
          mutate(model = i, spp = j, scenario = k)
        return(bm_depletion)
        } else {
          return(NULL)
        }
      }) %>% do.call("rbind", .)
    }) %>% do.call("rbind", .)
  }) %>% 
  do.call("rbind", .) %>%
  select(model, spp, scenario, rep:true_depletion)


write.csv(depletion, file = paste_path(model_dir, "results", "depletion.csv"),
          quote = FALSE, row.names = FALSE)
