# this is just a script to check and make sure each estimation model
# has sufficient fish for computing likelihoods on

library(gadgetSim)
library(tidyverse)
source("~/gadget/models/simulations/functions/check_est_mods.R")

est_mod_path <- "~/gadget/models/simulations/comp_lik"
data_scenario <- c("dr", "dp")
#likelihood <- c("ss", "mltnom")
lik_type <- c("ss", "mltnom")
sel_type <- c("log", "dome")
scenarios <- 
    expand.grid(data = data_scenario,
                lik = lik_type,
                sel = sel_type) %>%
    unite(scenario) %>%
    pull(scenario)
spp <- c("cod", "capelin", "flatfish")
fishing_scenarios <- c("fish_down", "flat_msy", "two_way_trip")
sel_type <- "dome"

stds <- get_timeseries(est_mod_path, scenarios, spp, fishing_scenarios)
ts_plots <- make_timeseries_plots(stds)
depletion <- get_depletion(stds)
est_params <- get_params(est_mod_path, scenarios, spp, fishing_scenarios)
om_params <- get_om_params("~/gadget/models/simulations/op_mods", spp)

params <- 
    est_params %>%
    select(model, spp, fish_scenario, switch, value) %>%
    left_join(om_params, by = c("spp", "switch")) %>%
    mutate(diff = value - true_value)

dep_params <- left_join(params, depletion, 
                        by = c("model", "spp", "fish_scenario"))

# plot depletion by param difference values to look for any patterns
g <- 
    ggplot(data=dep_params, aes(x=diff, y=min_dep, color = spp)) + 
    geom_point() + facet_wrap(~switch, scales = "free")

# set up a generalized linear model to statistically detect any patterns
glm_dat <-
    dep_params %>%
    group_by(model, spp, fish_scenario, switch) %>%
    summarize(failed = ifelse(min_dep == 0, 1, 0),
              diff = diff) %>%
    spread(key = switch, value = diff)

spawning_mod <- glm(failed ~ bh.lambda * bh.mu * spp, 
                    family = binomial(link = "logit"),
                    data = glm_dat)
maturity_mod <- glm(failed ~ mat.alpha * mat.l50 * spp, 
                    family = binomial(link = "logit"),
                    data = glm_dat)
growth_mod <- glm(failed ~ linf * k * t0 * spp, 
                  family = binomial(link = "logit"),
                  data = glm_dat)
fleet_mod <- glm(failed ~ comm.alpha * comm.l50 * spp, 
                 family = binomial(link = "logit"),
                 data = glm_dat)

# this is a test to see if the ratio between mu and lambda for bev-holt matters
params <- 
    params %>% 
    select(model, switch, value) %>%
    spread(key = switch, value = value) %>% 
    mutate(spawn.ratio = capelin.bh.mu / capelin.bh.lambda) %>%
    gather(key = switch, value = value, 2:ncol(.))

crash_mods <- c("dp_mvlog", "dp_ss")
bad_mods <- params %>% filter(model %in% crash_mods)

params_plot <- 
    ggplot(data=params, aes(x=switch, y=value, color = model)) + 
    geom_point() + facet_wrap(~switch, scales = "free") + 
    geom_point(data = bad_mods, aes(x=switch, y=value), size = 5)