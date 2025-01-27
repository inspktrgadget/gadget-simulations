# basic script to setup a Gadget operating model
library(gadgetSim)
library(tidyverse)

paste_path <- function(...) {
  paste(list(...), collapse = "/")
}

spp <- "cod"
om_dir <- sprintf("~/gadget/models/simulations/op_mods/%s", spp)

# read in parameters 
sel_type <- "dome"
source(paste_path(om_dir, "setup", "om_params.R"))

#------------------------------
# setup time and area
st_year <- 1
end_year <- 120
time <- make_gadget_timefile(st_year, end_year, "quarterly")
area <-
  make_gadget_areafile(areas = 1, size = 1e6,
                       temp_data = expand.grid(year = st_year:end_year, 
                                               step = 1:4,
                                               area = 1, temp = 3) %>%
                         arrange(year, step, area, temp))
#------------------------------
# setup the stock
# setup basic stock information
reflength <- seq(minlength, maxlength, dl)
stock_info <-
  list(stockname = stockname, livesonareas = 1, 
       minage = minage, maxage = maxage,
       minlength = minlength, maxlength = maxlength, dl = dl)

# setup refweightfile
stock_refwgt <-
  data.frame(len = reflength,
             weight = lw_alpha * reflength ^ lw_beta)

# setup growth
stock_growth <-
  list(growthfunction = "lengthvbsimple",
       growthparameters = c(linf, k, lw_alpha, lw_beta),
       beta = gr_beta, 
       maxlengthgroupgrowth = gr_mlgg)

# setup naturalmortality

# setup initial conditions
init_data <-
  normalparamfile(age = seq(minage, maxage, 1),
                  area = 1,
                  age.factor = 1e4 * exp(-cumsum(stock_m)),
                  area.factor = 1,
                  mean = vb_formula(stockname, minage:maxage,
                                    params = list(linf = linf, 
                                                  k = k, 
                                                  t0 = t0)),
                  sd = init_sd[minage:maxage],
                  alpha = lw_alpha,
                  beta = lw_beta)
stock_initcond <- list(normalparamfile = init_data)

# setup spawning
stock_spawnfile <-
  make_gadget_spawnfile(
    stockname = stockname,
    start_year = st_year,
    end_year = end_year,
    proportionfunction = c("exponential", mat_alpha, mat_l50),
    recruitment = bev_holt_formula(stockname, params = c(bh_mu, bh_lambda)),
    stockparameters = c(vb(linf, k, t0, minage), minage_sd, lw_alpha, lw_beta)
  )

# create gadget stockfile
stock <-
  make_gadget_stockfile(stock = stock_info,
                        refweightfile = stock_refwgt,
                        growth = stock_growth,
                        naturalmortality = stock_m,
                        iseaten = 1,
                        initialconditions = stock_initcond,
                        spawning = stock_spawnfile)

#-------------------------------
# setup the fleets
source(paste_path(om_dir, "setup", "compute_f.R"))
lin_flt_data <- 
  expand.grid(year = (st_year + 40):end_year, 
              steps = 1:4, 
              area = 1, 
              fleetname = "lin") %>%
  arrange(year)

# base argument list to make_gadget_fleet
fleet_args <- 
  list(type = "linearfleet",
       suitability = 
         if (sel_type == "log") {
           make_exponentiall50_suit("lin", stockname, params = fleet_sel_params)
         } else if (sel_type == "dome") {
           make_gamma_suit("lin", stockname, params = fleet_sel_params)
         } else {
           stop("You didn't specify selectivity type")
         })

# two-way trip scenario
two_way_trip <- 
  lin_flt_data %>%
  mutate(scaling = c(seq(0.01, f_high, 
                         length.out = nrow(lin_flt_data) * 0.6),
                     seq(f_high, fmsy, 
                         length.out = nrow(lin_flt_data) * 0.4)))
twt_args <- modifyList(fleet_args, list(amount = two_way_trip))
twt_fleet <- make_gadget_fleet(lin = twt_args)

# fish down scenario
fish_down <- 
  mutate(lin_flt_data, scaling = seq(0.01, f_high, 
                                     length.out = nrow(lin_flt_data)))
fish_down_args <- modifyList(fleet_args, list(amount = fish_down))
fish_down_fleet <- make_gadget_fleet(lin = fish_down_args)

# flat msy scenario
flat_msy <- mutate(lin_flt_data, scaling = fmsy)
flat_msy_args <- modifyList(fleet_args, list(amount = flat_msy))
flat_msy_fleet <- make_gadget_fleet(lin = flat_msy_args)

#------------------------------
# simulate the Gadget models

# two-way trip scenario
two_way_trip <- 
  lin_flt_data %>%
  mutate(scaling = c(seq(0.01, f_high, 
                         length.out = nrow(lin_flt_data) * 0.6),
                     seq(f_high, fmsy, 
                         length.out = nrow(lin_flt_data) * 0.4)))
twt_args <- modifyList(fleet_args, list(amount = two_way_trip))
twt_fleet <- make_gadget_fleet(lin = twt_args)

# fish down scenario
fish_down <- 
  mutate(lin_flt_data, 
         scaling = seq(0.01, f_high, length.out = nrow(lin_flt_data)))
fish_down_args <- modifyList(fleet_args, list(amount = fish_down))
fish_down_fleet <- make_gadget_fleet(lin = fish_down_args)

# flat msy scenario
flat_msy <- mutate(lin_flt_data, scaling = fmsy)
flat_msy_args <- modifyList(fleet_args, list(amount = flat_msy))
flat_msy_fleet <- make_gadget_fleet(lin = flat_msy_args)

#------------------------------
# simulate the Gadget models

simulate_model <- function(fleet, scenario) {
  path <- paste_path(om_dir, scenario)
  mod <- 
    make_gadget_model(time = time, 
                      area = area, 
                      stock = stock, 
                      fleet = fleet)
  write_gadget_model(mod, path = path)
  stock_std <- get_stock_std(path = path)
  ts_plot <- 
    ggplot(data=filter(stock_std, step == 1, year >= 20), 
           aes(x = year, y = number)) + 
    geom_line() + facet_wrap(~age, scales = "free_y")
  return(ts_plot)
}
twt_mod <- simulate_model(twt_fleet, "two_way_trip")
fd_mod <- simulate_model(fish_down_fleet, "fish_down")
fmsy_mod <- simulate_model(flat_msy_fleet, "flat_msy")

if (!interactive()) {
  ggsave(file = paste_path(om_dir, "setup", "figs", "twt_mod.pdf"), twt_mod)
  ggsave(file = paste_path(om_dir, "setup", "figs", "fd_mod.pdf"), fd_mod)
  ggsave(file = paste_path(om_dir, "setup", "figs", "fmsy_mod.pdf"), fmsy_mod)
} else {
  twt_stock <- get_stock_std(path = paste_path(om_dir, "two_way_trip"))
  fd_stock <- get_stock_std(path = paste_path(om_dir, "fish_down"))
  fmsy_stock <- get_stock_std(path = paste_path(om_dir, "flat_msy"))
}
