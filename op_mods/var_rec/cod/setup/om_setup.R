# basic script to setup a Gadget operating model
# this om is with variable recruitment
library(gadgetSim)
library(tidyverse)

paste_path <- function(...) {
    paste(list(...), collapse = "/")
}
om_dir <- "~/gadget/models/simulations/op_mods/var_rec/cod"

#------------------------------
# setup time and area
st_year <- 1
end_year <- 120
time <- make_gadget_timefile(st_year, end_year, "quarterly")
area <-
    make_gadget_areafile(areas = 1, size = 1e6,
                         temp_data = expand.grid(year = st_year:end_year, 
                                                 step = 1:4,
                                                 area = 1, temp = 3))

#------------------------------
# setup the stock
# setup basic stock information
stockname <- "cod"
minage <- 1
maxage <- 20
minlength <- 1
maxlength <- 150
dl <- 1
alpha <- 6.7e-06
beta <- 3.1
reflength <- seq(minlength, maxlength, dl)
stock_info <-
    list(stockname = stockname, livesonareas = 1, 
         minage = minage, maxage = maxage,
         minlength = minlength, maxlength = maxlength, dl = dl)

# setup refweightfile
stock_refwgt <-
    data.frame(len = reflength,
               weight = alpha * reflength ^ beta)

# setup growth
linf <- 125
k <- 0.15
t0 <- -0.35
stock_growth <-
    list(growthfunction = "lengthvbsimple",
         growthparameters = c(linf, k, alpha, beta),
		 beta = 30, 
		 maxlengthgroupgrowth = 15)

# setup naturalmortality
stock_m <- rep(0.2, (maxage - minage + 1))

# setup initial conditions
init_data <-
    normalparamfile(age = seq(minage, maxage, 1),
                    area = 1,
                    age.factor = 1e4 * 
                        exp(-cumsum(rep(0.2, (maxage - minage + 1)))),
                    area.factor = 1,
                    mean = vb_formula(stockname, minage:maxage,
                                      params = list(linf = linf, 
                                                    k = k, 
                                                    t0 = t0)),
                    sd = c(1:10, rep(10, 10)),
                    alpha = alpha,
                    beta = beta)
stock_initcond <- list(normalparamfile = init_data)

# source R script that makes separate stockfiles for each scenario
source(paste_path(om_dir, "setup", "setup_stocks.R"))


#-------------------------------
# setup the fleets
exp_sel_params <- list(alpha = 0.1, l50 = 75)
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
         suitability = make_exponentiall50_suit("lin", stockname, 
												params = exp_sel_params))

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
fish_down <- mutate(lin_flt_data, 
                    scaling = seq(0.01, f_high, 
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

simulate_model <- function(stock, fleet, scenario) {
    path <- paste_path(om_dir, scenario)
    mod <- 
        make_gadget_model(time = time, 
                          area = area, 
                          stock = stock, 
                          fleet = fleet)
    write_gadget_model(mod, path = path)
    stock_std <- get_stock_std(path = path)
    ts_plot <- 
        ggplot(data=filter(stock_std[[1]], step == 1, year >= 20), 
               aes(x = year, y = number)) + 
        geom_line() + facet_wrap(~age, scales = "free_y")
    return(ts_plot)
}

twt_mod <- simulate_model(cod_twt_stock, twt_fleet, "two_way_trip")
fd_mod <- simulate_model(cod_fd_stock, fish_down_fleet, "fish_down")
fmsy_mod <- simulate_model(cod_fmsy_stock, flat_msy_fleet, "flat_msy")
