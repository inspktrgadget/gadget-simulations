# basic script to setup a Gadget operating model
library(gadgetSim)
library(tidyverse)

paste_path <- function(...) {
    paste(list(...), collapse = "/")
}
om_dir <- "~/gadget/models/simulations/op_mods/cod"

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
stock_m <- rep(0.2, length(minage:maxage))

# setup initial conditions
init_sd <- c(seq(2, 10, length.out = 5), rep(10, 50))
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
                    alpha = alpha,
                    beta = beta)
stock_initcond <- list(normalparamfile = init_data)

# setup spawning
bh_mu <- 4e8
bh_lambda <- 1.067e08
mat_alpha <- 0.2
mat_l50 <- 64
stock_spawnfile <-
    make_gadget_spawnfile(
        stockname = stockname,
        start_year = st_year,
        end_year = end_year,
        proportionfunction = c("exponential", -mat_alpha, mat_l50),
        recruitment = bev_holt_formula(stockname, params = c(bh_mu, bh_lambda)),
        stockparameters = c(vb(linf, k, t0, minage), 1, alpha, beta)
)

# create gadget stockfile
cod <-
    make_gadget_stockfile(stock = stock_info,
                          refweightfile = stock_refwgt,
                          growth = stock_growth,
                          naturalmortality = stock_m,
                          iseaten = 1,
                          initialconditions = stock_initcond,
                          spawning = stock_spawnfile)

#-------------------------------
# setup the fleets
exp_sel_params <- list(alpha = 0.1, l50 = 60)
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
                          stock = cod, 
                          fleet = fleet)
    write_gadget_model(mod, path = path)
    stock_std <- get_stock_std(path = path)
    ts_plot <- 
        ggplot(data=filter(stock_std$cod, step == 1, year >= 20), 
               aes(x = year, y = number)) + 
        geom_line() + facet_wrap(~age, scales = "free_y")
    return(ts_plot)
}
twt_mod <- simulate_model(twt_fleet, "two_way_trip")
fd_mod <- simulate_model(fish_down_fleet, "fish_down")
fmsy_mod <- simulate_model(flat_msy_fleet, "flat_msy")
