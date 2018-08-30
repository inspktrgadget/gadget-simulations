# script to setup stockfile for each different scenario

#-------------------------------------------------------------------------------
# setup stock for each different scenario

#-------------------------------------------------------------------------------
# two_way_trip

base_om_dir <- "~/gadget/models/simulations/op_mods/cod"
make_scenario_stock <- function(scenario) {
    # get stock std from basic operating model to get ballpark for recruitment
    stock_std <- 
        get_stock_std(path = paste_path(base_om_dir, scenario))
    stock_rec <- 
        stock_std[[1]] %>%
        filter(step == 1, age == minage) %>%
        mutate(number = number * exp(rnorm(n(), 0, 0.02)))
    
    # setup recruitment
    stock_renewal <- 
        numberfile(year = stock_rec$year,
                   step = 1,
                   area = 1,
                   age = minage,
                   length = min(stock_rec$length) - 
                       (3 * stock_rec$length.sd[which.min(stock_rec$length)]),
                   number = stock_rec$number,
                   weight = lw(alpha, beta, vb(linf, k, t0, minage)))
    
    # create gadget stockfile
    stock <-
        make_gadget_stockfile(stock = stock_info,
                              refweightfile = stock_refwgt,
                              growth = stock_growth,
                              naturalmortality = stock_m,
                              iseaten = 1,
                              initialconditions = stock_initcond,
                              renewal = stock_renewal)
    return(stock)
}

# make stock for each scenario
cod_twt_stock <- make_scenario_stock("two_way_trip")
cod_fd_stock <- make_scenario_stock("fish_down")
cod_fmsy_stock <- make_scenario_stock("flat_msy")
