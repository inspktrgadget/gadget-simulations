# sample Gadget OMs for various proportions of age and length data
library(methods)
library(gadgetSim)
library(tidyverse)

# setup paths to OMs and EM dir
paste_path <- function(...) {
    return(paste(list(...), collapse = "/"))
}

# define the operating model and estimation model directories
om_dir <- "~/gadget/models/simulations/op_mods"
em_dir <- "~/gadget/models/simulations/comp_lik"
check_dir_exists(em_dir)
cod_om_dir <- paste_path(om_dir, "cod")
flatfish_om_dir <- paste_path(om_dir, "flatfish")
capelin_om_dir <- paste_path(om_dir, "capelin")

# define fishing scenarios
f_scenarios <- c("two_way_trip", "fish_down", "flat_msy")

# retrieve the OM outputs
retrieve_stock_stds <- function(om_path) {
    tmp <-
        lapply(f_scenarios, function(x) {
            return(get_stock_std(path = paste_path(om_path, x)))
        }) %>%
        setNames(f_scenarios)
    return(tmp)
}
cod_stock_stds <- retrieve_stock_stds(cod_om_dir)
flatfish_stock_stds <- retrieve_stock_stds(flatfish_om_dir)
capelin_stock_stds <- retrieve_stock_stds(capelin_om_dir)

# define length groups and error for sampling
cod_lengrps <- seq(0.5, 150.5, by = 1)
flatfish_lengrps <- seq(0.5, 100.5, by = 1)
capelin_lengrps <- seq(0.5, 25.5, by = 1)

# source in the survey fleets for each life-history type
source(paste_path(em_dir, "setup", "setup_surveys.R"))

# create scenarios
data_scenario <- c("dr", "dp")
likelihoods <- c("ss", "mltnom",
                 "mvn", "mvlog")
scenarios <- sprintf("%s_%s", sort(rep(data_scenario, 4)), likelihoods)


# sample the OM outputs at each level and write to respective
# directories and sub-directories
sample_reps <- 3
ncores <- 5

# here for temporary use while setting up
x <- 1
scenario <- scenarios[x]
scenario_dir <- paste_path(em_dir, scenario)
stock_std <- capelin_stock_stds
stockname <- "capelin"
om_dir <- capelin_om_dir
fishing_scenario <- names(capelin_stock_stds)
survey_fleets <- capelin_survey_fleets
lengrps <- capelin_lengrps
selectivity <- capelin_survey_selectivity
survey_error <- catch_error <- 0.2

# define function to feed into lapply which makes dirs, sets em up,
# samples gadget, and creates likelihood n number of times
set_em_up <- function(x, stock_std, stockname, om_dir, scenario,
                      fishing_scenario, survey_fleets, lengrps, selectivity,
                      survey_error, catch_error) {
    fishing_scenario <- fishing_scenario[x]
    scenario_dir <- paste_path(em_dir, scenario)
    check_dir_exists(paste_path(scenario_dir, stockname, fishing_scenario),
                     recursive = TRUE)
    lik_type <- strsplit(scenario, split = "_")[[1]][2]
    
    #------------------------------------------
    # read in and write out the operating model
    op_mod <-
        read_gadget_model(path = paste_path(om_dir,
                                            fishing_scenario))
    # get the stock std from the operating model
    stock_data <- stock_std[[x]][[stockname]]

    # obtain model values to guide parameter bounds later
    source(paste_path(om_dir, "setup", "om_params.R"),
           local = TRUE)
    source(paste_path(em_dir, "setup", "survey_params.R"),
           local = TRUE)
    source(paste_path(em_dir, "setup", "setup_params.R"),
           local = TRUE)

    # add survey fleets
    op_mod <- add_survey_fleet(op_mod, survey_fleets)

    # retrieve some basic information from op models to setup em
    gr_params <-
        c(make_switches(paste(stockname, "linf", sep = "."),
                        paste(stockname, "k", sep = ".")),
          lw_alpha, lw_beta)
    ages <- get_stock_ages(op_mod$stocks)
    init_cond <-
        normalparamfile(age = ages,
                        area = 1,
                        age.factor =
                            make_switches(paste(stockname,
                                                "init.age.mult",
                                                sep = ".")),
                        area.factor =
                            make_switches(paste(stockname,
                                                "init.area.mult",
                                                sep = ".")),
                        mean = vb_formula(stockname, ages),
                        stddev = init_sd[1:length(ages)],
                        alpha = lw_alpha,
                        beta = lw_beta)

    # read in original gadget spawnfile
    base_om_dir <- paste_path("~/gadget/models/simulations/op_mods",
                              stockname, "two_way_trip")
    spawning <- gadgetSim:::read_gadget_spawnfile(paste_path("Modelfiles",
                                                 paste(stockname,
                                                       "spawnfile", sep = ".")),
                                      path = base_om_dir)

    # get landings from operating model to use in estimation model fleet
    landings <-
        stock_data %>%
        group_by(year, step, area) %>%
        summarize(landed_biomass = sum(biomass.consumed)) %>%
        mutate(fleetname = "comm") %>%
        select(year, step, area, fleetname, landed_biomass) %>%
        filter(landed_biomass > 0)
    comm_fleet <-
        list(type = "totalfleet",
             suitability = make_exponentiall50_suit("comm", stockname),
             amount = landings)

    # update operating model to produce estimation model
    est_mod <-
        op_mod %>%
        update_model("stocks",
                     item = "growth",
                     growthparameters = gr_params,
                     beta = sprintf("(* #%1$s.bbin #%1$s.bbin.mult)",
                                    stockname)) %>%
        update_model("stocks",
                     item = "initialconditions",
                     normalparamfile = init_cond) %>%
        # update_model("stocks",
        #              doesspawn = 1) %>%
        update_model("stocks",
                     item = "spawning",
                     proportionfunction = c("exponential",
                                            make_switches(
                                                sprintf("%s.mat.alpha",
                                                        stockname)
                                            ),
                                            make_switches(
                                                sprintf("%s.mat.l50",
                                                        stockname)
                                            )),
                     recruitment = bev_holt_formula(stockname),
                     stockparameters = c(vb_formula(stockname, minage),
                                         minage_sd,
                                         gr_params[3:4])) %>%
        update_model("stocks",
                    doesrenew = 0) %>%
        # swap out the linearfleet for a totalfleet which uses landings data
        add_fleet(make_gadget_fleet(comm = comm_fleet)) %>%
        rm_fleet("lin")

    # use if op_mod has variable recruitment
    # est_mod$stocks[[grep("minlength",
    #                      names(est_mod$stocks),
    #                      fixed = TRUE)[3]]] <- NULL
    # est_mod$stocks[[grep("maxlength",
    #                      names(est_mod$stocks),
    #                      fixed = TRUE)[4]]] <- NULL
    # est_mod$stocks[[grep("dl", names(est_mod$stocks),
    #                      fixed = TRUE)[3]]] <- NULL
    # est_mod$stocks[[grep("numberfile", names(est_mod$stocks),
    #                 fixed = TRUE)]] <- NULL
    est_mod$stocks$doesstray <- NULL

    est_mod <- update_model(est_mod, "stocks", doesstray = 0)

    path <- paste_path(scenario_dir, stockname, fishing_scenario)
    check_dir_exists(path, recursive = TRUE)
    write_gadget_model(est_mod,
                       path = path)

    # write out params to estimation file
    write_gadget_file(params, path = path)

    #-----------------------------------------------
    # get samples from op_model and add lengthgroups

    # adding intra-haul correlation to samples
    # this works by creating 100 bins, defining schooling structure within
    # each bin (by length), and then sampling a single school in each bin
    # first define the bins and the schooling structure
    n_bins <- 100
    n_schools <- 10
    len_mns <-
        vapply(2:length(lengrps), function(x) {
            mean(c(lengrps[x], lengrps[x-1]))
        }, numeric(1))
    school_defs <- cut(len_mns, n_schools)
    # first define bins and split data to them
    make_schools <- function(variable = "number", survey_st_year, error) {
        bins <-
            stock_data %>%
            filter(year >= survey_st_year) %>%
            survey_gadget(length_groups = lengrps,
                          survey_suitability = selectivity,
                          survey_sigma = error,
                          variable = variable) %>%
            filter(number > 0) %>%
            replicate(n_bins, ., simplify = FALSE)
        schools <-
            lapply(seq_along(bins), function(x) {
                tmp <-
                    bins[[x]] %>%
                    mutate(bin = x)
                tmp$number <- round(tmp$number / n_bins)
                return(tmp)
            }) %>%
            do.call("rbind", .) %>%
            filter(number > 0) %>%
            group_by(year, step, area, bin) %>%
            mutate(school = as.numeric(school_defs[length])) %>%
            ungroup()
        return(schools)
    }
    school_survey <- make_schools(survey_st_year = survey_st_year,
                                  error = survey_error)
    school_comm <- make_schools(variable = "number.consumed",
                                survey_st_year = comm_survey_st_year,
                                error = catch_error)

    # define length samples based on data rich/data poor scenario
    is_dr <- grepl("dr", strsplit(scenario, split = "_")[[1]][1])
    length_samples <- ifelse(is_dr, 1e4, 2e3)
    age_samples <- ifelse(is_dr, 1e3, 100)

    # get the likelihood type for composition data
    lik_type <-
        switch(lik_type,
               ss = "sumofsquares",
               mltnom = "multinomial",
               mvn = "mvn",
               mvlog = "mvlogistic")

    samples <-
        lapply(1:sample_reps, function(y) {
            #-------------------------------
            # survey length_group added data
            spr_survey_samples <-
                school_survey %>%
                filter(step == 1) %>%
                group_by(year, step, area, bin) %>%
                filter(school == sample(unique(as.numeric(school_defs)), 1)) %>%
                group_by(year, step, area, age, length) %>%
                summarize(number = sum(number))
            aut_survey_samples <-
                school_survey %>%
                filter(step == 3) %>%
                group_by(year, step, area, bin) %>%
                filter(school == sample(unique(as.numeric(school_defs)), 1)) %>%
                group_by(year, step, area, age, length) %>%
                summarize(number = sum(number))
            catch_samples <-
                school_comm %>%
                group_by(year, step, area, bin) %>%
                filter(school == sample(unique(as.numeric(school_defs)), 1)) %>%
                group_by(year, step, area, age, length) %>%
                summarize(number = sum(number))
            spr_comp_data <-
                spr_survey_samples %>%
                strip_age_length_data(length_samples = length_samples,
                                      age_samples = age_samples)
            aut_comp_data <-
                aut_survey_samples %>%
                strip_age_length_data(length_samples = length_samples,
                                      age_samples = age_samples)
            catch_comp_data <-
                catch_samples %>%
                strip_age_length_data(length_samples = length_samples,
                                      age_samples = age_samples)
            stock_ages <- op_mod$stocks$minage:op_mod$stocks$maxage
            rep <- sprintf("reps/rep%s", y)
            sub_dir <- gadget_sub_dir(rep, path = path)
            source(paste_path(em_dir, "setup", "setup_likelihood.R"),
                   local = TRUE)
            likelihood <-
                make_gadget_likelihood(spr_ldist,
                                       aut_ldist,
                                       comm_ldist,
                                       spr_aldist,
                                       aut_aldist,
                                       comm_aldist,
                                       spr_si1,
                                       spr_si2,
                                       spr_si3,
                                       spr_si4,
                                       spr_si5,
                                       aut_si1,
                                       aut_si2,
                                       aut_si3,
                                       aut_si4,
                                       aut_si5)
            rep_main <-
                modifyList(op_mod$main,
                           list(likelihoodfiles =
                                    paste_path(rep, "likelihood")))
            write_gadget_file(rep_main, path = sub_dir)
            write_gadget_file(likelihood, path = sub_dir)
        })
    optinfofile <- make_gadget_optinfofile(
        simann = list(t=3e06),
        hooke = "default",
        bfgs = "default"
    )
    write_gadget_file(make_gadget_optinfofile(),
                      path = path)
    file.copy(paste_path(em_dir, "setup", "run.R"), path)
}



for (i in scenarios) {
        #-----------------------------------------
        # create a directory for the current level
        level_dir <- paste_path(em_dir, i)

        system.time({
        cod_models <-
            lapply(seq_along(cod_stock_stds), set_em_up,
                   cod_stock_stds, stockname = "cod", om_dir = cod_om_dir,
                   scenario = i,
                   fishing_scenario = names(cod_stock_stds),
                   survey_fleets = cod_survey_fleets,
                   lengrps = cod_lengrps,
                   selectivity = cod_survey_selectivity,
                   survey_error = survey_error, catch_error = catch_error)
        flatfish_models <-
            lapply(seq_along(flatfish_stock_stds), set_em_up,
                   flatfish_stock_stds, stockname = "flatfish",
				   om_dir = flatfish_om_dir,
				   scenario = i,
                   fishing_scenario = names(flatfish_stock_stds),
                   survey_fleets = flatfish_survey_fleets,
                   lengrps = flatfish_lengrps,
                   selectivity = flatfish_survey_selectivity,
                   survey_error = survey_error, catch_error = catch_error)
        capelin_models <-
            lapply(seq_along(capelin_stock_stds), set_em_up,
                   capelin_stock_stds, stockname = "capelin",
				   om_dir = capelin_om_dir,
				   scenario = i,
                   fishing_scenario = names(capelin_stock_stds),
                   survey_fleets = capelin_survey_fleets,
                   lengrps = capelin_lengrps,
                   selectivity = capelin_survey_selectivity,
                   survey_error = survey_error, catch_error = catch_error)
        })
}
