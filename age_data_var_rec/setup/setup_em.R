# sample Gadget OMs for various proportions of age and length data
library(methods)
library(gadgetSim)
library(mfdb)
library(tidyverse)

# setup paths to OMs and EM dir
paste_path <- function(...) {
    return(paste(list(...), collapse = "/"))
}

# define the operating model and estimation model directories
om_dir <- "~/gadget/models/simulations/op_mods/var_rec"
em_dir <- "~/gadget/models/simulations/age_data_var_rec"
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

# define the length and age proportions to be sampled
age_prop <- c(1, 0.5, 0.2, 0.1, 0.01)
age_samples <- c(1000, 500, 200, 100, 50)

# define length groups and error for sampling
cod_lengrps <- seq(0.5, 150.5, by = 1)
flatfish_lengrps <- seq(0.5, 100.5, by = 1)
capelin_lengrps <- seq(0.5, 25.5, by = 1)

# source in the survey fleets for each life-history type
source(paste_path(em_dir, "setup", "setup_surveys.R"))

# sample the OM outputs at each level and write to respective
# directories and sub-directories
sample_reps <- 10
ncores <- 5

i <- 1000

# here for temporary use while setting up
level_dir <- paste_path(em_dir, paste("age_samples", i, sep = "_"))
x <- 1
stock_std <- cod_stock_stds
stockname <- "cod"
om_dir <- cod_om_dir
fishing_scenario <- names(cod_stock_stds)
survey_fleets <- cod_survey_fleets
lengrps <- cod_lengrps
selectivity <- cod_survey_selectivity
survey_error <- catch_error <- 0

# define function to feed into lapply which makes dirs, sets em up,
# samples gadget, and creates likelihood n number of times
set_em_up <- function(x, stock_std, stockname, om_dir, fishing_scenario,
                      survey_fleets, lengrps, selectivity,
                      survey_error, catch_error) {
    fishing_scenario <- fishing_scenario[x]
    check_dir_exists(paste_path(level_dir, stockname, fishing_scenario),
                     recursive = TRUE)
    #------------------------------------------
    # read in and write out the operating model
    op_mod <-
        read_gadget_model(path = paste_path(om_dir,
                                            fishing_scenario))
    
    # obtain model values to guide parameter bounds later
    gr_params <- op_mod$stocks$growthparameters
    source(paste_path(em_dir, "setup", "setup_params.R"),
           local = TRUE)
    
    # add survey fleets
    op_mod <- add_survey_fleet(op_mod, survey_fleets)
    
    # retrieve some basic information from op models to setup em
    gr_params[1:2] <- make_switches(paste(stockname, "linf", sep = "."),
                                    paste(stockname, "k", sep = "."))
    ages <- get_stock_ages(op_mod$stocks)
    init_sd <- c(1:10, rep(10, 40))
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
                        mean = vb_formula("cod", ages),
                        stddev = init_sd[1:length(ages)],
                        alpha = gr_params[3],
                        beta = gr_params[4])
    
    # read in original gadget spawnfile
    base_om_dir <- paste_path("~/gadget/models/simulations/op_mods",
                              stockname, "two_way_trip")
    spawning <- gadgetSim:::read_gadget_spawnfile(paste_path("Modelfiles",
                                                 paste(stockname,
                                                       "spawnfile", sep = ".")),
                                      path = base_om_dir)
    
    # update operating model to produce estimation model
    est_mod <-
        op_mod %>%
        update_model("stocks",
                     growthparameters = gr_params,
                     beta = sprintf("(* #%1$s.bbin #%1$s.bbin.mult)", 
                                    stockname)) %>%
        update_model("stocks",
                     normalparamfile = init_cond) %>%
        update_model("stocks",
                     doesspawn = 1,
                     spawnfile = spawning) %>%
        update_model("stocks",
                     item = "spawnfile",
                     recruitment = bev_holt_formula(stockname),
                     stockparameters = c(vb_formula(stockname, 1), 2,
                                         gr_params[3:4])) %>%
        update_model("stocks",
                    doesrenew = 0) %>%
        update_fleet("lin",
                     suitability = make_exponentiall50_suit("lin",
                                                            stockname))
        
    est_mod$stocks[[grep("minlength", 
                         names(est_mod$stocks), 
                         fixed = TRUE)[3]]] <- NULL
    est_mod$stocks[[grep("maxlength", 
                         names(est_mod$stocks),
                         fixed = TRUE)[4]]] <- NULL
    est_mod$stocks[[grep("dl", names(est_mod$stocks),
                         fixed = TRUE)[3]]] <- NULL
    est_mod$stocks[[grep("numberfile", names(est_mod$stocks),
                    fixed = TRUE)]] <- NULL
    est_mod$stocks$doesstray <- NULL
    
    est_mod <- update_model(est_mod, "stocks", doesstray = 0)
    
    path <- paste_path(level_dir, stockname, fishing_scenario)
    check_dir_exists(path, recursive = TRUE)
    write_gadget_model(est_mod,
                       path = path)
    
    # write out params to estimation file
    write_gadget_file(params, path = path)
    
    #--------------------------
    # get samples from op_model and add lengthgroups
    model_length_groups <-
        stock_std[[x]][[stockname]] %>%
        filter(year >= survey_st_year) %>%
        add_lengthgroups(length_groups = lengrps)
    
    samples <-
        lapply(1:sample_reps, function(y) {
            #-------------------------------
            # survey length_group added data
            spr_survey_samples <-
                model_length_groups %>%
                filter(step == 1) %>%
                survey_gadget(length_groups = lengrps,
                              survey_suitability = selectivity,
                              survey_sigma = survey_error) %>%
                filter(number > 0)
            aut_survey_samples <-
                model_length_groups %>%
                filter(step == 3) %>%
                survey_gadget(length_groups = lengrps,
                              survey_suitability = selectivity,
                              survey_sigma = survey_error) %>%
                filter(number > 0)
            catch_samples <-
                model_length_groups %>%
                survey_gadget(length_groups = lengrps,
                              survey_suitability = rep(0.001,
                                                       length(lengrps)),
                              survey_sigma = catch_error) %>%
                filter(number > 0)
            spr_comp_data <-
                spr_survey_samples %>%
                strip_age_length_data(length_samples = 0.01, 
                                      age_samples = i,
                                      quiet = TRUE)
            aut_comp_data <-
                aut_survey_samples %>%
                strip_age_length_data(length_samples = 0.01, 
                                      age_samples = i,
                                      quiet = TRUE)
            catch_comp_data <-
                catch_samples %>%
                strip_age_length_data(length_samples = 0.01, 
                                      age_samples = i,
                                      quiet = TRUE)
            stock_ages <- op_mod$stocks$minage:op_mod$stocks$maxage
            rep <- sprintf("reps/rep%s", y)
            sub_dir <- gadget_sub_dir(rep, path = path)
            source(paste_path(em_dir, "setup",
                              sprintf("%s_likelihood_comps.R",
                                      stockname)),
                   local = TRUE)
            likelihood <-
                make_gadget_likelihood(spr_ldist,
                                       aut_ldist,
                                       lin_ldist,
                                       spr_aldist,
                                       aut_aldist,
                                       lin_aldist,
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
}



for (i in age_prop) {
        #-----------------------------------------
        # create a directory for the current level
        level_dir <- paste_path(em_dir, paste("age_prop", i, sep = "_"))

        system.time({
        cod_models <-
            lapply(seq_along(cod_stock_stds), set_em_up,
                   cod_stock_stds, stockname = "cod", om_dir = cod_om_dir,
                   fishing_scenario = names(cod_stock_stds),
                   survey_fleets = cod_survey_fleets,
                   lengrps = cod_lengrps,
                   selectivity = cod_survey_selectivity,
                   survey_error = survey_error, catch_error = catch_error)
        flatfish_models <-
            lapply(seq_along(flatfish_stock_stds), set_em_up,
                   flatfish_stock_stds, stockname = "flatfish",
				   om_dir = flatfish_om_dir,
                   fishing_scenario = names(flatfish_stock_stds),
                   survey_fleets = flatfish_survey_fleets,
                   lengrps = flatfish_lengrps,
                   selectivity = flatfish_survey_selectivity,
                   survey_error = survey_error, catch_error = catch_error)
        capelin_models <-
            lapply(seq_along(capelin_stock_stds), set_em_up,
                   capelin_stock_stds, stockname = "capelin",
				   om_dir = capelin_om_dir,
                   fishing_scenario = names(capelin_stock_stds),
                   survey_fleets = capelin_survey_fleets,
                   lengrps = capelin_lengrps,
                   selectivity = capelin_survey_selectivity,
                   survey_error = survey_error, catch_error = catch_error)
        })
}

for (i in age_samples) {
    #-----------------------------------------
    # create a directory for the current level
    level_dir <- paste_path(em_dir, paste("age_samples", i, sep = "_"))
    
    system.time({
        cod_models <-
            lapply(seq_along(cod_stock_stds), set_em_up,
                   cod_stock_stds, stockname = "cod", om_dir = cod_om_dir,
                   fishing_scenario = names(cod_stock_stds),
                   survey_fleets = cod_survey_fleets,
                   lengrps = cod_lengrps,
                   selectivity = cod_survey_selectivity,
                   survey_error = survey_error, catch_error = catch_error)
        flatfish_models <-
            lapply(seq_along(flatfish_stock_stds), set_em_up,
                   flatfish_stock_stds, stockname = "flatfish",
                   om_dir = flatfish_om_dir,
                   fishing_scenario = names(flatfish_stock_stds),
                   survey_fleets = flatfish_survey_fleets,
                   lengrps = flatfish_lengrps,
                   selectivity = flatfish_survey_selectivity,
                   survey_error = survey_error, catch_error = catch_error)
        capelin_models <-
            lapply(seq_along(capelin_stock_stds), set_em_up,
                   capelin_stock_stds, stockname = "capelin",
                   om_dir = capelin_om_dir,
                   fishing_scenario = names(capelin_stock_stds),
                   survey_fleets = capelin_survey_fleets,
                   lengrps = capelin_lengrps,
                   selectivity = capelin_survey_selectivity,
                   survey_error = survey_error, catch_error = catch_error)
    })
}
