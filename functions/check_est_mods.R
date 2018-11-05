# these are functions to help with checking to make sure estimation models
# are set up correctly and don't crash
# there are also functions here to help with analyzing which parameters lead
# to a crash in the estimation models

paste_path <- function(...) {
    return(paste(list(...), collapse = "/"))
}

get_timeseries <- function(est_mod_path, scenarios, spp, fishing_scenario) {
    stds <- 
        lapply(scenarios, function(x) {
            lapply(spp, function(y) {
                lapply(fishing_scenario, function(z) {
                    path <- paste_path(est_mod_path, x, y, z)
                    stock_std <- get_stock_std(main = "main", 
                                               params_file = "params.in",
                                               path = path)
                    test <- 
                        stock_std[[1]] %>% 
                        filter(step == 1) %>%
                        group_by(year) %>% 
                        summarize(total = sum(number)) %>%
                        mutate(model = x, spp = y, fish_scenario = z)
                    return(test)
                }) %>%
                    do.call("rbind", .)
            }) %>%
                do.call("rbind", .)
        })
    return(stds)
}

make_timeseries_plots <- function(stds) {
    pop_timeseries <- 
        lapply(stds, function(x) {
            model <- unique(x$model)
            g <- 
                ggplot(data=x, aes(x=year, y=total/1e06)) + geom_line() + 
                facet_wrap(spp ~ fish_scenario, scales = "free_y") + 
                xlab("Year") + ylab("Numbers (millions)") + 
                ggtitle(label = model)
            return(g)
        })
    return(pop_timeseries)
}

get_depletion <- function(stds) {
    depletion <- 
        lapply(stds, function(x) {
            dep <- 
                x %>%
                group_by(model, spp, fish_scenario) %>%
                summarize(max = max(total), 
                          min = min(total), 
                          end = total[n()]) %>%
                group_by(model, spp, fish_scenario) %>%
                summarize(min_dep = min / max, 
                          end_dep = end / max)
        }) %>%
        do.call("rbind", .)
    return(depletion)
}

get_params <- function(est_mod_path, scenarios, spp, fishing_scenario) {
    parms2keep <- c("linf", "k", "t0", "bh.mu", "bh.lambda", "mat.alpha", 
                    "mat.l50", "comm.alpha", "comm.beta", "comm.gamma")
    params <- 
        lapply(scenarios, function(x) {
            lapply(spp, function(y) {
                lapply(fishing_scenario, function(z) {
                    path <- paste_path(est_mod_path, x, y, z)
                    prms <- read.table(paste(path, "params.in", sep = "/"), 
                                       comment.char = ";", header = TRUE,
                                       stringsAsFactors = FALSE)
                    names(prms) <- 
                        c("switch", "value", "lower", "upper", "optimize")
                    prms <- 
                        prms %>%
                        mutate(switch = gsub(paste0(y, "."), "", switch)) %>%
                        filter(switch %in% parms2keep) %>%
                        mutate(model = x, spp = y, fish_scenario = z)
                    return(prms)
                }) %>%
                    do.call("rbind", .)
            }) %>%
                do.call("rbind", .)
        }) %>%
        do.call("rbind", .)
}

get_om_params <- function(om_mod_path, spp) {
    om_parms <- 
        lapply(spp, function(x) {
            source(paste_path(om_mod_path, x, "setup/om_params.R"),
                   local = TRUE)
            tmp <- 
                lapply(ls(), function(y) {
                    parms2keep <- c("linf", "k", "t0", "bh.mu", "bh.lambda", 
                                    "mat.alpha", "mat.l50", 
                                    "comm.alpha", "comm.beta", "comm.gamma")
                    if (gsub("_", ".", y) %in% parms2keep) {
                        return(data.frame(switch = gsub("_", ".", y), 
                                          true_value = get(y),
                                          stringsAsFactors = FALSE))
                    } else {
                        return(NULL)
                    }
                }) %>%
                do.call("rbind", .) %>%
                mutate(spp = x)
        }) %>%
        do.call("rbind", .)
    return(om_parms)
}

# the below functions are for tweaking and checking a single model
# this will be of use if you have initial paramater combinations that are 
# reluctant to work (i.e. the population crashes)
plot_single_mod <- function(est_mod_path, spp, fish_scenario) {
    tmp <- 
        get_stock_std(main = "main", params = "params.in",
                      path = paste_path(est_mod_path, spp, fish_scenario)) %>%
        `[[`(1) %>%
        filter(step == 1) %>%
        group_by(year, step, area) %>%
        summarize(total = sum(number))
    est_mod <- strsplit(est_mod_path, split = "/")[[1]]
    model <- est_mod[length(est_mod)]
    return(
        ggplot(data = tmp, aes(x=year, y=total/1e6)) + geom_line() +
        xlab("Year") + ylab("Number (millions") + 
        ggtitle(label = model))
}

get_single_param <- function(est_mod_path, spp, fish_scenario, param) {
    params <- read.table(params_path, header = TRUE, comment.char = ";",
                         stringsAsFactors = FALSE)
    return(params$value[grep(param, params$switch)])
}

tweak_param <- function(est_mod_path, spp, fish_scenario, 
                        param2tweak, value) {
    params_path <- paste_path(est_mod_path, spp, fish_scenario, "params.in")
    params <- read.table(params_path, header = TRUE, comment.char = ";",
                         stringsAsFactors = FALSE)
    params$value[grep(param2tweak, params$switch)] <- value
    upper_bound <- params$upper[grep(param2tweak, params$switch)]
    lower_bound <- params$lower[grep(param2tweak, params$switch)]
    if (value < lower_bound | value > upper_bound) {
        stop("Specified value is not within bounds. Please revise")
    }
    params <- structure(params, class = c("gadget_params", "data.frame"))
    write_gadget_file(params, file = params_path)
}

tweak_mod <- function(est_mod_path, spp, fish_scenario,
                      param2tweak, value) {
    tweak_param(est_mod_path, spp, fish_scenario, 
                param2tweak, value)
    plot_single_mod(est_mod_path, spp, fish_scenario)
}