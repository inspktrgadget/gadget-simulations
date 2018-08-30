# pull landings data from operating models
library(gadgetSim)
library(Rgadget)

paste_path <- function(...) {
    return(paste(list(...), collapse = "/"))
}

om_dir <- "~/gadget/models/simulations/op_mods"
op_mods <- c("cod", "flatfish", "capelin")
scenarios <- c("two_way_trip", "fish_down", "flat_msy")

catch_data <- 
    lapply(op_mods, function(x) {
        lapply(scenarios, function(y) {
            std <- get_stock_std(path = paste_path(om_dir, x, y))
            catch <- 
                std[[x]] %>%
                group_by(year, step, area) %>%
                summarize(total_weight = sum(biomass.consumed)) %>%
                mutate(fleet = "comm") %>%
                select(year, step, area, fleet, total_weight) %>%
                filter(total_weight > 0)
            header <- paste(paste(c(sprintf("; Data for operating model", x, y),
                                    "; -- data --",
                                      "; year step area fleetname total_weight"),
                                    collapse = "\n"))
            write(header, file = paste_path(om_dir, "landings", 
                                            paste(x, y, sep = "_")))
            write.table(catch, file = paste_path(om_dir, "landings", 
                                                 paste(x, y, sep = "_")), 
                        append = TRUE, row.names = FALSE, 
                        col.names = FALSE, quote = FALSE)
        })
    })