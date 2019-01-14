library(tidyverse)


load("~/gadget/simulations/age_len_prop/results/model_output.RData")
models <- unique(mod_list$model)

# plot total numbers
# rep == 0 is the om
year_filter <- 0
numbers_plot <- 
  lapply(models, function(x) {
    g <- 
      ggplot(data=filter(mod_list, model == x, step == 1, 
                         year > year_filter, rep > 0), 
             aes(x=year, y=number/1e6, group = factor(rep))) + 
      geom_line() + 
      geom_line(data=filter(mod_list, step == 1, rep == 0), 
                aes(x=year, y=number/1e6),
                color = "red") + 
      facet_wrap(spp ~ scenario, scales = "free_y") + 
      xlab("Year") + ylab("Numbers (in millions)") + 
      ggtitle(paste(x, " -- Numbers"))
    return(g)
  })
  

# plot total biomass
bm_plot <- 
  lapply(models, function(x) {
    g <- 
      ggplot(data=filter(mod_list, model == x, step == 1, 
                         year > year_filter, rep > 0), 
             aes(x=year, y=biomass/1e6, group = factor(rep))) + 
      geom_line() + 
      geom_line(data=filter(mod_list, step == 1, rep == 0), 
                aes(x=year, y=biomass/1e6),
                color = "red") + 
      facet_wrap(spp ~ scenario, scales = "free_y") + 
      xlab("Year") + ylab("Biomass (thousand tons)") +
      ggtitle(paste(x, "-- Biomass"))
  })

