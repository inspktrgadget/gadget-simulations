library(tidyverse)

# some function definitions
paste_path <- function(...) {
  paste(list(...), collapse = "/")
}
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

# read in parameter estimates and depletion data
path <- "~/gadget/models/simulations/"

params <- 
  read.csv(paste_path(path, "em_params.csv"), header = TRUE,
           stringsAsFactors = FALSE) %>%
  filter(rep > 0)
depletion <- read.csv(paste_path(path, "depletion.csv"), header = TRUE,
                      stringsAsFactors = FALSE)

# calculate re and mare
params_re <- 
  params %>%
  mutate(re = ((value - true_value) / true_value))

params_mare <- 
  params_re %>%
  group_by(model, spp, scenario, switch) %>%
  summarize(mare = median(abs(re)))

depletion_re <- 
  depletion %>%
  mutate(depletion_re = ((depletion - true_depletion) / true_depletion),
         terminal_bm_re = ((terminal_bm - true_terminal_bm) / 
                             true_terminal_bm))

depletion_mare <- 
  depletion_re %>%
  group_by(model, spp, scenario) %>%
  summarize(depletion_mare = median(abs(depletion_re)),
            terminal_bm_mare = median(abs(terminal_bm_re)))

# create figures of re and mare for parameters
mod_levels <- c("model_paths")
mod_labels <- c("shortnames of models")
param_re_plot <- function(param) {
  data <- 
    params_re %>%
    mutate(spp = vapply(spp, .simpleCap, character(1)),
           scenario = vapply(gsub("_", " ", scenario), 
                             .simpleCap, character(1)),
           model = factor(model, levels = mod_levels, labels = mod_labels))
  g <- 
    ggplot(data=filter(data, switch == param), 
           aes(x=model, y=re, color = factor(model))) + 
    geom_boxplot(outlier.alpha=0.2) + 
    geom_jitter(width=0.3, alpha=0.2) +
    facet_wrap(spp ~ scenario, scales = "free_y") + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    ggtitle(.simpleCap(gsub("_", " ", param))) +
    labs(color = "Scenario") + 
    xlab("Model") + ylab("Relative Error (%)")
  return(g)
}

params2plot <- 
  c("linf", "k", "t0", "bh_mu", "bh_lambda", "mat_alpha", "mat_l50")

linf_plot <- param_re_plot("linf")
k_plot <- param_re_plot("k")
t0_plot <- param_re_plot("t0")
bh_mu_plot <- param_re_plot("bh_mu")
bh_lambda_plot <- param_re_plot("bh_lambda")
mat_alpha_plot <- param_re_plot("mat_alpha")
mat_l50_plot <- param_re_plot("mat_l50")

# create figures of depletion and terminal ssb
dep_re_plot <- function(value) {
  data <- 
    depletion_re %>%
    mutate(spp = vapply(spp, .simpleCap, character(1)),
           scenario = vapply(gsub("_", " ", scenario), 
                             .simpleCap, character(1)),
           model = factor(model, levels = mod_levels, labels = mod_labels))
  g <- 
    ggplot(data=data, 
           aes(x=model, y=eval(as.symbol(paste(value, "re", sep = "_"))),
               color = factor(model))) + 
    geom_boxplot(outlier.alpha=0.2) +
    geom_jitter(width=0.3, alpha = 0.2) +
    facet_wrap(spp ~ scenario, scales = "free_y") + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    ggtitle(.simpleCap(gsub("_", " ", value))) +
    labs(color = "Scenario") + 
    xlab("Model") + ylab("Relative Error (%)")
  return(g)
}

depletion_plot <- dep_re_plot("depletion")
terminal_bm_plot <- dep_re_plot("terminal_bm")
