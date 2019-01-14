library(methods)
library(parallel)
library(Rgadget)

paste_path <- function(...) {
    paste(list(...), collapse = "/")
}

base_dir <- "~/gadget/simulations/age_len_prop/model/spp/scenario"

setwd(base_dir)
tmp <- system.time(mclapply(1:100, function(x) {
    tmp <- system.time(
      gadget.iterative(rew.sI = TRUE,
                       main = sprintf("reps/rep%s/main", x),
                       grouping = list(len1 = c("spr.si.1", "aut.si.1"),
                                       len2 = c("spr.si.2", "aut.si.2"),
                                       len3 = c("spr.si.3", "aut.si.3"),
                                       len4 = c("spr.si.4", "aut.si.4"),
                                       len5 = c("spr.si.5", "aut.si.5")),
                       wgts = sprintf("reps/rep%s/WGTS", x),
                       run.serial = TRUE))[[3]]
    hrs <- tmp %/% 3600
    mins <- (tmp - (hrs * 3600)) %/% 60
    cat("Model", x, "took", hrs, "hours and", mins, "minutes to run.", "\n",
        file = "runtime", append = TRUE)
}, mc.cores = 10))[[3]]


hrs <- tmp %/% 3600
mins <- (tmp - (hrs * 3600)) %/% 60
cat("Total runtime for all replicates was", hrs, "hours and", mins, "minutes.",
    file = "runtime", append = TRUE)
