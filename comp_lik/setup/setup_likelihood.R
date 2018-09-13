# setup likelihood components

# define some parameters if comp_likelihood is multivariate

# lag for mvn
lik_lag <- NULL
lik_lag <- 
    if (lik_type == "mvn") {
        switch(stockname, cod = 10, flatfish = 10, capelin = 5)
    }

# sigma for mvn or mvlogistic
lik_sigma <- NULL
lik_sigma <- 
    if (lik_type %in% c("mvn", "mvlogistic")) {
        sprintf("#%s.%s.sigma", stockname, "%s")
    }

# parameters for mvn
lik_params <- NULL
lik_params <- 
    if (lik_type == "mvn") {
        as.list(c(seq(0.8, 0, length.out = ceiling(lik_lag / 2)),
                  rep(-0.1, times = floor(lik_lag / 2))))
    }

# setup catchdistribution components for composition data
spr_ldist <-
    make_gadget_catchdistribution(name = "spr.ldist",
                                  data = spr_comp_data$length_data,
                                  `function` = lik_type,
                                  lag = lik_lag,
                                  sigma = switch(lik_type,
                                                 sumofsquares = NULL,
                                                 multinomial = NULL,
                                                 sprintf(lik_sigma,
                                                         "spr.ldist")),
                                  param = lik_params,
                                  fleetnames = "spr",
                                  stocknames = stockname,
                                  ageagg = list(all = stock_ages))
aut_ldist <-
    make_gadget_catchdistribution(name = "aut.ldist",
                                  data = aut_comp_data$length_data,
                                  `function` = lik_type,
                                  lag = lik_lag,
                                  sigma = switch(lik_type,
                                                 sumofsquares = NULL,
                                                 multinomial = NULL,
                                                 sprintf(lik_sigma,
                                                         "aut.ldist")),                                  
                                  param = lik_params,
                                  fleetnames = "aut",
                                  stocknames = stockname,
                                  ageagg = list(all = stock_ages))

comm_ldist <-
    make_gadget_catchdistribution(name = "comm.ldist",
                                  data = catch_comp_data$length_data,
                                  `function` = lik_type,
                                  lag = lik_lag,
                                  sigma = switch(lik_type,
                                                 sumofsquares = NULL,
                                                 multinomial = NULL,
                                                 sprintf(lik_sigma,
                                                         "comm.ldist")),
                                  param = lik_params,
                                  fleetnames = "comm",
                                  stocknames = stockname,
                                  ageagg = list(all = stock_ages))

spr_aldist <-
    make_gadget_catchdistribution(name = "spr.aldist",
                                  data = spr_comp_data$age_data,
                                  `function` = lik_type,
                                  lag = lik_lag,
                                  sigma = switch(lik_type,
                                                 sumofsquares = NULL,
                                                 multinomial = NULL,
                                                 sprintf(lik_sigma,
                                                         "spr.aldist")),
                                  param = lik_params,
                                  fleetnames = "spr",
                                  stocknames = stockname)

aut_aldist <-
    make_gadget_catchdistribution(name = "aut.aldist",    
                                  data = aut_comp_data$age_data,
                                  `function` = lik_type,
                                  lag = lik_lag,
                                  sigma = switch(lik_type,
                                                sumofsquares = NULL,
                                                multinomial = NULL,
                                                sprintf(lik_sigma,
                                                        "aut.aldist")),
                                  param = lik_params,
                                  fleetnames = "aut",
                                  stocknames = stockname)

comm_aldist <-
    make_gadget_catchdistribution(name = "comm.aldist",
                                  data = catch_comp_data$age_data,
                                  `function` = lik_type,
                                  lag = lik_lag,
                                  sigma = switch(lik_type,
                                                 sumofsquares = NULL,
                                                 multinomial = NULL,
                                                 sprintf(lik_sigma,
                                                         "comm.aldist")),
                                  param = lik_params,
                                  fleetnames = "comm",
                                  stocknames = stockname)

# setup survey indices
len1 <- switch(stockname, cod = 30, flatfish = 20, capelin = 8)
len2 <- switch(stockname, cod = 45, flatfish = 30, capelin = 12)
len3 <- switch(stockname, cod = 60, flatfish = 40, capelin = 16)
len4 <- switch(stockname, cod = 75, flatfish = 50, capelin = 20)
maxlen <- switch(stockname, cod = 150, flatfish = 100, capelin = 25)

spr_si1 <-
    make_gadget_surveyindices(name = "spr.si.1",
                              data = get_si_data(spr_survey_samples, 
                                                 groups = 1:len1),
                              sitype = "lengths",
                              stocknames = stockname,
                              fittype = "fixedslopeloglinearfit",
                              slope = 1,
                              lenagg = make_si_aggfile("len1", 1, len1))
spr_si2 <-
    make_gadget_surveyindices(name = "spr.si.2",
                              data = get_si_data(spr_survey_samples,
                                                 groups = (len1 + 1):len2),
                              sitype = "lengths",
                              stocknames = stockname,
                              fittype = "fixedslopeloglinearfit",
                              slope = 1,
                              lenagg = make_si_aggfile("len2",
                                                       (len1 + 1),
                                                       len2))

spr_si3 <-
    make_gadget_surveyindices(name = "spr.si.3",
                              data = get_si_data(spr_survey_samples,
                                                 groups = (len2 + 1):len3),
                              sitype = "lengths",
                              stocknames = stockname,
                              fittype = "fixedslopeloglinearfit",
                              slope = 1,
                              lenagg = make_si_aggfile("len3",
                                                       (len2 + 1),
                                                       len3))

spr_si4 <-
    make_gadget_surveyindices(name = "spr.si.4",
                              data = get_si_data(spr_survey_samples,
                                                 groups = (len3 + 1):len4),
                              sitype = "lengths",
                              stocknames = stockname,
                              fittype = "fixedslopeloglinearfit",
                              slope = 1,
                              lenagg = make_si_aggfile("len4",
                                                       (len3 + 1),
                                                       len4))

spr_si5 <-
    make_gadget_surveyindices(name = "spr.si.5",
                              data = get_si_data(spr_survey_samples,
                                                 groups = (len4 + 1):maxlen),
                              sitype = "lengths",
                              stocknames = stockname,
                              fittype = "fixedslopeloglinearfit",
                              slope = 1,
                              lenagg = make_si_aggfile("len5",
                                                       (len4 + 1),
                                                       maxlen))


# setup autumn surveyindices
aut_si1 <-
    make_gadget_surveyindices(name = "aut.si.1",
                              data = get_si_data(aut_survey_samples,
                                                 groups = 1:len1),
                              sitype = "lengths",
                              stocknames = stockname,
                              fittype = "fixedslopeloglinearfit",
                              slope = 1,
                              lenagg = make_si_aggfile("len1", 1, len1))
aut_si2 <-
    make_gadget_surveyindices(name = "aut.si.2",
                              data = get_si_data(aut_survey_samples,
                                                 groups = (len1 + 1):len2),
                              sitype = "lengths",
                              stocknames = stockname,
                              fittype = "fixedslopeloglinearfit",
                              slope = 1,
                              lenagg = make_si_aggfile("len2",
                                                       (len1 + 1),
                                                       len2))

aut_si3 <-
    make_gadget_surveyindices(name = "aut.si.3",
                              data = get_si_data(aut_survey_samples,
                                                 groups = (len2 + 1):len3),
                              sitype = "lengths",
                              stocknames = stockname,
                              fittype = "fixedslopeloglinearfit",
                              slope = 1,
                              lenagg = make_si_aggfile("len3",
                                                       (len2 + 1),
                                                       len3))

aut_si4 <-
    make_gadget_surveyindices(name = "aut.si.4",
                              data = get_si_data(aut_survey_samples,
                                                 groups = (len3 + 1):len4),
                              sitype = "lengths",
                              stocknames = stockname,
                              fittype = "fixedslopeloglinearfit",
                              slope = 1,
                              lenagg = make_si_aggfile("len4",
                                                       (len3 + 1),
                                                       len4))

aut_si5 <-
    make_gadget_surveyindices(name = "aut.si.5",
                              data = get_si_data(aut_survey_samples,
                                                 groups = (len4 + 1):maxlen),
                              sitype = "lengths",
                              stocknames = stockname,
                              fittype = "fixedslopeloglinearfit",
                              slope = 1,
                              lenagg = make_si_aggfile("len5",
                                                       (len4 + 1),
                                                       maxlen))
