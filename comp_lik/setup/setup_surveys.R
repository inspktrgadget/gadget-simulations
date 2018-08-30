# define survey selectivities and error
cod_survey_selectivity <-
    logistic_selectivity(cod_lengrps, 0.25, 40, max_prop = 0.01)
flatfish_survey_selectivity <-
    logistic_selectivity(flatfish_lengrps, 0.25, 20, max_prop = 0.01)
capelin_survey_selectivity <-
    logistic_selectivity(capelin_lengrps, 2, 8, max_prop = 0.01)
survey_error <- 0.2
catch_error <- 0.2


# define the survey fleets
survey_st_year <- 90
survey_end_year <- 120
survey_data <- function(fleetname, step) {
    expand.grid(year = survey_st_year:survey_end_year,
                steps = step,
                area = 1,
                fleetname = fleetname,
                amount = 1)
}

# define survey fleets for cod
cod_spr_survey <-
    list(type = "totalfleet",
         suitability = make_exponentiall50_suit("spr", "cod"),
         amount = survey_data("spr", 1))
cod_aut_survey <-
    list(type = "totalfleet",
         suitability = make_exponentiall50_suit("aut", "cod"),
         amount = survey_data("aut", 3))
cod_survey_fleets <- make_gadget_fleet(spr = cod_spr_survey,
                                       aut = cod_aut_survey)

# define survey fleets for flatfish
flatfish_spr_survey <-
    list(type = "totalfleet",
         suitability = make_exponentiall50_suit("spr", "flatfish"),
         amount = survey_data("spr", 1))
flatfish_aut_survey <-
    list(type = "totalfleet",
         suitability = make_exponentiall50_suit("aut", "flatfish"),
         amount = survey_data("aut", 3))
flatfish_survey_fleets <- make_gadget_fleet(spr = flatfish_spr_survey,
                                            aut = flatfish_aut_survey)

# define survey fleets for capelin
capelin_spr_survey <-
    list(type = "totalfleet",
         suitability = make_exponentiall50_suit("spr", "capelin"),
         amount = survey_data("spr", 1))
capelin_aut_survey <-
    list(type = "totalfleet",
         suitability = make_exponentiall50_suit("aut", "capelin"),
         amount = survey_data("aut", 3))
capelin_survey_fleets <- make_gadget_fleet(spr = capelin_spr_survey,
                                           aut = capelin_aut_survey)
