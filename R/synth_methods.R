# This script compares synthetic control methods using the example methods 
# contained within the packages documentation. I modify the augsynth 
# example to include a version with neither ridge nor unit fe, and a version
# with both ridge and unit fe.

library(augsynth)
library(tidysynth)
library(Synth)
library(microbenchmark)

sysname <- Sys.info()["sysname"]

mb <-  microbenchmark(
  synth = {
    data(synth.data)
    dataprep.out<-
      dataprep(
        foo = synth.data,
        predictors = c("X1", "X2", "X3"),
        predictors.op = "mean",
        dependent = "Y",
        unit.variable = "unit.num",
        time.variable = "year",
        special.predictors = list(
          list("Y", 1991, "mean"),
          list("Y", 1985, "mean"),
          list("Y", 1980, "mean")
        ),
        treatment.identifier = 7,
        controls.identifier = c(29, 2, 13, 17, 32, 38),
        time.predictors.prior = c(1984:1989),
        time.optimize.ssr = c(1984:1990),
        unit.names.variable = "name",
        time.plot = 1984:1996
      )
    synth.out <- synth(dataprep.out)},
  tidysynth = {smoking %>%
      synthetic_control(outcome = cigsale,
                        unit = state,
                        time = year,
                        i_unit = "California",
                        i_time = 1988,
                        generate_placebos= FALSE) %>%
      generate_predictor(time_window=1980:1988,
                         lnincome = mean(lnincome, na.rm = TRUE),
                         retprice = mean(retprice, na.rm = TRUE),
                         age15to24 = mean(age15to24, na.rm = TRUE)) %>%
      generate_predictor(time_window=1984:1988,
                         beer = mean(beer, na.rm = TRUE)) %>%
      generate_predictor(time_window=1975,
                         cigsale_1975 = cigsale) %>%
      generate_predictor(time_window=1980,
                         cigsale_1980 = cigsale) %>%
      generate_predictor(time_window=1988,
                         cigsale_1988 = cigsale) %>%
      generate_weights(optimization_window =1970:1988,
                       Margin.ipop=.02,Sigf.ipop=7,Bound.ipop=6) %>%
      generate_control()},
  augsynth_no_ridge_no_fe = augsynth(lngdpcapita ~ treated, fips, year_qtr, kansas,
                                     progfunc = "None", scm = F),
  augsynth_ridge_fe = augsynth(lngdpcapita ~ treated, fips, year_qtr, kansas,
                               progfunc = "Ridge", scm = T),
  
  times = 10)


# Save Results ------------------------------------------------------------

saveRDS(mb, paste0("output/synth_methods-", sysname, ".RDS"))
