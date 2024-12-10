## code to prepare `DATASET` dataset goes here

# make this file for every data for testing

# ---------------------------------------------------------------------------- #

library(haven)
library(usethis)
ACCES_credit_WD_data <- read_dta("~/Rstudio/RDD code/Empirical Application/ACCES_credit_WD_data.dta")
puntos_corte_2000_2014 <- read_dta("~/Rstudio/RDD code/Empirical Application/puntos_corte_2000-2014.dta")

use_data(ACCES_credit_WD_data, internal = TRUE)
use_data(puntos_corte_2000_2014, internal = TRUE)
