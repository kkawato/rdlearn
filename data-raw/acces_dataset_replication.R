# ---------------------------------------------------------------------------- #
# This code demonstrates how to create the `acces` dataset from raw data
# ---------------------------------------------------------------------------- #
# you need to download the original data `puntos_corte_2000_2014` and `ACCES_credit_WD_data by using 'dataverse' package.
# library("dataverse")
# library("tibble") # to see dataframes in tidyverse-form
# puntos_corte_2000_2014 <-
# ACCES_credit_WD_data <-

library(tidyverse)

# Clean the data
cut.v = loan.aux %>%
  filter(anho_corte == 2010) %>%
  select(icfes_departamento = cod_depto, corte_univ) %>%
  arrange(desc(corte_univ))

sdat = loan %>%
  filter(icfes_periodo %in% c(20101, 20102)) %>%
  select(icfes_departamento, ingresa_u3, icfes_puesto, elegible_icetex2) %>%
  left_join(cut.v, by = "icfes_departamento")

dat = sdat %>%
  filter(!((icfes_puesto < corte_univ) != elegible_icetex2))

dat.ave = dat %>%
  group_by(icfes_departamento, icfes_puesto) %>%
  summarise(ave = mean(ingresa_u3), .groups = "drop") %>%
  left_join(cut.v, by = "icfes_departamento")

ind.dep = dat.ave %>%
  count(icfes_departamento) %>%
  filter(n < 200) %>%
  pull(icfes_departamento)

dat.sub = dat.ave %>%
  filter(!(icfes_departamento %in% c(ind.dep)))

Y = dat.sub$ave  # Outcome
X = -dat.sub$icfes_puesto # Running variable
C = -dat.sub$corte_univ # Cutoff
# Map department names to icfes_departamento
department_map = loan.aux %>%
  select(icfes_departamento = cod_depto, departamento_name = departamento) %>%
  distinct()

# Add department names to the data
dat.sub = dat.sub %>%
  left_join(department_map, by = "icfes_departamento")
department = dat.sub$departamento_name

# Combine into final data frame
acces = data.frame(elig = Y, saber11 = X, cutoff = C, department = department)
