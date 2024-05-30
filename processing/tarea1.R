# Code 1: Data preparation

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               rio,
               here,
               vtable, 
               kableExtra,
               sjPlot,
               estimatr,
               panelr,
               plm,
               Matching,
               ebal,
               cobalt)

options(scipen=999)
rm(list = ls())


# 2. Data -----------------------------------------------------------------

db_or <- rio::import(file = here("input", "EVS_main.csv")) %>% 
  as_tibble()

glimpse(db_or)

# 3. Analysis -------------------------------------------------------------

db <- db_or %>% 
  dplyr::select(intolerance, Distance, prop_jewish25, 
                unemployment33, nazishare33, state)


lm(intolerance ~ Distance, data = db) %>% summary()


lm(intolerance ~ Distance + factor(state), data = db) %>% summary()


lm_robust(intolerance ~ Distance, data = db,
          clusters = state, se_type = "stata", # Cluster SE
          fixed_effects = ~ state) # FE con dummies

lm(intolerance ~ Distance + prop_jewish25 + unemployment33 + nazishare33, data = db) %>% summary()



lm_robust(intolerance ~ Distance + prop_jewish25 + unemployment33 + nazishare33, data = db,
          clusters = state, se_type = "stata", # Cluster SE
          fixed_effects = ~ state) 
