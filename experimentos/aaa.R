library(readr)
library(haven)
library(dplyr)
library(ggplot2)
library(broom)
library(margins)
library(survey)
library(modelsummary)
library(emmeans)
library(tinytable)
library(here)
library(psych)
library(dotwhisker)

results_tbl <- read_csv("resultados_valentino_STATA.csv")

results_tbl <- results_tbl %>%
  mutate(p_holm = p.adjust(p_value, method = "holm"))

results_tbl %>% 
  mutate(p_value = round(p_value, 5),
         p_holm = round(p_holm, 5))

results_tbl$p_holm < 0.05

holm_sig <-  p.adjust(results_tbl$p.value, method = "holm") < 0.05

holm_sig