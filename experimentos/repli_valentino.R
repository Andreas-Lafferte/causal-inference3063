# 0. Install and load required packages
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
library(readr)

# 1. Load the data (adjust path as needed)
data <- read_dta(here("experimentos/Data.dta"))

data <- data %>%
  mutate(
    polviews    = factor(polviews,    levels = c(0,1,2), labels = c("Republican","Independent","Democrat")),
    gender      = factor(gender,      levels = c(1,2,3), labels = c("Men","Women","Nonbinary")),
    race4       = factor(race4,       levels = c(1,2,3,4), labels = c("White","Black","Hispanic","Other")),
    intentional = factor(intentional, levels = c(0,1), labels = c("Unintentional","Intentional")),
    treatvscons = factor(treatvscons, levels = c(0,1), labels = c("UnequalTreatment","UnequalOutcomes")),
    powerdiff   = factor(powerdiff,   levels = c(0,1), labels = c("LessPowerful","MorePowerful"))
  )

# 2. Define survey design for clustering by PID and population weight
svy_design <- survey::svydesign(
  ids     = ~PID,
  weights = ~weight,
  data    = data
)

# 3. Models: only unequal treatment vs unequal outcome

model_het_tvc <- svyglm(
  appraisal ~ polviews*treatvscons + gender*treatvscons + race4*treatvscons + 
    age*treatvscons + educ*treatvscons + lninc*treatvscons + usborn, design = svy_design)

# 4. Scales (alpha)
data <- data %>% 
  mutate(
    discrimination = psych::alpha(data[, c("groupMinorities", "groupWomen", "groupPoor")])$scores,
    complain       = psych::alpha(data[, c("complainMinorities", "complainWomen", "complainPoor")])$scores,
    attention      = psych::alpha(data[, c("attentionMinorities", "attentionWomen", "attentionPoor")])$scores,
    mip            = psych::alpha(data[, c("mipMinorities", "mipWomen", "mipPoor")])$scores,
    law            = psych::alpha(data[, c("lawMinorities", "lawWomen", "lawPoor")])$scores,
    affirm         = psych::alpha(data[, c("affirmMinorities", "affirmWomen", "affirmPoor")])$scores,
    fund           = psych::alpha(data[, c("fundMinorities", "fundWomen", "fundPoor")])$scores
  )

svy_design <- update(svy_design, discrimination = data$discrimination,
                     complain = data$complain, attention = data$attention,
                     mip = data$mip, law = data$law, affirm = data$affirm,
                     fund = data$fund)

# 5. Sociopolitical outcome models
socio_models <- function(outcome) {
  list(
    svyglm(as.formula(paste0(outcome, " ~ appraisal*intentional + polviews + gender + race4 + age + educ + lninc + usborn")), design = svy_design),
    svyglm(as.formula(paste0(outcome, " ~ appraisal*treatvscons + polviews + gender + race4 + age + educ + lninc + usborn")), design = svy_design),
    svyglm(as.formula(paste0(outcome, " ~ appraisal*powerdiff + polviews + gender + race4 + age + educ + lninc + usborn")), design = svy_design)
  )
}

outcomes <- c("discrimination", "complain", "attention", "mip", "law", "affirm", "fund")
models_list <- lapply(outcomes, socio_models)
names(models_list) <- outcomes

# 6. Extract marginal effects of appraisal by treatvscons for each outcome
extract_treatvscons_margins <- function(model) {
  emm <- emtrends(model, var = "appraisal", specs = ~ treatvscons)
  out <- broom::tidy(emm)
  out
}

tvc_margins <- lapply(models_list, function(m) extract_treatvscons_margins(m[[2]]))
names(tvc_margins) <- names(models_list)

# 7. Export

# write.csv(tvc_margins, file = here("experimentos/resultados_valentino_STATA.csv"))

# 8. Holm

results_tbl <- read_csv("experimentos/resultados_valentino_STATA.csv")

results_tbl <- results_tbl %>%
  mutate(p_holm = p.adjust(p_value, method = "holm"))

results_tbl %>% 
  mutate(p_value = round(p_value, 5),
         p_holm = round(p_holm, 5))

results_tbl$p_holm < 0.05
