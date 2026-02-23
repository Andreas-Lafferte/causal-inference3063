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
library(dotwhisker)

# 1. Load the data (adjust path as needed)
data <- read_dta(here("Data.dta"))

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

# 3. Models: homogeneity, treatment vs. control, power differences
# 3.1 Cultural homogeneity vs. heterogeneity (intentionality)
model_hom_int <- survey::svyglm(
  appraisal ~ polviews + gender + race4 + age + educ + lninc + usborn + intentional, 
  design = svy_design)

model_het_int <- svyglm(
  appraisal ~ polviews*intentional + gender*intentional + race4*intentional + 
    age*intentional + educ*intentional + lninc*intentional + usborn, design = svy_design)

# 3.2 (Un)equal outcomes vs. treatment
model_hom_tvc <- svyglm(
  appraisal ~ polviews + gender + race4 + age + educ + lninc + usborn + treatvscons, design = svy_design)

model_het_tvc <- svyglm(
  appraisal ~ polviews*treatvscons + gender*treatvscons + race4*treatvscons + 
    age*treatvscons + educ*treatvscons + lninc*treatvscons + usborn, design = svy_design)

# 3.3 Power differences
model_hom_pow <- svyglm(
  appraisal ~ polviews + gender + race4 + age + educ + lninc + usborn + powerdiff, 
  design = svy_design)

model_het_pow <- svyglm(
  appraisal ~ polviews*powerdiff + gender*powerdiff + race4*powerdiff + 
    age*powerdiff + educ*powerdiff + lninc*powerdiff + usborn, design = svy_design)

# 4. Marginal effects with emmeans
emm_int <- emmeans(model_het_int, ~ polviews | intentional)
emm_tvc <- emmeans(model_het_tvc, ~ polviews | treatvscons)
emm_pow <- emmeans(model_het_pow, ~ polviews | powerdiff)

summary(emm_int)
summary(emm_tvc)
summary(emm_pow)

# 5. Scales (alpha)
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

# 6. Sociopolitical outcome models
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

# 7. Summary output
for (out in names(models_list)) {
  print(paste("Resultados para:", out))
  modelsummary(models_list[[out]], title = paste("Modelos para:", out))
}

# 8. Optional: Visualización con dotwhisker
dwplot(models_list[["discrimination"]]) +
  theme_minimal() +
  labs(title = "Effect of Appraisal on Discrimination", x = "Estimate", y = "")
