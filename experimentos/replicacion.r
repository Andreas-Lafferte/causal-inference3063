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

here("Data.dta")

# 1. Load the data (adjust path as needed)
data <- read_dta(here("Data.dta"))

data <- data %>%
  mutate(
    polviews    = factor(polviews,    levels = c(0,1,2),
                         labels = c("Republican","Independent","Democrat")),
    gender      = factor(gender,      levels = c(1,2,3),
                         labels = c("Men","Women","Nonbinary")),
    race4       = factor(race4,       levels = c(1,2,3,4),
                         labels = c("White","Black","Hispanic","Other")),
    intentional = factor(intentional, levels = c(0,1),
                         labels = c("Unintentional","Intentional")),
    treatvscons = factor(treatvscons, levels = c(0,1),
                         labels = c("UnequalTreatment","UnequalOutcomes")),
    powerdiff   = factor(powerdiff,   levels = c(0,1),
                         labels = c("LessPowerful","MorePowerful"))
  )

# 3. Define survey design for clustering by PID and population weight
svy_design <- survey::svydesign(
  ids     = ~PID,
  weights = ~weight,
  data    = data
)

# 4. Models: homogeneity, treatment vs. control, power differences
# 4.1 Cultural homogeneity vs. heterogeneity (intentionality)
model_hom_int <- survey::svyglm(
  appraisal ~ factor(polviews) + factor(gender) + factor(race4) + age + 
    factor(educ) + lninc + factor(usborn) + factor(intentional), 
  design = svy_design)

model_het_int <- svyglm(
  appraisal ~ factor(polviews)*factor(intentional) + 
    factor(gender)*factor(intentional) + 
    factor(race4)*factor(intentional) + 
    age*factor(intentional) + 
    factor(educ)*factor(intentional) + 
    lninc*factor(intentional) + 
    factor(usborn), design = svy_design)


# 4.2 (Un)equal outcomes vs. treatment
model_hom_tvc <- svyglm(
  appraisal ~ factor(polviews) + factor(gender) + 
    factor(race4) + age + factor(educ) + lninc + 
    factor(usborn) + factor(treatvscons), design = svy_design)

model_het_tvc <- svyglm(
  appraisal ~ factor(polviews)*factor(treatvscons) + 
    factor(gender)*factor(treatvscons) + 
    factor(race4)*factor(treatvscons) + 
    age*factor(treatvscons) + 
    factor(educ)*factor(treatvscons) + 
    lninc*factor(treatvscons) + 
    factor(usborn), design = svy_design)

# 4.3 Power differences
model_hom_pow <- svyglm(
  appraisal ~ factor(polviews) + factor(gender) + factor(race4) + age + 
    factor(educ) + lninc + factor(usborn) + factor(powerdiff), 
  design = svy_design)

model_het_pow <- svyglm(
  appraisal ~ factor(polviews)*factor(powerdiff) + 
   factor(gender)*factor(powerdiff) + 
    factor(race4)*factor(powerdiff) + age*factor(powerdiff) + 
    factor(educ)*factor(powerdiff) + lninc*factor(powerdiff) + 
    factor(usborn), design = svy_design)


# Efectos marginales
emm_int <- emmeans(model_het_int, ~ polviews | intentional)
emm_tvc <- emmeans(model_het_tvc, ~ polviews | treatvscons)
emm_pow <- emmeans(model_het_pow, ~ polviews | powerdiff)

# Escalas tipo alpha y regresiones sociopolíticas
alpha_discrim <- psych::alpha(data[, c("groupMinorities", "groupWomen", "groupPoor")])$scores
alpha_complain <- psych::alpha(data[, c("complainMinorities", "complainWomen", "complainPoor")])$scores
alpha_attention <- psych::alpha(data[, c("attentionMinorities", "attentionWomen", "attentionPoor")])$scores
alpha_mip <- psych::alpha(data[, c("mipMinorities", "mipWomen", "mipPoor")])$scores
alpha_law <- psych::alpha(data[, c("lawMinorities", "lawWomen", "lawPoor")])$scores
alpha_affirm <- psych::alpha(data[, c("affirmMinorities", "affirmWomen", "affirmPoor")])$scores
alpha_fund <- psych::alpha(data[, c("fundMinorities", "fundWomen", "fundPoor")])$scores

# Crear nueva data.frame con estos puntajes
data <- data %>% 
  mutate(discrimination = alpha_discrim,
         complain = alpha_complain,
         attention = alpha_attention,
         mip = alpha_mip,
         law = alpha_law,
         affirm = alpha_affirm,
         fund = alpha_fund)

svy_design <- update(svy_design, discrimination = data$discrimination,
                 complain = data$complain,
                 attention = data$attention,
                 mip = data$mip,
                 law = data$law,
                 affirm = data$affirm,
                 fund = data$fund)

# Sociopolitical outcome models
socio_models <- function(outcome) {
  list(
    svyglm(as.formula(paste0(outcome, " ~ appraisal*factor(intentional) + factor(polviews) + factor(gender) + factor(race4) + age + factor(educ) + lninc + factor(usborn)")), design = svy_design),
    svyglm(as.formula(paste0(outcome, " ~ appraisal*factor(treatvscons) + factor(polviews) + factor(gender) + factor(race4) + age + factor(educ) + lninc + factor(usborn)")), design = svy_design),
    svyglm(as.formula(paste0(outcome, " ~ appraisal*factor(powerdiff) + factor(polviews) + factor(gender) + factor(race4) + age + factor(educ) + lninc + factor(usborn)")), design = svy_design)
  )
}

outcomes <- c("discrimination", "complain", "attention", "mip", "law", "affirm", "fund")
models_list <- lapply(outcomes, socio_models)

# Mostrar resultados
names(models_list) <- outcomes
for (out in names(models_list)) {
  print(paste("Resultados para:", out))
  modelsummary(models_list[[out]], title = paste("Modelos para:", out))
}

# Exportar a Word si se desea (requiere paquete flextable + modelsummary)
# modelsummary(models_list[["discrimination"]], output = "discrimination.docx")

# Opcional: Visualización de coeficientes con dotwhisker
dwplot(models_list[["discrimination"]]) +
  theme_minimal() +
  labs(title = "Effect of Appraisal on Discrimination", x = "Estimate", y = "")

