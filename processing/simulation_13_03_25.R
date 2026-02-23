library(pacman)

pacman::p_load(randomizr, 
               fabricatr,
               estimatr,
               DeclareDesign,
               tidyverse,
               dataverse,
               rdss)

b <- 0
model <-
  declare_model(
    N = 1000,
    history = sample(c(0, 1), N, replace = TRUE),
    potential_outcomes(Y ~ b * history + runif(1, 0, 0.5) * Z + rnorm(N)))

inquiry <-
  declare_inquiry(ATE = mean(Y_Z_1) - mean(Y_Z_0))

data_strategy <-
  declare_sampling(S = complete_rs(N = N, n = 150), filter = S == 1) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z))

answer_strategy <-
  declare_estimator(Y ~ Z, .method = difference_in_means, inquiry = "ATE")

declaration_2.1 <- model + inquiry + data_strategy + answer_strategy
data<-draw_data(declaration_2.1)
data
draw_estimands(declaration_2.1)
draw_estimates(declaration_2.1)

diagnose_design(declaration_2.1,sims=100)

program_diagnosands <-
  declare_diagnosands(
    success = mean(estimate > 0.3 & p.value < 0.05 & estimand > 0.2)
  )
declaration_2.1 |>
  diagnose_design(diagnosands=program_diagnosands,sims=100)


