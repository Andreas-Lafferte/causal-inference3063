library(modelsummary)


plot_models(list(m1_ittd, m2_ittd), show.values = T, ci.lvl = 95)

modelplot(
  list("D ~ Z con Länder FE" = m2_itt,
       "D ~ Z sin Länder FE" = m1_itt),
  coef_omit = "Intercept",
  coef_map = c("distance" = "Distance")) + 
  geom_vline(xintercept = 0, color = "darkorange", linetype = "dashed") +  
  guides(color = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = c("black", "gray50")) +
  labs(color = "Modelos")
  
