
# Ejercicio 3, 2023

library(foreign) 
library(dplyr)
library(tidyverse)
library(estimatr)
library(texreg)
library(xtable)
library(stargazer)

rm(list=ls())

# -----------------Procesamiento datos ----------------- #

data <- read.dta("input/dataset.dta")

# --------- Actividad 2 ------ #
data2 <- filter(data, MainSample==1)

# a y b
#############
# Opcion 1
modela<-lm_robust(Share_reg70_w2 ~ LnDistMilitaryBase +
	                               share_allende70 + lnDistRegCapital + sh_rural_70,
								   fixed_effects = ~ IDProv,
								   weights = Pop70, data=data2)

summary(modela)

# Opcion 2
modelb<-lm_robust(Share_reg70_w2 ~ LnDistMilitaryBase +
	                               share_allende70 + lnDistRegCapital + sh_rural_70 + Pop70_pthousands,
								   fixed_effects = ~ IDProv,
								   weights = Pop70, data=data2)

summary(modelb)

texreg(list(modela,modelb),
custom.model.names=c("Modelo 1","Modelo 2"),
caption ="Modelos Panel A",
digits=3,
include.ci=FALSE,
float.pos="h",
caption.above=TRUE,
include.fstatistic=FALSE
)


# c y d
#############
# Opcion 1
modelc<-iv_robust(Share_reg70_w2 ~ shVictims_70 + share_allende70 + lnDistRegCapital + sh_rural_70 |
	                               LnDistMilitaryBase + share_allende70 + lnDistRegCapital + sh_rural_70,
								   fixed_effects = ~ IDProv,
								   weights = Pop70, data=data2)

summary(modelc)

# Opcion 2
modeld<-iv_robust(Share_reg70_w2 ~ shVictims_70 + share_allende70 + lnDistRegCapital + sh_rural_70 + Pop70_pthousands |
	                               LnDistMilitaryBase + share_allende70 + lnDistRegCapital + sh_rural_70 + Pop70_pthousands,
								   fixed_effects = ~ IDProv,
								   weights = Pop70, data=data2)

summary(modeld)


texreg(list(modelc,modeld),
custom.model.names=c("Modelo 1","Modelo 2"),
caption ="Modelos Panel A",
digits=3,
include.ci=FALSE,
float.pos="h",
caption.above=TRUE,
include.fstatistic=FALSE
)




# ------------- Actividad 3 ------------- #
library(DirectEffects)

table(data2$IDProv, exclude=NULL)

# Centrar mediador
summary(data2$shVictims_70)
table(data2$shVictims_70, exclude=NULL)

data2$shVictims_70M <- data2$shVictims_70 - mean(data2$shVictims_70)
summary(data2$shVictims_70M)

# b.i y b.ii
#########

# Opcion 1
	# Outcome: Share_reg70_w2
	# Tratamiento: LnDistMilitaryBase
	# Mediador: shVictims_70
	# Confounders pre-tratamiento: shVictims_70, share_allende70, lnDistRegCapital, sh_rural_70, Pop70_pthousands
	# Confounders post-tratamiento: IDProv
	
direct2 <- sequential_g(Share_reg70_w2 ~  LnDistMilitaryBase + # A
		                             share_allende70 + lnDistRegCapital + sh_rural_70 + Pop70_pthousands | # X
						             factor(IDProv) | # Z
						             shVictims_70M, # M
						             weights = Pop70, data=data2)

summary(direct2)					  	
		





