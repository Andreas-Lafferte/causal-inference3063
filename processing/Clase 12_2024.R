#########################################################
#Clase 12: boxes 5.4, 5.5 y 5.6 de Gerber y Green (2012)#
#########################################################

#-------------Cargar paquetes con los que vamos a trabajar----------------#

# Limpia tu espacio de trabajo
rm(list=ls(all=TRUE))

# Activar paquetes relevantes
# Instalar paquetes "Formula", "strucchange" y "AER"
library(sandwich)
library(AER) #Función de variable instrumental, instalar 
library(foreign)

#----------------------------Data mining-----------------------------------#

data1 <- read.dta("input/Boxes5_4_5_5.dta")

# Vea que han cargado bien los datos
names(data1)
dim(data1)

# Seleccionar hogares unipersonales que son contactados puerta a puerta (canvass)
sel <-  data1$onetreat==1 & data1$mailings==0 & data1$phongotv==0 & data1$persons==1

# Verificar número de observaciones
table(sel)
data2 <- data1[sel,]

# Definir variables
v98      <- data2$v98 
persngrp <- data2$persngrp 
cntany   <- data2$cntany 

table(v98)

#--------------------------Estimaciones---------------------------------------#

# Box 5.4: ITT
# -----------

# SE no robustos
coef(summary(lm(v98 ~ persngrp)))
# SE robustos
itt_fit <- lm(v98 ~ persngrp)
coeftest(itt_fit,vcovHC(itt_fit, typt="HC2"))

# Box 5.5: ITT_D
# --------------

# SE no robustos
coef(summary(lm(cntany ~ persngrp)))
# Se robustos
itt_d_fit <- lm(cntany ~ persngrp)
coeftest(itt_d_fit,vcovHC(itt_d_fit))

# Box 5.6: CACE
# ------------

# SE no robustos
coef(summary(ivreg(v98 ~ cntany,~persngrp)))

# SE robustos
cace_fit <- ivreg(v98 ~ cntany,~persngrp)
coeftest(cace_fit,vcovHC(cace_fit))

library(estimatr)
iv=iv_robust(v98 ~ cntany | persngrp)
summary(iv)


screenreg(itt_fit)
screenreg(itt_d_fit)
screenreg(iv)



