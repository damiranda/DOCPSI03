## ----message=FALSE, warning=FALSE-----------------------------------
#install.packages(c("splitstackshape","psych"))
library(readr)
library(splitstackshape)
library(psych)
library(summarytools)
library(skimr)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(table1)
library(modest)
library(BSDA)
library(crosstable)
library(gmodels)
library(corrplot)
library(sjPlot)
library(stargazer)


## ----message=FALSE, warning=FALSE-----------------------------------

base= read.csv("https://www.dropbox.com/scl/fi/qvdzks6my66zes3vwuwrs/ELSOC_W01_subset.csv?rlkey=3xoojm6ropijg9at6wt32o7wf&dl=1")



## -------------------------------------------------------------------
# Vista previa de los datos
#View(base)

# Mostrar los nombres de las variables contenidas en la base datos
names(base)

# Muestra el número de filas y columnas (casos y variables)
dim(base)

# Muestra los primeros 10 casos de la base para todas las variables
head(base, 10)

# Muestra los primeros 10 casos de la base para una variable específica
head(base$rank, 10)



## ----results='asis'-------------------------------------------------
print(summarytools::dfSummary(base, method = "render")) ## Descripción de la base de datos



## ----echo=TRUE, results='markup'------------------------------------
## Seleccionamos las variables a analizar
conf=base%>%
  dplyr::select(c05_01, c05_02, c05_03, c05_04, c05_05, c05_06, c05_07, c05_08)

tab_corr(conf)


## ----echo=FALSE-----------------------------------------------------
M <-cor(conf, use = "complete.obs")

corrplot(M, type="upper", order="hclust")



## -------------------------------------------------------------------
tab_itemscale(conf)


## -------------------------------------------------------------------
base <- base %>%
  dplyr::mutate(conf_inst= (c05_01 + c05_02 + c05_03 + c05_04 + c05_05  + c05_06 + c05_07 + c05_08)/8)

## Revisar la variable agregada
#print(summarytools::dfSummary(base, method = "render")) ## Descripción de la base de datos
names(base)

hist(base$conf_inst)

skim(base$conf_inst)


## -------------------------------------------------------------------
#names(base)

# Preparar la base: crear nombres y guardar variables de interés
reg=base%>%
  mutate(Interés= c13) %>%
  mutate(Conversación= c14_01) %>%
  mutate(Educación= m01) %>%
  mutate(Confianza= conf_inst) %>%
  dplyr::select(Confianza, Interés, Conversación, Educación)

 
skim(reg) 

# Matriz de correlaciones
tab_corr(reg)


## ----results='asis'-------------------------------------------------
simple= lm(Confianza ~ Interés, data=reg)
#summary(simple)
#stargazer(simple, type="html")
texreg::screenreg(simple)


## ----results='asis'-------------------------------------------------
multiple= lm(Confianza ~ Interés + Conversación + Educación, data=reg)
#summary(multiple)
#stargazer(multiple, type="html")
texreg::screenreg(multiple, digits = 3)

