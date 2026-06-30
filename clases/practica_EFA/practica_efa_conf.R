## ----results='hide'-----------------------------------------------------------------------------------------

pacman::p_load(stargazer, # Reporte a latex
sjPlot, sjmisc, # reporte y gráficos
corrplot, # grafico correlaciones
xtable, # Reporte a latex
Hmisc, # varias funciones
psych, # fa y principal factors
psy, # scree plot function
nFactors, # parallel
GPArotation, devtools, ggrepel, EFAtools, psychTools) # rotación



## ----tidy=FALSE---------------------------------------------------------------------------------------------
data = read.csv("./efa_asignaturas.csv")

#purl("C:/Users/Daniel Miranda/Dropbox (MIDE)/MIDEUC/Clases/SEM_2020/sesiones/2 efa/practica2sem/practica2sem.Rmd", output="test.R", documentation=2)

#purl("test.Rmd", output = "test2.R", documentation = 2)



## -----------------------------------------------------------------------------------------------------------
summary(data)

head(data) # muestra 6 primeros casos, 
           # útil para revisar lectura de datos

dim(data)  # filas columnas

nrow(na.omit(data)) # número de casos con datos completos

View(data)

str(data)

names(data)


## -----------------------------------------------------------------------------------------------------------
stargazer(data, type = "text")  # para visualizar en consola



## ----results='asis', eval=FALSE-----------------------------------------------------------------------------
## stargazer(data, type = "html") # a html


## -----------------------------------------------------------------------------------------------------------
#sjplot(data$BIO, "frq") # no muy buena descripción ...

plot_frq(data) 



## -----------------------------------------------------------------------------------------------------------
data2=6-data # recode items para que más acuerdo vaya a la derecha
labels <- c("Muy de acuerdo", "De acuerdo","Neutro", "Desacuerdo", "Muy en desacuerdo")
items <- c("Biología", "Geografía", "Química", "Algebra", "Cálculo", "Estadística")



## ----fig.width=8,fig.height=4-------------------------------------------------------------------------------
plot_likert(data2, 
           legend.labels = labels,
           axis.labels = items,
           cat.neutral = 3, # identifica a indiferentes
           geom.colors = "PuBu", # colorbrewer2.org para temas 
           sort.frq = "neg.asc" # sort descending
           )  



## ----results='asis'-----------------------------------------------------------------------------------------
#sjPlot::tab_stackfrq(data2)



## -----------------------------------------------------------------------------------------------------------
corMat  <- round(cor(data), 2) # estimar matriz pearson

corMat # muestra matriz



## ----results='asis'-----------------------------------------------------------------------------------------
#tab_corr(data, triangle = "lower") # Tabla en html

stargazer(corMat, title="Correlaciones") #Latex table



## -----------------------------------------------------------------------------------------------------------
M=cor(data) # matriz simple de correlaciones de los datos
corrplot(M, type="lower") # lower x bajo diagonal


## -----------------------------------------------------------------------------------------------------------
corrplot(M, type="lower",
      order="AOE", cl.pos="b", tl.pos="d") #agrega nombres en diag.


## -----------------------------------------------------------------------------------------------------------
KMO(corMat) 
cortest.bartlett(corMat, n = 300)


## -----------------------------------------------------------------------------------------------------------
fac_pa <- fa(r = data, nfactors = 2, fm= "pa")
#summary(fac_pa)
fac_pa

sjPlot::tab_fa(fac_pa)


## -----------------------------------------------------------------------------------------------------------
fac_ml <- fa(r = data, nfactors = 2, fm= "ml")
fac_ml

sjPlot::tab_fa(fac_ml)


## -----------------------------------------------------------------------------------------------------------
factor.plot(fac_ml, labels=rownames(fac_ml$loadings))


## -----------------------------------------------------------------------------------------------------------
fac_ml <- fa(r = data, nfactors = 2, fm= "ml", scores="regression")
data2=data
data3 <- cbind(data2, fac_ml$scores)
head(data3)



## -----------------------------------------------------------------------------------------------------------
scree.plot(data)   


## -----------------------------------------------------------------------------------------------------------
fa.parallel(corMat, n.obs=300)  


## ----eval=FALSE, echo=FALSE---------------------------------------------------------------------------------
## library(nFactors)
## ev <- eigen(corMat) # get eigenvalues
## ap <- parallel(subject=300,var=6,
##   rep=100,cent=.05)
## nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
## plotnScree(nS)
## 
## 
## #Factor de aceleración: solución numérica que muestra el punto que presenta el mayor cambio de pendiente
## 
## #Optimal coordinates: muestra el primer eigenvalue que puede ser mejor "extrapolado" desde el eigenvalue previo ("optimal coordinates are the extrapolated coordinates of the previous eigenvalue that allow the observed eigenvalue to go beyond this extrapolation" (http://www.inside-r.org/packages/cran/nFactors/docs/nScree)


## -----------------------------------------------------------------------------------------------------------
fac_ml_var <- fa(r = data, nfactors = 2, fm= "ml", rotate="varimax") # ortogonal
fac_ml_var



## -----------------------------------------------------------------------------------------------------------
fac_ml_pro <- fa(r = data, nfactors = 2, fm= "ml", rotate="promax")
fac_ml_pro



## ----eval=FALSE, results='markup'---------------------------------------------------------------------------
## # tab_fa(data, method="ml", rotation = "varimax", show.comm = TRUE, title = "Análisis factorial asignaturas")
## 


## ----results="asis", eval=FALSE, echo=TRUE------------------------------------------------------------------
## str(fac_ml_pro$loadings)
## class(fac_ml_pro$loadings)


## ----results="asis", eval=TRUE, echo=TRUE, message=FALSE, error=FALSE---------------------------------------
xtable(unclass(fac_ml_pro$loadings))

# fa2latex(fac_ml_pro) # Función desactualizada


## ----results='markup'---------------------------------------------------------------------------------------

psych::alpha(data)



## ----eval=FALSE---------------------------------------------------------------------------------------------
## sjPlot::tab_itemscale(data)
## 


## ----echo=FALSE, out.width = '70%', fig.retina = 1, fig.align='center'--------------------------------------
knitr::include_graphics("alpha.png")


## -----------------------------------------------------------------------------------------------------------

sci= data%>%
 dplyr::select(BIO, CHEM, GEO)

sjPlot::tab_itemscale(sci)


## -----------------------------------------------------------------------------------------------------------

mat= data%>%
 dplyr::select(ALG, CALC, STAT)

sjPlot::tab_itemscale(mat)


## -----------------------------------------------------------------------------------------------------------

psych::omega(sci, plot=FALSE)



## -----------------------------------------------------------------------------------------------------------

psych::omega(mat, plot=FALSE)



## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------
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
library(rio)
library(performance)
library(see)
library(interactions)


## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------

base= rio::import("https://www.dropbox.com/scl/fi/wo7rvoz0lw9wkx65boen8/ELSOC_W01_subset.csv?rlkey=kox9y3ie7n61dj26hidg91o56&dl=1")
names(base)


## -----------------------------------------------------------------------------------------------------------
# Vista previa de los datos
View(base)

# Mostrar los nombres de las variables contenidas en la base datos
names(base)

# Muestra el número de filas y columnas (casos y variables)
dim(base)

# Muestra los primeros 10 casos de la base para todas las variables
head(base, 10)

# Muestra los primeros 10 casos de la base para una variable específica
head(base$c01, 10)



## ----results='asis'-----------------------------------------------------------------------------------------
print(summarytools::dfSummary(base, method = "render")) ## Descripción de la base de datos



## ----echo=TRUE, results='markup'----------------------------------------------------------------------------
## Seleccionamos las variables a analizar
conf=base%>%
  dplyr::select(c05_01, c05_02, c05_03, c05_04, c05_05, c05_06, c05_07, c05_08)

tab_corr(conf)


## ----echo=FALSE---------------------------------------------------------------------------------------------
M <-cor(conf, use = "complete.obs")

corrplot(M, type="upper", order="hclust")



## -----------------------------------------------------------------------------------------------------------
tab_itemscale(conf)


## -----------------------------------------------------------------------------------------------------------
base <- base %>%
  dplyr::mutate(conf_inst= (c05_01 + c05_02 + c05_03 + c05_04 + c05_05  + c05_06 + c05_07 + c05_08)/8)

## Revisar la variable agregada
#print(summarytools::dfSummary(base, method = "render")) ## Descripción de la base de datos
names(base)

hist(base$conf_inst)

skim(base$conf_inst)

