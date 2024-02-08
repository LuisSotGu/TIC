###########################################
#                                         #
#       MODELO ECONOMETRICO ESPACIAL      #
#                                         #
###########################################
#install.packages("splm")
library(splm)
library(dplyr)
library(plm)
library(spatialreg)
library(sf)
library(readr)
library(sfdep)
library(spdep)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gstat)
library(gridExtra)
library(patchwork)
library(SDPDmod)
library(lmtest)


setwd("~/TIC")
# Carga de las variables
panel<-read.csv("~\\TIC\\base_prov_trimestral.csv")
panel_mineduc<-read.csv("~\\TIC\\base_prov_mineduc.csv")
panel<-merge(panel,panel_mineduc,by=c("prov","periodo"))

panel<-panel|> filter(prov!=90 & prov!=20)
panel<- panel |> mutate(prov = ifelse(prov>20,prov - 1,prov))

panel<-panel[order(panel$prov),]

# arreglar porque hay 24 provincias
pdata <- pdata.frame(panel, index = c("prov", "periodo"))

# carga de la SWM
W<-as.matrix(read.csv("~\\TIC\\W_ord.csv"))
#probar con matriz 1,0s
shape_prov<-st_read("~\\TIC\\SHP\\nxprovincias.shp")
shape_prov <- shape_prov %>%
  filter(!(DPA_DESPRO %in% c("GALAPAGOS", "ZONA NO DELIMITADA")))

W_queen<-nb2listw(st_contiguity(shape_prov$geometry))

# Formula


fm1<-log(promedio_ingpc)~
  proporcion_mujeres +
  ratioanalf+
  RatioDocente+
  Ratio_Desertores+
  Ratio_BACH+
  ratiouniv+
  ratioexp+
  ratiobach+
  ratio_empleo_pleno+
  edad_prom

# Prueba LM para error
slmtest(fm1,data = pdata,listw = W,test="lme")

# Prueba LM para lag
slmtest(fm1,data = pdata,listw = W,test="lml")

# Prueba LM robusta para error
slmtest(fm1,data = pdata,listw = W,test="rlme")
# Prueba LM robusta para lag
slmtest(fm1,data = pdata,listw = W,test="rlml")


##################################################
#               POOLED OLS                       #
##################################################
pooledOLS<-plm(fm1,data = just_data,model = "pooling")
summary(pooledOLS)
stargazer(pooledOLS)

##################################################
#           PRUEBAS DE ESPECIFICACION            #
##################################################

# Autocorrelación serial residuos
pdwtest(pooledOLS)
pbgtest(pooledOLS)
# Heterocedasticidad
bptest(pooledOLS)
# Multicolinealidad
car::vif(pooledOLS)
corrplot::corrplot(cor(just_data),method = "ellipse",
                   type="upper")

#####################################################
#####################################################
fm<-log(promedio_ingpc)~
  #proporcion_mujeres +
  ratioanalf+ #Número de personas no analfabetas / total personas
  RatioDocente+ # Numero de alumnos por docente
  Ratio_Desertores+ # Número de desertores / Total
  Ratio_BACH+ # Número de alumnos bgu / número de instituciones con bgu
  ratiouniv+ # Número de universitarios / número que deberían estar en la U
  #ratioexp+ #
  ratiobach+ # num_bachilleres / num de estudiantes mayores a 17
  ratio_empleo_pleno+ # num con empleo pleno / num de personas que deberían tener empleo pleno
  edad_prom 

##################################################
#         MAXIMA VEROSIMILITUD                   #
##################################################

## Fixed effects SAR
sarfe <- spml(formula = fm, data = pdata, listw = W, model = "within",
              effect = "individual",
              spatial.error = "none", lag = TRUE)
summary(sarfe)

## Fixed effects SEM
semfe <- spml(formula = fm, data = pdata, listw = W, model = "within",
              effect = "individual",
              spatial.error = "b", lag = FALSE)
summary(semfe)

####################################################
#        Estadísticos para comparar                #

#AIC, BIC, MORAN, R^2, EMC
# Moran en Errores
sphtest(fm,data=pdata,
        listw = mat2listw(W),
        spatial.model = "error",
        method = "ML",
        errors = "BSK")
sphtest(fm,data=pdata,
        listw = mat2listw(W),
        spatial.model = "lag",
        method = "ML",
        errors = "BSK")

sphtest(fm,data=pdata,
        listw = mat2listw(W),
        spatial.model = "lag",
        method = "GM")
sphtest(fm,data=pdata,
        listw = mat2listw(W),
        spatial.model = "error",
        method = "GM")
##################################################################
#                       Statistics
###################################################################
source(AICsplm.R) # Necesario para obtener AIC, BIC de objetos splm
# El archivo es propiedad de Raphael Saldanha. El archivo AICsplm.R se puede descargar de:
# https://github.com/rfsaldanha/ecoespacialunicamp/blob/master/OLD/AICsplm.R

AICsplm(sarfe)

AICsplm(semfe)

#residuals moran
generar_secuencia <- function(inicio_anio, fin_anio, excluido_anio) {
  secuencia <- character(0)
  
  for (anio in inicio_anio:fin_anio) {
    if (anio != excluido_anio) {
      for (trimestre in 1:4) {
        secuencia <- c(secuencia, paste0(anio, "T", trimestre))
      }
    }
  }
  
  return(secuencia)
}

prov_generator<-function(trimestre = "2014T1"){
  provtrim<-numeric(23)
  for(i in 1:23){provtrim[i]<-paste0(i,"-",trimestre)}
  return(provtrim)
}

sarfe$residuals[provtrim]

trimestres<-generar_secuencia(2014,2022,2020)

I<-numeric(length = length(trimestres))
pvalue<-numeric(length = length(trimestres))
for(t in trimestres){
  I[which(trimestres==t)]<-moran(semfe$residuals[prov_generator(trimestre = t)],listw = mat2listw(W),n=23,S0 = 94.87682)$I
  pvalue[which(trimestres==t)]<-moran.test(semfe$residuals[prov_generator(trimestre = t)],listw = mat2listw(W))$p.value
}

plot(1:length(trimestres),pvalue,type="l",col="red",lwd=2,ylim = c(-0.3,1),lty=1, xlab = "Periodos", ylab = " ", main = "I Moran para el error y su significancia: Modelo SEM")
lines(1:length(trimestres),I,col="blue",lwd=2,lty=2)
abline(h=0)
legend("bottomright", legend = c("I Moran","p valor"), col = c("blue", "red"), lty = c(2, 1), lwd = 2)

