library(survey)
library(srvyr)
library(readr)
library(dplyr)
library(data.table)
library(haven)

#X201403_EnemduBDD_15anios <- read_sav("C:/Users/sotom/Downloads/ENEMDU_2014_03/bdd_enemdu_15anios_03_2014/201403_EnemduBDD_15anios.sav")

Base_de_Datos<- read_csv("C:/Users/sotom/Desktop/TIC/ENEMDU_compilado.csv")

Base_de_Datos <- Base_de_Datos %>% mutate(fexp = as.character(fexp))
Base_de_Datos <- Base_de_Datos %>% mutate(fexp = gsub(",","\\.",fexp))
Base_de_Datos <- Base_de_Datos %>% mutate(fexp = as.numeric(fexp))

###################################################################################################
#                                                                                                 #
#                               MODIFICACION DE VARIABLES Y TRATAMIENTO DE NAs                    #
#                                                                                                 #
###################################################################################################

## SABE LEER Y ESCRIBIR
# Si se encuentra un valor vacío se coloca la marca 1 (Si)

Base_de_Datos<-Base_de_Datos|> mutate(leer_escribir = ifelse(is.na(p11),1,p11))

#Años de trabajo
# Si está vacío se coloca 0 (Sin experiencia)
Base_de_Datos<-Base_de_Datos|>mutate(anios_trabajo = ifelse(is.na(p45),0,p45))

# Obtuvo titulo bachillerato
# Si tiene NA = 0, si estuvo en el bachillerato menos de 3 años = 0
Base_de_Datos<-Base_de_Datos |> mutate(estudiante_bach =ifelse(is.na(p10a),0,ifelse(p10a==7,ifelse(p10b<3,0,1),ifelse(p10a<7,0,1))))  

# Obtuvo titulo universidad
# Si tiene NA = 0, si estuvo en la univ menos de 4 años = 0
Base_de_Datos<-Base_de_Datos |> mutate(estudiante_univ =ifelse(is.na(p10a),0,ifelse(p10a==9,ifelse(p10b<4,0,1),ifelse(p10a<9,0,1))))  

# Ingpc
# ingpc = NA entonces ingpc = 0
Base_de_Datos<-Base_de_Datos|> mutate(ingpc = ifelse(is.na(ingpc),0,ingpc))

##############################################################################
#                                                                            #
#                        NUEVAS VARIABLES                                    #
#                                                                            #
##############################################################################

# Variables por crear:

# Numero de personas en el hogar
# Numero de personas mayores a 18
# Numero de personas mayores a 23
# Numero de personas edades 6-17
# Numero de personas con titulo universitario
# Numero de personas con titulo bachillerato
# Numero de personas mayores a 5 años que saben leer escribir
# Ratio de trabajo por persona
# Numero de personas con titulo secundario

Base_de_Datos<-Base_de_Datos |>
  
  group_by(id_hogar,periodo)|>
  
  mutate(mayores_a_18 = sum(p03>=18), 
         num_personas_hogar = n(),
         entre_6_17 = sum(p03<=17 & p03>=6),
         mayores_a_23 = sum(p03>=23),
         num_leen_escrib = sum(leer_escribir == 1 & p03>5),
         mayores_a_5 = sum(p03>5),
         ratio_trabajo = ifelse(p03==0,0,anios_trabajo/p03),
         num_titulo_bach = sum(estudiante_bach),
         num_titulo_univ = sum(estudiante_univ),
         num_asiste_clases = sum(p03<=17 & p03>=6 & p07==1)
  )

Base_de_Datos<- ungroup(Base_de_Datos)

###########################################################################
#                                                                         #                                        
#                         CALCULOS RATIOS                                 #     
#                                                                         #                        
###########################################################################


Base_de_Datos<-Base_de_Datos|>
  group_by(id_hogar,periodo)|>
  mutate(ratio_experiencia = mean(ratio_trabajo),
         ratio_analfabetismo = num_leen_escrib/mayores_a_5,
         ratio_titulo_bach = num_titulo_bach/mayores_a_18,#familia con menores de 18 NA
         ratio_titulo_univ = num_titulo_univ/mayores_a_23,#hay familias menores a 23 y num_titulo_basch= NA
         ratio_asistencia_clases = num_asiste_clases/entre_6_17#Dejarlo NA
         )

Base_de_Datos<-ungroup(Base_de_Datos)
Base_de_Datos<-as.data.table(Base_de_Datos)


# ratios asistencia a clases: 596907 (0.29 %)

#Decisión: quitar los NAs titulo bach y ratio titulo univ
Base_de_Datos<-Base_de_Datos[!is.na(ratio_titulo_bach),]
Base_de_Datos<-Base_de_Datos[!is.na(ratio_titulo_univ),]

Base_de_Datos<-Base_de_Datos[!Base_de_Datos$ratio_titulo_univ >1]
Base_de_Datos<-Base_de_Datos[!Base_de_Datos$ratio_titulo_bach >1]



# Agregar categoría para NAs en asistencia clase



###########################################################################
#                                                                         #                                        
#                       Base Nivel Provincial                             #     
#                                                                         #                        
###########################################################################

#############   Proporcion hombres/mujeres    ############
total_sex <- Base_de_Datos[, .(total = sum(fexp)), by = .(periodo, prov, p02)]
total_sex_general<- total_sex[, .(total_general = sum(total)), by = .(periodo, prov)]

sexo <- merge(total_sex, total_sex_general, by = c("periodo", "prov"))

# Calculamos la proporción de hombres y mujeres
sexo[, proporcion_mujeres := total / total_general]

sexo <- sexo |> filter(p02 == "2") |> select(-c(total,p02,total_general))

#BASE_model <- merge(promedios_ingresos,sexo, by = c("periodo", "prov"), all = TRUE)
BASE_model <- sexo


############   Ratio experiencia #######################
# usar información por hogar: jefe del hogar

ratio_exp_prov <- Base_de_Datos[, .(ratioexp = sum(ratio_experiencia * fexp) / sum(fexp)), by = .(prov, periodo)]
BASE_model <- merge(BASE_model,ratio_exp_prov, by = c("periodo", "prov"), all = TRUE)


############    Ratio analfabetismo ######################

ratio_analf_prov <- Base_de_Datos[, .(ratioanalf = sum(ratio_analfabetismo * fexp) / sum(fexp)), by = .(prov, periodo)]
BASE_model <- merge(BASE_model,ratio_analf_prov, by = c("periodo", "prov"), all = TRUE)


############    Ratios bachilleres ####################

ratio_bach_prov <- Base_de_Datos[, .(ratiobach = sum(ratio_titulo_bach * fexp) / sum(fexp)), by = .(prov, periodo)]
BASE_model <- merge(BASE_model,ratio_bach_prov, by = c("periodo", "prov"), all = TRUE)


############    Ratio universitario titulado ################
ratio_univ_prov <- Base_de_Datos[, .(ratiouniv = sum(ratio_titulo_univ * fexp) / sum(fexp)), by = .(prov, periodo)]
BASE_model <- merge(BASE_model,ratio_univ_prov, by = c("periodo", "prov"), all = TRUE)

############    Ratios Asistencia a clase ##################
ratio_asistclases_prov <- Base_de_Datos[, .(ratio_asistencia_clases = sum(ratio_analfabetismo * fexp) / sum(fexp)), by = .(prov, periodo)]
BASE_model <- merge(BASE_model,ratio_asistclases_prov, by = c("periodo", "prov"), all = TRUE)

############# Edad promedio  ##############
# 0 NA

edad <- Base_de_Datos[, .(edad_prom = sum(p03 * fexp) / sum(fexp)), by = .(prov, periodo)]

BASE_model <- merge(BASE_model,edad, by = c("periodo", "prov"), all = TRUE)


#############  Ingresos ####################
#pONER 0s en NAS
promedios_ingresos <- Base_de_Datos[, .(promedio_ingpc = sum(ingpc * fexp) / sum(fexp)), by = .(prov, periodo)]

BASE_model <- merge(BASE_model,promedios_ingresos, by = c("periodo", "prov"), all = TRUE)

View(BASE_model)


#######################################################################################
#                             CODIGO ANTIGUO                                          #
#                               NO PARTE DEL CODIGO                                   #
#######################################################################################

############# Sexo p02 ##############
# 0 NA
# % sexo femenino

Base_de_Datos_NA<- as.data.table(Base_de_Datos[!is.na(Base_de_Datos$p02),])

total_sex <- Base_de_Datos_NA[, .(total = sum(fexp)), by = .(periodo, prov, p02)]
total_sex_general<- total_sex[, .(total_general = sum(total)), by = .(periodo, prov)]

sexo <- merge(total_sex, total_sex_general, by = c("periodo", "prov"))

# Calculamos la proporción de hombres y mujeres
sexo[, proporcion_mujeres := total / total_general]

sexo <- sexo |> filter(p02 == "2") |> select(-c(total,p02,total_general))

BASE_model <- merge(promedios_ingresos,sexo, by = c("periodo", "prov"), all = TRUE)

############# Edad p03 ##############
# 0 NA

Base_de_Datos_NA<- as.data.table(Base_de_Datos[!is.na(Base_de_Datos$p03),])

edad <- Base_de_Datos_NA[, .(edad_prom = sum(p03 * fexp) / sum(fexp)), by = .(prov, periodo)]

BASE_model <- merge(BASE_model,edad, by = c("periodo", "prov"), all = TRUE)


####### Ingresos #############
# El 0.07% de los datos son NA
# Por qué hay NAs en la base? Que significan?

Base_de_Datos_NA <- as.data.table(Base_de_Datos[!is.na(Base_de_Datos$ingpc),])
promedios_ingresos <- Base_de_Datos_NA[, .(promedio_ingpc = sum(ingpc * fexp) / sum(fexp)), by = .(prov, periodo)]


############ Asiste a clases p07 ############
# 8% de los datos son NA
# NAs representan niños menores a 4 años

Base_de_Datos_NA<-as.data.table(Base_de_Datos[!is.na(Base_de_Datos$p07),])

total_asist<-Base_de_Datos_NA[, .(total = sum(fexp)), by = .(periodo, prov, p07)]

total_asist_general<- total_asist[, .(total_general = sum(total)), by = .(periodo, prov)]

asiste_clases <- merge(total_asist, total_asist_general, by = c("periodo", "prov"))

asiste_clases[, proporcion_asiste_clases := total / total_general]

asiste_clases <- asiste_clases |> filter(p07 == "1") |> select(-c(total,p07,total_general))

BASE_model <- merge(BASE_model,asiste_clases, by = c("periodo", "prov"), all = TRUE)

###### Nivel de instrucción 10a ############
# 8% de los datos son NA
# NAs representan niños menores a 4 años

#1:ninguno, 2: 
#Propuesta: porcentaje universitarios?

Base_de_Datos_NA<- as.data.table(Base_de_Datos[!is.na(Base_de_Datos$p10a),])


###### Sabe leer y escribir p11 ###############
# 48.8% de los datos son NAs

Base_de_Datos_NA<- as.data.table(Base_de_Datos[!is.na(Base_de_Datos$p11),])

total_analf <- Base_de_Datos_NA[, .(total = sum(fexp)), by = .(periodo, prov, p11)]
total_analf_general<- total_analf[, .(total_general = sum(total)), by = .(periodo, prov)]

analf <- merge(total_analf, total_analf_general, by = c("periodo", "prov"))
analf[, proporcion := total / total_general]

###### Por sus estudios obtuvo algun título p12a ######## -> hay muchos dónde la respuesta no aplica
# preguntar cómo colocar esta variable.


###### Trabajó la semana anterior? p20 ############### 1: si
# 8% de los datos son NA

Base_de_Datos_NA<- as.data.table(Base_de_Datos[!is.na(Base_de_Datos$p20),])

##### Cuantos años trabaja? p45 ################
# 50% de datos NAs

##### Recibió bono de desarrollo? p75 ##########

#8% no aplican (niños menores a 5 años)


Base_de_Datos_NA<-as.data.table(Base_de_Datos[!is.na(Base_de_Datos$p75),])

total_bono<-Base_de_Datos_NA[, .(total = sum(fexp)), by = .(periodo, prov, p75)]

total_bono_general<- total_bono[, .(total_general = sum(total)), by = .(periodo, prov)]

recibe_bono <- merge(total_bono,total_bono_general, by = c("periodo", "prov"))

recibe_bono[, proporcion_recibe_bono := total / total_general]

recibe_bono <- recibe_bono |> filter(p75 == "1") |> select(-c(total,p75,total_general))

BASE_model <- merge(BASE_model,recibe_bono, by = c("periodo", "prov"), all = TRUE)



nombres_provincias <- c(
  "Azuay", "Bolivar", "Cañar", "Carchi", "Cotopaxi", "Chimborazo", "El Oro",
  "Esmeraldas", "Guayas", "Imbabura", "Loja", "Los Rios", "Manabi",
  "Morona Santiago", "Napo", "Pastaza", "Pichincha",
  "Tungurahua", "Zamora Chinchipe", "Galápagos", "Sucumbios",
  "Orellana", "Santo Domingo De Los Tsachilas", "Santa Elena"
)

write.csv(BASE_model,"base_prov_trimestral.csv",row.names = FALSE)

promedios_ingresos$prov<-nombres_provincias[promedios_ingresos$prov]
write.csv(promedios_ingresos, "ingresos_ecuador_compilado.csv", row.names = FALSE)

# PARA CADA VARIABLE SE necesita un diseño muestral distinto

# Base_de_Datos_NA <- Base_de_Datos[!is.na(Base_de_Datos$ingpc),]
# 
# diseño_muestral_NA <- svydesign(ids=~upm,
#                                 strata=~estrato,
#                                 weights=~fexp,
#                                 data=Base_de_Datos_NA,
#                                 nest=TRUE)
# 
# ingreso_nacional <- svyby(~ingpc, ~periodo, diseño_muestral_NA, svymean)
# pobreza_nacional <- svyby(~pobreza, ~periodo, diseño_muestral_NA, svyciprop)

#Intentar hacerlo por semestres :c

