library(survey)
library(srvyr)
library(readr)
library(dplyr)
library(data.table)
library(haven)
###########################################################

#                   TRATAMIENTO BASE                      #
#                         ENEMDU                          #

###########################################################

Base_de_Datos<- read_csv("~/TIC/ENEMDU_compilado.csv")

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
         num_asiste_clases = sum(p03<=17 & p03>=6 & p07==1),
         mayores_a_15 = sum(p03>=15),
         empleo_pleno = sum(condact==1)
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
         ratio_titulo_univ = num_titulo_univ/mayores_a_23,#hay familias menores a 23 y num_titulo_bach= NA
         ratio_asistencia_clases = num_asiste_clases/entre_6_17,#Dejarlo NA
         ratio_empleo_pleno = empleo_pleno/mayores_a_15
  )

Base_de_Datos<-ungroup(Base_de_Datos)
Base_de_Datos<-as.data.table(Base_de_Datos)


# ratios asistencia a clases: 596907 (0.29 %)

#Decisión: quitar los NAs titulo bach y ratio titulo univ
Base_de_Datos<-Base_de_Datos[!is.na(ratio_titulo_bach),]
Base_de_Datos<-Base_de_Datos[!is.na(ratio_titulo_univ),]

Base_de_Datos<-Base_de_Datos[!Base_de_Datos$ratio_titulo_univ >1]
Base_de_Datos<-Base_de_Datos[!Base_de_Datos$ratio_titulo_bach >1]


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
ratio_asistclases_prov <- Base_de_Datos[, .(ratio_asistencia_clases = sum(ratio_asistencia_clases * fexp) / sum(fexp)), by = .(prov, periodo)]
BASE_model <- merge(BASE_model,ratio_asistclases_prov, by = c("periodo", "prov"), all = TRUE)

############# Edad promedio  ##############
# 0 NA

edad <- Base_de_Datos[, .(edad_prom = sum(p03 * fexp) / sum(fexp)), by = .(prov, periodo)]

BASE_model <- merge(BASE_model,edad, by = c("periodo", "prov"), all = TRUE)

############# Empleo pleno #################
ratio_empleo_pleno<-Base_de_Datos[, .(ratio_empleo_pleno = sum(ratio_empleo_pleno * fexp) / sum(fexp)), by = .(prov, periodo)]
BASE_model <- merge(BASE_model,ratio_empleo_pleno, by = c("periodo", "prov"), all = TRUE)

#############  Ingresos ####################
#pONER 0s en NAS
promedios_ingresos <- Base_de_Datos[, .(promedio_ingpc = sum(ingpc * fexp) / sum(fexp)), by = .(prov, periodo)]

BASE_model <- merge(BASE_model,promedios_ingresos, by = c("periodo", "prov"), all = TRUE)

View(BASE_model)

promedios_ingresos<-promedios_ingresos|>filter(prov!=90)

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

