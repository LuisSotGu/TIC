# Carga de datos MINEDUC
library(readr)
library(dplyr)

# Con las variables FIN:

# Necesito: 
# Numero de estudiantes por provincia
# Numero de profesores por provincia
# Numero de estudiantes al finalizar
# Numero de instituciones por provincia

educ_2014<-read_delim("Datos MINEDUC/2_MINEDUC_RegistrosAdministrativos_2014-2015Fin.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
educ_2015<-read_delim("Datos MINEDUC/2_MINEDUC_RegistrosAdministrativos_2015-2016Fin.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
educ_2016<-read_delim("Datos MINEDUC/2_MINEDUC_RegistrosAdministrativos_2016-2017Fin.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
educ_2017<-read_delim("Datos MINEDUC/2_MINEDUC_RegistrosAdministrativos_2017-2018Fin.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
educ_2018<-read_delim("Datos MINEDUC/2_MINEDUC_RegistrosAdministrativos_2018-2019Fin.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
educ_2019<-read_delim("Datos MINEDUC/2_MINEDUC_RegistrosAdministrativos_2019-2020Fin.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
educ_2020<-read_delim("Datos MINEDUC/2_MINEDUC_RegistrosAdministrativos_2020-2021Fin.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
educ_2021<-read_delim("Datos MINEDUC/2MINEDUC_RegistrosAdministrativos_2021-2022-Fin.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE,locale = locale(encoding = "latin1"))
educ_2022<-read_delim("Datos MINEDUC/MINEDUC_RegistrosAdministrativos_2022-2023Inicio.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE,locale = locale(encoding = "latin1"))

compil <- mget(paste0("educ_", 2014:2022))
peduc<-data.frame()

########################################################################
#                     CREACION DE NUEVAS VARIABLES                     #
########################################################################

#####################
# RATIO DOCENTE: numero de alumnos por docente
# RATIO ABANDONO: desertores/total
# COBERTURA BGU: número de alumnos bgu/ número de instituciones con bgu
# COBERTURA

# Para 2014-2019 no , para 2021 y 2022 se debe hacer aparte por la base
for (i in 1:9) {
  df <- compil[[i]]
  year<-2013+i
  if(year<2020){
    y<-rep(year,25)
    # Ratio Docentes
    df<-df[df$`Total_Estudiantes`!=0,]# Se quitan las instituciones con cero estudiantes
    df$RatioDocente<-df$Total_Docentes/df$`Total_Estudiantes`
    df_new<-aggregate(cbind(RatioDocente) ~ as.numeric(Cod_Provincia), data = df, FUN = mean)
    df_new$RatioDocente<-1/df_new$RatioDocente
    df_new$Año<-y
    # Ratio abandono
    columnas_desertores <- df %>% select(contains("Desertores"))# Seleccionar columnas que contienen la cadena "Desertores" en su nombre
    df<- df %>% mutate(Total_Desertores = rowSums(columnas_desertores, na.rm = TRUE))
    df<- df %>% mutate(Ratio_Desertores = ifelse(Total_Estudiantes>0,Total_Desertores/Total_Estudiantes,0))
    
    df_new2<-aggregate(cbind(Ratio_Desertores) ~ as.numeric(Cod_Provincia), data = df, FUN = mean)
    df_new$Ratio_Desertores<-df_new2$Ratio_Desertores
    
    # Ratio Bachillerato
    columnas_bachillerato <- df %>% select(contains("BACH"))
    df<-df %>% mutate(Total_BACH = rowSums(columnas_bachillerato,na.rm=TRUE))
    df<-df %>% mutate(Tiene_BACH =  ifelse(grepl("Bachillerato", Nivel_Educacion), 1, 0))
    
    df_new3<- df %>% group_by(Cod_Provincia) %>% summarize(Razon = round(sum(Total_BACH) / sum(Tiene_BACH)))
    df_new$Ratio_BACH<-df_new3$Razon
    
    peduc<-rbind(peduc,df_new)
    
  }
  if( year == 2021)
  {
    y<-rep(year,24)
    # Ratio Docentes
    df<-df[df$`Total_Estudiantes`!=0,]# Se quitan las instituciones con cero estudiantes
    df$RatioDocente<-df$Total_Docentes/df$`Total_Estudiantes`
    df_new<-aggregate(cbind(RatioDocente) ~ as.numeric(Cod_Provincia), data = df, FUN = mean)
    df_new$RatioDocente<-1/df_new$RatioDocente
    df_new$Año<-y
    # Ratio abandono
    columnas_desertores <- df %>% select(contains("Desertores"))# Seleccionar columnas que contienen la cadena "Desertores" en su nombre
    df<- df %>% mutate(Total_Desertores = rowSums(columnas_desertores, na.rm = TRUE))
    df<- df %>% mutate(Ratio_Desertores = ifelse(Total_Estudiantes>0,Total_Desertores/Total_Estudiantes,0))
    
    df_new2<-aggregate(cbind(Ratio_Desertores) ~ as.numeric(Cod_Provincia), data = df, FUN = mean)
    df_new$Ratio_Desertores<-df_new2$Ratio_Desertores
    # Ratio Bachillerato
    columnas_bachillerato <- df %>% select(contains("BACH"))
    df<-df %>% mutate(Total_BACH = rowSums(columnas_bachillerato,na.rm=TRUE))
    df<-df %>% mutate(Tiene_BACH =  ifelse(grepl("Bachillerato", `Nivel Educaci¢n`), 1, 0))
    
    df_new3<- df %>% group_by(Cod_Provincia) %>% summarize(Razon = round(sum(Total_BACH) / sum(Tiene_BACH)))
    df_new$Ratio_BACH<-df_new3$Razon
    
    peduc<-rbind(peduc,df_new)
  }
  if(year == 2022)
  {
    y<-rep(year,24)
    # Ratio Docentes
    df<-df[df$`Total Estudiantes`!=0,]# Se quitan las instituciones con cero estudiantes
    df$RatioDocente<-df$Total_Docentes/df$`Total Estudiantes`
    df_new<-aggregate(cbind(RatioDocente) ~ as.numeric(Cod_Provincia), data = df, FUN = mean)
    df_new$RatioDocente<-1/df_new$RatioDocente
    df_new$Año<-y
    # Ratio abandono
    columnas_desertores <- df %>% select(contains("Desertores"))# Seleccionar columnas que contienen la cadena "Desertores" en su nombre
    df<- df %>% mutate(Total_Desertores = rowSums(columnas_desertores, na.rm = TRUE))
    df<- df %>% mutate(Ratio_Desertores = ifelse(`Total Estudiantes`>0,Total_Desertores/`Total Estudiantes`,0))
    
    df_new2<-aggregate(cbind(Ratio_Desertores) ~ as.numeric(Cod_Provincia), data = df, FUN = mean)
    df_new$Ratio_Desertores<-df_new2$Ratio_Desertores
    # Ratio Bachillerato
    columnas_bachillerato <- df %>% select(contains("BACH"))
    df<-df %>% mutate(Total_BACH = rowSums(columnas_bachillerato,na.rm=TRUE))
    df<-df %>% mutate(Tiene_BACH =  ifelse(grepl("Bachillerato", `Nivel_Educaci¢n`), 1, 0))
    
    df_new3<- df %>% group_by(Cod_Provincia) %>% summarize(Razon = round(sum(Total_BACH) / sum(Tiene_BACH)))
    df_new$Ratio_BACH<-df_new3$Razon
    peduc<-rbind(peduc,df_new)
  }
}



#################################################################
#                       EXPORTANDO DATOS                        #
#################################################################

# Quitar 20 (Galapagos)
peduc<-peduc[peduc!=20,]

# Crear un data.frame con todas las combinaciones de Año y trimestre
nuevo_df <- expand.grid(Año = unique(peduc$Año), Trimestre = paste0("T", 1:4))

# Combinar con el data.frame original
resultado <- merge(nuevo_df, peduc, by = "Año")

# Agregar el sufijo al identificador del trimestre
resultado$periodo <- paste0(resultado$Año,resultado$Trimestre)

resultado$prov<-resultado$`as.numeric(Cod_Provincia)`

# Arreglando 2022 data
media_por_provincia <- resultado |> filter(Año != 2022) |>
  group_by(prov) |>
  summarise(media_valor = mean(Ratio_Desertores, na.rm = TRUE))

df_2022<-resultado |> filter(Año == 2022) |> group_by(prov,Año) |> left_join(media_por_provincia, by = "prov")|>ungroup()
df_2022$Ratio_Desertores<-df_2022$media_valor
df_2022$media_valor<-NULL

resultado<-rbind(resultado|> filter(Año!=2022),df_2022)


# Eliminar las columnas innecesarias
resultado <- resultado[, c("Año", "Trimestre", "periodo", setdiff(names(peduc), "Año"))]

resultado$prov<-resultado$`as.numeric(Cod_Provincia)`

peduc<- resultado |> select("periodo","prov","RatioDocente","Ratio_Desertores","Ratio_BACH")
peduc<-peduc[!is.na(peduc$prov),]

write.csv(peduc,file = "base_prov_mineduc.csv")
