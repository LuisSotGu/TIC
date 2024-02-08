library(sf)
library(readr)
library(sfdep)
library(spdep)
library(dplyr)
library(ggplot2)
library(tidyr)

######################################################
#           Construcción de la matriz Getis          #
######################################################


#Carga de el shp
shape_prov<-st_read("~\\TIC\\SHP\\nxprovincias.shp")
shape_prov <- shape_prov %>%
  filter(!(DPA_DESPRO %in% c("GALAPAGOS", "ZONA NO DELIMITADA")))

ingresos_ecuador_compilado <- read_csv("~\\TIC\\ingresos_ecuador_compilado.csv")
#Ingresos 
ingresos<- ingresos_ecuador_compilado |>
  filter(prov !="Galápagos")|>
  group_by(prov) |>
  summarise(promedio_ingpc = mean(promedio_ingpc))

ingresos$DPA_DESPRO<-toupper(ingresos$prov)

shape_prov<-merge(shape_prov,ingresos,by = "DPA_DESPRO",all.x = TRUE)

shape_prov$ing<-shape_prov$promedio_ingpc
#############################################################################################################

#       ORD GETIS MATRIX

##############################################################################################################

#Distancia entre centroides
centroids<-shape_prov %>% st_centroid(geometry)

distances_centroids<-st_distance(centroids)

distances<-distances_centroids/1000

#################   Funciones para calcular G_i(d(i,j)) #######################
# Ambas son funciones de G*, solo son implementaciones distintas para facilitar el codigo

G_star<-function(i,j,X = ingresos$promedio_ingpc){
  if(i==j){
    return(X[i]/sum(X))
  }
  W<-ifelse(as.numeric(distances[i,])<=as.numeric(distances[i,j]),1,0)
  #W[i]<-0
  return(sum(W%*%X)/sum(X))
}

G_star_d<-function(i,d,X= ingresos$promedio_ingpc){
  if(d==0){
    return(X[i]/sum(X))
  }
  W<-ifelse(as.numeric(distances[i,])<= d,1,0)
  #W[i]<-0
  return(sum(W%*%X)/sum(X))
}

################## Eleccion de la distancia crítica ############################

Y<-matrix(numeric(23),nrow=23,ncol=23)

for(i in 1:23){
  for(j in 1:23){
    Y[i,j]<-G_star(i,j)
  }}

mean_dists_crits<-numeric(23)

for(i in 1:23){
  x <- as.numeric(distances[i,])
  y <- Y[i,]
  
  # Pendientes entre puntos adyacentes
  slopes <- diff(y) / diff(x)
  
  # Pendiente máxima y su índice
  max_slope <- max(slopes)
  max_slope_index <- which.max(slopes)
  # Puntos correspondientes a la pendiente máxima
  x_max_slope_start <- x[max_slope_index]
  x_max_slope_end <- x[max_slope_index + 1]
  
  mean_dists_crits[i]<-mean(c(x_max_slope_start,x_max_slope_end))
}

mean_dists_crits# Vector de distancias criticas 


# Exportar la matriz W_ord

write.csv(W_ord, "W_ord.csv", row.names = FALSE)


####################### Construccion de la matriz W_ord   ######################

W_ord<-matrix(0,nrow=23,ncol=23)
dist_criticas<-mean_dists_crits

for(i in 1:23){
  dNN1<-as.numeric(min(distances[i,][-i]))
  dc<-dist_criticas[i]
  if(dc>dNN1){
    for(j in (1:23)[-i]){
      if(as.numeric(distances[i,j])<=dc){
        W_ord[i,j] = 
          abs(G_star_d(i,dc) - G_star(i,j))/(abs((G_star_d(i,dc) - G_star(i,i))))
        
      }else{
        W_ord[i,j] = 0
      }
    }
  }
  if(dc == dNN1){
    for(j in (1:23)[-i]){
      if(as.numeric(distances[i,j])==dc){
        W_ord[i,j] = 1
      }else{
        W_ord[i,j] = 0
      }
    }
  }
}