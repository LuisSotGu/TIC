library(readxl)
library(dplyr)
library(readr)
library(data.table)
setwd("C:/Users/sotom/Desktop/TIC")

#Funcion util
obtener_prov <- function(x) {
  ifelse(nchar(x) == 5, substr(x, 1, 1), substr(x, 1, 2))
}


################### CARGA DE ENCUESTAS ###############################################

# 2014
ENEMDU_PERSONAS_2014_03_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2014_03/ENEMDU_PERSONAS_2014_03_hom/ENEMDU_PERSONAS_2014_03_hom.csv")
ENEMDU_PERSONAS_2014_06_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2014_06/ENEMDU_PERSONAS_2014_06_hom/ENEMDU_PERSONAS_2014_06_hom.csv")
ENEMDU_PERSONAS_2014_09_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2014_09/ENEMDU_PERSONAS_2014_09_hom/ENEMDU_PERSONAS_2014_09_hom.csv")
ENEMDU_PERSONAS_2014_12_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2014_12/ENEMDU_PERSONAS_2014_12_hom/ENEMDU_PERSONAS_2014_12_hom.csv")

# 2015
ENEMDU_PERSONAS_2015_03_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2015_03/ENEMDU_PERSONAS_2015_03_hom/ENEMDU_PERSONAS_2015_03_hom.csv")
ENEMDU_PERSONAS_2015_06_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2015_06/ENEMDU_PERSONAS_2015_06_hom/ENEMDU_PERSONAS_2015_06_hom.csv")
ENEMDU_PERSONAS_2015_09_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2015_09/ENEMDU_PERSONAS_2015_09_hom/ENEMDU_PERSONAS_2015_09_hom.csv")
ENEMDU_PERSONAS_2015_12_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2015_12/ENEMDU_PERSONAS_2015_12_hom/ENEMDU_PERSONAS_2015_12_hom.csv")

# 2016
ENEMDU_PERSONAS_2016_03_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2016_03/ENEMDU_PERSONAS_2016_03_hom/ENEMDU_PERSONAS_2016_03_hom.csv")
ENEMDU_PERSONAS_2016_06_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2016_06/ENEMDU_PERSONAS_2016_06_hom/ENEMDU_PERSONAS_2016_06_hom.csv")
ENEMDU_PERSONAS_2016_09_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2016_09/ENEMDU_PERSONAS_2016_09_hom/ENEMDU_PERSONAS_2016_09_hom.csv")
ENEMDU_PERSONAS_2016_12_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2016_12/ENEMDU_PERSONAS_2016_12_hom/ENEMDU_PERSONAS_2016_12_hom.csv")

# 2017
ENEMDU_PERSONAS_2017_03_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2017_03/ENEMDU_PERSONAS_2017_03_hom/ENEMDU_PERSONAS_2017_03_hom.csv")
ENEMDU_PERSONAS_2017_06_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2017_06/ENEMDU_PERSONAS_2017_06_hom/ENEMDU_PERSONAS_2017_06_hom.csv")
ENEMDU_PERSONAS_2017_09_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2017_09/ENEMDU_PERSONAS_2017_09_hom/ENEMDU_PERSONAS_2017_09_hom.csv")
ENEMDU_PERSONAS_2017_12_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2017_12/ENEMDU_PERSONAS_2017_12_hom/ENEMDU_PERSONAS_2017_12_hom.csv")

# 2018
ENEMDU_PERSONAS_2018_03_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2018_03/ENEMDU_PERSONAS_2018_03_hom/ENEMDU_PERSONAS_2018_03_hom.csv")
ENEMDU_PERSONAS_2018_06_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2018_06/ENEMDU_PERSONAS_2018_06_hom/ENEMDU_PERSONAS_2018_06_hom.csv")
ENEMDU_PERSONAS_2018_09_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2018_09/ENEMDU_PERSONAS_2018_09_hom/ENEMDU_PERSONAS_2018_09_hom.csv")
ENEMDU_PERSONAS_2018_12_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2018_12/ENEMDU_PERSONAS_2018_12_hom/ENEMDU_PERSONAS_2018_12_hom.csv")

# 2019
ENEMDU_PERSONAS_2019_03_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2019_03/ENEMDU_PERSONAS_2019_03_hom/ENEMDU_PERSONAS_2019_03_hom.csv")
ENEMDU_PERSONAS_2019_06_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2019_06/ENEMDU_PERSONAS_2019_06_hom/ENEMDU_PERSONAS_2019_06_hom.csv")
ENEMDU_PERSONAS_2019_09_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2019_09/enemdu_personas_2019_09.csv")
ENEMDU_PERSONAS_2019_12_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2019_12/enemdu_persona_201912.csv")


# 2021
ENEMDU_PERSONAS_2021_03_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2021_03/enemdu_persona_2021_03.csv")
ENEMDU_PERSONAS_2021_06_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2021_06/enemdu_persona_2021_06.csv")
ENEMDU_PERSONAS_2021_09_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2021_09/enemdu_persona_2021_09.csv")
ENEMDU_PERSONAS_2021_12_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2021_12/enemdu_persona_2021_12.csv")


# 2022
ENEMDU_PERSONAS_2022_03_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2022_03/enemdu_persona_2022_03.csv")
ENEMDU_PERSONAS_2022_06_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2022_06/enemdu_persona_2022_06.csv")
ENEMDU_PERSONAS_2022_09_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2022_09/enemdu_persona_2022_09.csv")
ENEMDU_PERSONAS_2022_12_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2022_12/enemdu_persona_2022_12.csv")

# 2023
ENEMDU_PERSONAS_2023_03_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2023_03/enemdu_persona_2023_03.csv")
ENEMDU_PERSONAS_2023_06_hom<-read.csv2("Encuestas trimestrales/ENEMDU_BDD_2023_06/enemdu_persona_2023_06.csv")


### Elimino las variables que no se usan en el modelo
# ENEMDU_2014<- ENEMDU_PERSONAS_2014_03_hom %>% select(periodo,area,ciudad,zona,sector,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp)


### Agrego un identificador de los periodos

ENEMDU_PERSONAS_2014_03_hom<- ENEMDU_PERSONAS_2014_03_hom %>% mutate(periodo = "2014T1")
ENEMDU_PERSONAS_2014_06_hom<- ENEMDU_PERSONAS_2014_06_hom %>% mutate(periodo = "2014T2")
ENEMDU_PERSONAS_2014_09_hom<- ENEMDU_PERSONAS_2014_09_hom %>% mutate(periodo = "2014T3")
ENEMDU_PERSONAS_2014_12_hom<- ENEMDU_PERSONAS_2014_12_hom %>% mutate(periodo = "2014T4")

ENEMDU_PERSONAS_2015_03_hom<- ENEMDU_PERSONAS_2015_03_hom %>% mutate(periodo = "2015T1")
ENEMDU_PERSONAS_2015_06_hom<- ENEMDU_PERSONAS_2015_06_hom %>% mutate(periodo = "2015T2")
ENEMDU_PERSONAS_2015_09_hom<- ENEMDU_PERSONAS_2015_09_hom %>% mutate(periodo = "2015T3")
ENEMDU_PERSONAS_2015_12_hom<- ENEMDU_PERSONAS_2015_12_hom %>% mutate(periodo = "2015T4")

ENEMDU_PERSONAS_2016_03_hom<- ENEMDU_PERSONAS_2016_03_hom %>% mutate(periodo = "2016T1")
ENEMDU_PERSONAS_2016_06_hom<- ENEMDU_PERSONAS_2016_06_hom %>% mutate(periodo = "2016T2")
ENEMDU_PERSONAS_2016_09_hom<- ENEMDU_PERSONAS_2016_09_hom %>% mutate(periodo = "2016T3")
ENEMDU_PERSONAS_2016_12_hom<- ENEMDU_PERSONAS_2016_12_hom %>% mutate(periodo = "2016T4")

ENEMDU_PERSONAS_2017_03_hom<- ENEMDU_PERSONAS_2017_03_hom %>% mutate(periodo = "2017T1")
ENEMDU_PERSONAS_2017_06_hom<- ENEMDU_PERSONAS_2017_06_hom %>% mutate(periodo = "2017T2")
ENEMDU_PERSONAS_2017_09_hom<- ENEMDU_PERSONAS_2017_09_hom %>% mutate(periodo = "2017T3")
ENEMDU_PERSONAS_2017_12_hom<- ENEMDU_PERSONAS_2017_12_hom %>% mutate(periodo = "2017T4")

ENEMDU_PERSONAS_2018_03_hom<- ENEMDU_PERSONAS_2018_03_hom %>% mutate(periodo = "2018T1")
ENEMDU_PERSONAS_2018_06_hom<- ENEMDU_PERSONAS_2018_06_hom %>% mutate(periodo = "2018T2")
ENEMDU_PERSONAS_2018_09_hom<- ENEMDU_PERSONAS_2018_09_hom %>% mutate(periodo = "2018T3")
ENEMDU_PERSONAS_2018_12_hom<- ENEMDU_PERSONAS_2018_12_hom %>% mutate(periodo = "2018T4")

ENEMDU_PERSONAS_2019_03_hom<- ENEMDU_PERSONAS_2019_03_hom %>% mutate(periodo = "2019T1")
ENEMDU_PERSONAS_2019_06_hom<- ENEMDU_PERSONAS_2019_06_hom %>% mutate(periodo = "2019T2")
ENEMDU_PERSONAS_2019_09_hom<- ENEMDU_PERSONAS_2019_09_hom %>% mutate(periodo = "2019T3")
ENEMDU_PERSONAS_2019_12_hom<- ENEMDU_PERSONAS_2019_12_hom %>% mutate(periodo = "2019T4")

ENEMDU_PERSONAS_2021_03_hom<- ENEMDU_PERSONAS_2021_03_hom %>% mutate(periodo = "2021T1")
ENEMDU_PERSONAS_2021_06_hom<- ENEMDU_PERSONAS_2021_06_hom %>% mutate(periodo = "2021T2")
ENEMDU_PERSONAS_2021_09_hom<- ENEMDU_PERSONAS_2021_09_hom %>% mutate(periodo = "2021T3")
ENEMDU_PERSONAS_2021_12_hom<- ENEMDU_PERSONAS_2021_12_hom %>% mutate(periodo = "2021T4")

ENEMDU_PERSONAS_2022_03_hom<- ENEMDU_PERSONAS_2022_03_hom %>% mutate(periodo = "2022T1")
ENEMDU_PERSONAS_2022_06_hom<- ENEMDU_PERSONAS_2022_06_hom %>% mutate(periodo = "2022T2")
ENEMDU_PERSONAS_2022_09_hom<- ENEMDU_PERSONAS_2022_09_hom %>% mutate(periodo = "2022T3")
ENEMDU_PERSONAS_2022_12_hom<- ENEMDU_PERSONAS_2022_12_hom %>% mutate(periodo = "2022T4")

ENEMDU_PERSONAS_2023_03_hom<- ENEMDU_PERSONAS_2023_03_hom %>% mutate(periodo = "2023T1")
ENEMDU_PERSONAS_2023_06_hom<- ENEMDU_PERSONAS_2023_06_hom %>% mutate(periodo = "2023T2")


### Elimino las variables que no se usan en el modelo

ENEMDU_PERSONAS_2014_03_hom<- ENEMDU_PERSONAS_2014_03_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2014_06_hom<- ENEMDU_PERSONAS_2014_06_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2014_09_hom<- ENEMDU_PERSONAS_2014_09_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2014_12_hom<- ENEMDU_PERSONAS_2014_12_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)

ENEMDU_PERSONAS_2015_03_hom<- ENEMDU_PERSONAS_2015_03_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2015_06_hom<- ENEMDU_PERSONAS_2015_06_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2015_09_hom<- ENEMDU_PERSONAS_2015_09_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2015_12_hom<- ENEMDU_PERSONAS_2015_12_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)

ENEMDU_PERSONAS_2016_03_hom<- ENEMDU_PERSONAS_2016_03_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2016_06_hom<- ENEMDU_PERSONAS_2016_06_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2016_09_hom<- ENEMDU_PERSONAS_2016_09_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2016_12_hom<- ENEMDU_PERSONAS_2016_12_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)

ENEMDU_PERSONAS_2017_03_hom<- ENEMDU_PERSONAS_2017_03_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2017_06_hom<- ENEMDU_PERSONAS_2017_06_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2017_09_hom<- ENEMDU_PERSONAS_2017_09_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2017_12_hom<- ENEMDU_PERSONAS_2017_12_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)

ENEMDU_PERSONAS_2018_03_hom<- ENEMDU_PERSONAS_2018_03_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2018_06_hom<- ENEMDU_PERSONAS_2018_06_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2018_09_hom<- ENEMDU_PERSONAS_2018_09_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2018_12_hom<- ENEMDU_PERSONAS_2018_12_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)

ENEMDU_PERSONAS_2019_03_hom<- ENEMDU_PERSONAS_2019_03_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2019_06_hom<- ENEMDU_PERSONAS_2019_06_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2019_09_hom<- ENEMDU_PERSONAS_2019_09_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2019_12_hom<- ENEMDU_PERSONAS_2019_12_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)

ENEMDU_PERSONAS_2021_03_hom<- ENEMDU_PERSONAS_2021_03_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2021_06_hom<- ENEMDU_PERSONAS_2021_06_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2021_09_hom<- ENEMDU_PERSONAS_2021_09_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2021_12_hom<- ENEMDU_PERSONAS_2021_12_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)

ENEMDU_PERSONAS_2022_03_hom<- ENEMDU_PERSONAS_2022_03_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2022_06_hom<- ENEMDU_PERSONAS_2022_06_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2022_09_hom<- ENEMDU_PERSONAS_2022_09_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2022_12_hom<- ENEMDU_PERSONAS_2022_12_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)

ENEMDU_PERSONAS_2023_03_hom<- ENEMDU_PERSONAS_2023_03_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)
ENEMDU_PERSONAS_2023_06_hom<- ENEMDU_PERSONAS_2023_06_hom %>% select(periodo,area,ciudad,estrato,panelm,vivienda,hogar,upm,id_vivienda,id_hogar,id_persona,fexp,p02,p03,p07,p10a,p10b,p11,p12a,p20,p32,p45,p75,nnivins,ingpc,empleo,desempleo,condact)

# Los unifico por año
ENEMDU_2014<-rbind(ENEMDU_PERSONAS_2014_03_hom,ENEMDU_PERSONAS_2014_06_hom,ENEMDU_PERSONAS_2014_09_hom,ENEMDU_PERSONAS_2014_12_hom)
ENEMDU_2015<-rbind(ENEMDU_PERSONAS_2015_03_hom,ENEMDU_PERSONAS_2015_06_hom,ENEMDU_PERSONAS_2015_09_hom,ENEMDU_PERSONAS_2015_12_hom)
ENEMDU_2016<-rbind(ENEMDU_PERSONAS_2016_03_hom,ENEMDU_PERSONAS_2016_06_hom,ENEMDU_PERSONAS_2016_09_hom,ENEMDU_PERSONAS_2016_12_hom)
ENEMDU_2017<-rbind(ENEMDU_PERSONAS_2017_03_hom,ENEMDU_PERSONAS_2017_06_hom,ENEMDU_PERSONAS_2017_09_hom,ENEMDU_PERSONAS_2017_12_hom)
ENEMDU_2018<-rbind(ENEMDU_PERSONAS_2018_03_hom,ENEMDU_PERSONAS_2018_06_hom,ENEMDU_PERSONAS_2018_09_hom,ENEMDU_PERSONAS_2018_12_hom)
ENEMDU_2019<-rbind(ENEMDU_PERSONAS_2019_03_hom,ENEMDU_PERSONAS_2019_06_hom,ENEMDU_PERSONAS_2019_09_hom,ENEMDU_PERSONAS_2019_12_hom)
ENEMDU_2021<-rbind(ENEMDU_PERSONAS_2021_03_hom,ENEMDU_PERSONAS_2021_06_hom,ENEMDU_PERSONAS_2021_09_hom,ENEMDU_PERSONAS_2021_12_hom)
ENEMDU_2022<-rbind(ENEMDU_PERSONAS_2022_03_hom,ENEMDU_PERSONAS_2022_06_hom,ENEMDU_PERSONAS_2022_09_hom,ENEMDU_PERSONAS_2022_12_hom)
ENEMDU_2023<-rbind(ENEMDU_PERSONAS_2023_03_hom,ENEMDU_PERSONAS_2023_06_hom)

### Unificar todos los años
ENEMDU<-as.data.table(rbind(ENEMDU_2014,ENEMDU_2015,ENEMDU_2016,ENEMDU_2017,ENEMDU_2018,ENEMDU_2019,ENEMDU_2021,ENEMDU_2022,ENEMDU_2023))

### En ciudad dejar unicamente los dos primeros digitos para tener un control por provincia.
ENEMDU<-ENEMDU[,prov:=obtener_prov(ciudad)]

# La base resultante se exporta en un csv
write.csv(ENEMDU, "ENEMDU_compilado.csv", row.names = FALSE)
