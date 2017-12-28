#--------------------------------------------------------------------------------------------------------------------------
# PROCESO DE POSTULACIÓN:
# PAQUETES:
library(RPostgreSQL)
library(dplyr)
library(reshape2)
library(xlsx)
library(tidyr)
library(sqldf)
library(dbplyr)
library(plyr)
library(lubridate)
library(data.table)

age<-function(dob,age.day=today(),units="years",floor=TRUE) 
{
  calc.age = new_interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

senescyt_bi<-dbConnect(dbDriver("PostgreSQL"), user="postgres",password="postgres",
                       dbname="senescyt_bi",host="localhost")


load("c:/Users/rchavez/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/inscritos_1.RData")
load("c:/Users/rchavez/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/inscritos_2.RData")
load("c:/Users/rchavez/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/inscritos_3.RData")


load("c:/Users/rchavez/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/postulaciones_2.RData")

ins_p14<-inscritos_3 %>%
         filter(ins_sede_rec_asignado!="NA" & per_id==14) %>% 
         mutate(edad=age(fecha_nacimiento)) %>%        
         select(usu_id,edad) %>% 
         filter(edad>=25)

postulaciones_2<-as.data.frame(lapply(postulaciones_2, function(x) if(is.character(x))
                 iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)

postulaciones_2$prd_descripcion<-trimws(postulaciones_2$prd_descripcion)


post_p14<-postulaciones_2 %>% 
          filter(pos_estado_new==1 & per_id==14 & prd_descripcion %in% c("PRIMERA POSTULACION",
                                                                         "SEGUNDA POSTULACIÓN",
                                                                         "TERCERA POSTULACION",
                                                                         "SEXTA POSTULACION") & pos_prioridad==1) %>% 
          select(usu_id,car_nombre_carrera,provincia_campus) %>% 
          distinct()

# POSTULANTES A NIVEL NACIONAL:
postulantes_p14<-data.frame(table(post_p14$usu_id))


#POSTULANTES POR INSCRITOS:

post_p14_ins<-merge(ins_p14,post_p14,all=F)

#=========================================================================================================================
# POSTULANTES POR PROVINCIA:

provincias<-split(post_p14_ins,post_p14_ins$provincia_campus)

azuay<-data.frame(provincias$AZUAY)
azuay<-data.frame(table(azuay$car_nombre_carrera))

azuay<-azuay %>% 
       arrange(-Freq) %>% 
       head(10)


bolivar<-data.frame(provincias$BOLIVAR)

bolivar<-data.frame(table(bolivar$car_nombre_carrera))

bolivar<-bolivar %>% 
  arrange(-Freq) %>% 
  head(10)

caniar<-data.frame(provincias$CAÑAR)

caniar<-data.frame(table(caniar$car_nombre_carrera))

caniar<-caniar %>% 
  arrange(-Freq) %>% 
  head(10)

carchi<-data.frame(provincias$CARCHI)

carchi<-data.frame(table(carchi$car_nombre_carrera))

carchi<-carchi %>% 
  arrange(-Freq) %>% 
  head(10)

chimborazo<-data.frame(provincias$CHIMBORAZO)

chimborazo<-data.frame(table(chimborazo$car_nombre_carrera))

chimborazo<-chimborazo %>% 
  arrange(-Freq) %>% 
  head(10)

cotopaxi<-data.frame(provincias$COTOPAXI)

cotopaxi<-data.frame(table(cotopaxi$car_nombre_carrera))

cotopaxi<-cotopaxi %>% 
  arrange(-Freq) %>% 
  head(10)

el_oro<-data.frame(provincias$`EL ORO`)

el_oro<-data.frame(table(el_oro$car_nombre_carrera))

el_oro<-el_oro %>% 
  arrange(-Freq) %>% 
  head(10)

esmeraldas<-data.frame(provincias$ESMERALDAS)

esmeraldas<-data.frame(table(esmeraldas$car_nombre_carrera))

esmeraldas<-esmeraldas %>% 
  arrange(-Freq) %>% 
  head(10)

guayas<-data.frame(provincias$GUAYAS)

guayas<-data.frame(table(guayas$car_nombre_carrera))

guayas<-guayas %>% 
  arrange(-Freq) %>% 
  head(10)

imbabura<-data.frame(provincias$IMBABURA)

imbabura<-data.frame(table(imbabura$car_nombre_carrera))

imbabura<-imbabura %>% 
  arrange(-Freq) %>% 
  head(10)

loja<-data.frame(provincias$LOJA)

loja<-data.frame(table(loja$car_nombre_carrera))

loja<-loja %>% 
  arrange(-Freq) %>% 
  head(10)

los_rios<-data.frame(provincias$`LOS RIOS`)

los_rios<-data.frame(table(los_rios$car_nombre_carrera))

los_rios<-los_rios %>% 
  arrange(-Freq) %>% 
  head(10)

manabi<-data.frame(provincias$MANABI)

manabi<-data.frame(table(manabi$car_nombre_carrera))

manabi<-manabi %>% 
  arrange(-Freq) %>% 
  head(10)

morona_santiago<-data.frame(provincias$`MORONA SANTIAGO`)

morona_santiago<-data.frame(table(morona_santiago$car_nombre_carrera))

morona_santiago<-morona_santiago %>% 
  arrange(-Freq) %>% 
  head(10)

napo<-data.frame(provincias$NAPO)

napo<-data.frame(table(napo$car_nombre_carrera))

napo<-napo %>% 
  arrange(-Freq) %>% 
  head(10)

orellana<-data.frame(provincias$ORELLANA)

orellana<-data.frame(table(orellana$car_nombre_carrera))

orellana<-orellana %>% 
  arrange(-Freq) %>% 
  head(10)

pastaza<-data.frame(provincias$PASTAZA)

pastaza<-data.frame(table(pastaza$car_nombre_carrera))

pastaza<-pastaza %>% 
  arrange(-Freq) %>% 
  head(10)


pichincha<-data.frame(provincias$PICHINCHA)

pichincha<-data.frame(table(pichincha$car_nombre_carrera))

pichincha<-pichincha %>% 
  arrange(-Freq) %>% 
  head(10)


santa_elena<-data.frame(provincias$`SANTA ELENA`)

santa_elena<-data.frame(table(santa_elena$car_nombre_carrera))

santa_elena<-santa_elena %>% 
  arrange(-Freq) %>% 
  head(10)

sto_domingo<-data.frame(provincias$`SANTO DOMINGO DE LOS TSACHILAS`)

sto_domingo<-data.frame(table(sto_domingo$car_nombre_carrera))

sto_domingo<-sto_domingo %>% 
  arrange(-Freq) %>% 
  head(10)

sucumbios<-data.frame(provincias$SUCUMBIOS)

sucumbios<-data.frame(table(sucumbios$car_nombre_carrera))

sucumbios<-sucumbios %>% 
  arrange(-Freq) %>% 
  head(10)

tungurahua<-data.frame(provincias$TUNGURAHUA)

tungurahua<-data.frame(table(tungurahua$car_nombre_carrera))

tungurahua<-tungurahua %>% 
  arrange(-Freq) %>% 
  head(10)

zamora<-data.frame(provincias$`ZAMORA CHINCHIPE`)

zamora<-data.frame(table(zamora$car_nombre_carrera))

zamora<-zamora %>% 
  arrange(-Freq) %>% 
  head(10)


#=========================================================================================================================

nacional<-data.frame(table(post_p14_ins$car_nombre_carrera))

nacional<-nacional %>% 
          arrange(-Freq) %>% 
          head(10)

#=========================================================================================================================
# REPORTES EXCEL:

wb = createWorkbook()

sheet = createSheet(wb,"POSTULANTES")

addDataFrame(azuay,sheet=sheet,startColumn=2,
             startRow = 10,row.names=FALSE)

addDataFrame(bolivar,sheet=sheet,startColumn=2,
             startRow = 22,row.names=FALSE)


addDataFrame(caniar,sheet=sheet,startColumn=2,
             startRow = 34,row.names=FALSE)

addDataFrame(carchi,sheet=sheet,startColumn=2,
             startRow = 46,row.names=FALSE)

addDataFrame(chimborazo,sheet=sheet,startColumn=2,
             startRow = 58,row.names=FALSE)

addDataFrame(cotopaxi,sheet=sheet,startColumn=2,
             startRow = 70,row.names=FALSE)

addDataFrame(el_oro,sheet=sheet,startColumn=2,
             startRow = 82,row.names=FALSE)

addDataFrame(esmeraldas,sheet=sheet,startColumn=2,
             startRow = 94,row.names=FALSE)

addDataFrame(guayas,sheet=sheet,startColumn=2,
             startRow = 106,row.names=FALSE)

addDataFrame(imbabura,sheet=sheet,startColumn=2,
             startRow = 118,row.names=FALSE)

addDataFrame(loja,sheet=sheet,startColumn=2,
             startRow = 130,row.names=FALSE)

addDataFrame(los_rios,sheet=sheet,startColumn=2,
             startRow = 142,row.names=FALSE)

addDataFrame(manabi,sheet=sheet,startColumn=2,
             startRow = 154,row.names=FALSE)

addDataFrame(morona_santiago,sheet=sheet,startColumn=2,
             startRow = 166,row.names=FALSE)

addDataFrame(napo,sheet=sheet,startColumn=2,
             startRow = 178,row.names=FALSE)

addDataFrame(orellana,sheet=sheet,startColumn=2,
             startRow = 190,row.names=FALSE)

addDataFrame(pastaza,sheet=sheet,startColumn=2,
             startRow = 202,row.names=FALSE)

addDataFrame(pichincha,sheet=sheet,startColumn=2,
             startRow = 214,row.names=FALSE)

addDataFrame(santa_elena,sheet=sheet,startColumn=2,
             startRow = 226,row.names=FALSE)

addDataFrame(sto_domingo,sheet=sheet,startColumn=2,
             startRow = 238,row.names=FALSE)

addDataFrame(sucumbios,sheet=sheet,startColumn=2,
             startRow = 250,row.names=FALSE)

addDataFrame(tungurahua,sheet=sheet,startColumn=2,
             startRow = 262,row.names=FALSE)

addDataFrame(zamora,sheet=sheet,startColumn=2,
             startRow = 274,row.names=FALSE)

saveWorkbook(wb,"c:/Users/rchavez/Documents/PROYECTO_BI_SENESCYT_2/REPORTES EXCEL 1/POSTULANTES MAYORES A 25-CGI - NEW.xlsx")



























