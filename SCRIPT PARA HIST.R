#--------------------------------------------------------------------------------------------------------------------------

# PROCESO DE POSTULACIÓN:

# PAQUETES:
library(RPostgreSQL)
library(dplyr)
library(reshape2)
library(xlsx)
library(tidyr)
library(sqldf)
library(plyr)

# CONEXIÓN A POSTGRESQL:

senescyt_bi<-dbConnect("PostgreSQL", dbname="senescyt_bi",
                       host="localhost",port=5432,
                       user="postgres",password="postgres")


load("/home/marcelo/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/postulaciones_1.RData")
load("/home/marcelo/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/postulaciones_2.RData")


postulaciones_1<-as.data.frame(lapply(postulaciones_1,function(x) if(is.character(x))
  iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)

postulaciones_2<-as.data.frame(lapply(postulaciones_2,function(x) if(is.character(x))
  iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)


# OFERTA ACADÉMICA DEL SNNA:
variables_post1<-data.frame(names(postulaciones_1))
variables_post2<-data.frame(names(postulaciones_2))

oferta_p2_p12<-postulaciones_1 %>% 
          filter(oca_repostulacion==0 & oca_orden_prioridad<=5) %>% 
          select(per_id,provincia_ies,canton_ies,ies_nombre_instit,
                 ies_tipo_ies,ies_tipo_financiamiento,area,subarea,car_nombre_carrea,
                 modalidad,ras_nota_postula) %>% 
          group_by(per_id,provincia_ies,canton_ies,ies_nombre_instit,
                   ies_tipo_ies,ies_tipo_financiamiento,area,subarea,car_nombre_carrea,
                   modalidad) %>%
          summarise_each(funs(max, min, mean),ras_nota_postula) %>% 
          plyr::rename(c("car_nombre_carrea"="car_nombre_carrera","provincia_ies"="provincia_campus",
                         "canton_ies"="ciudad_campus","area"="area_nombre","subarea"="subarea_nombre",
                         "ras_nota_postula_max"="nota_postula_max",
                         "ras_nota_postula_min"="nota_postula_min",
                         "ras_nota_postula_mean"="nota_postula_mean"))
          
oferta_p13<-postulaciones_2 %>% 
          filter(per_id == 13 & pos_prioridad <= 5) %>% 
          select(per_id,provincia_campus,ciudad_campus,ies_nombre_instit,
                 ies_tipo_ies,ies_tipo_financiamiento,area_nombre,subarea_nombre,car_nombre_carrera,
                 modalidad,nota_postula) %>% 
          group_by(per_id,provincia_campus,ciudad_campus,ies_nombre_instit,
                   ies_tipo_ies,ies_tipo_financiamiento,area_nombre,subarea_nombre,car_nombre_carrera,
                   modalidad) %>% 
          summarise_each(funs(max, min, mean), nota_postula)

oferta_p14<-postulaciones_2 %>% 
  filter(per_id == 14 & pos_estado_new == 1 & prd_id_num_postulacion == 446 & pos_prioridad <= 5) %>% 
  select(per_id,provincia_campus,ciudad_campus,ies_nombre_instit,
         ies_tipo_ies,ies_tipo_financiamiento,area_nombre,subarea_nombre,car_nombre_carrera,
         modalidad,nota_postula) %>% 
  group_by(per_id,provincia_campus,ciudad_campus,ies_nombre_instit,
           ies_tipo_ies,ies_tipo_financiamiento,area_nombre,subarea_nombre,car_nombre_carrera,
           modalidad) %>% 
  summarise_each(funs(max, min, mean), nota_postula)


oferta<-list(data.frame(oferta_p2_p12), 
             data.frame(oferta_p13),
             data.frame(oferta_p14))

oferta<-do.call(rbind,oferta)


oferta$per_id<-recode(oferta$per_id,
                                    "2"="1er. Semestre 2012",
                                    "3"="2do. Semestre 2012",
                                    "4"="1er. Semestre 2013",
                                    "5"="2do. Semestre 2013",
                                    "6"="1er. Semestre 2014",
                                    "7"="2do. Semestre 2014",
                                    "8"="1er. Semestre 2015",
                                    "9"="2do. Semestre 2015",
                                    "10"="1er. Semestre 2016",
                                    "12"="2do. Semestre 2016",
                                    "13"="1er. Semestre 2017",
                                    "14"="2do. Semestre 2017")










