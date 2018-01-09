#=======================================================================================================
# SECCIÓN 0: ACTIVACIÓN DE LAS LIBRERÍAS A OCUPAR EN EL SCRIPTING

library(RPostgreSQL)
library(dplyr)
library(reshape2)
library(xlsx)
library(plyr)

# CONEXIÓN A POSTGRESQL:

senescyt_bi<-dbConnect("PostgreSQL", dbname="senescyt_bi",
                       host="localhost",port=5432,
                       user="postgres",password="postgres")

#=======================================================================================================
# SECCIÓN 1: LECTURA DE LAS BDD 


load("/home/marcelo/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/asignaciones_1.RData")

asignaciones_1<-as.data.frame(lapply(asignaciones_1,function(x) if(is.character(x))
                iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)


load("/home/marcelo/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/asignaciones_2.RData")

asignaciones_2<-as.data.frame(lapply(asignaciones_2,function(x) if(is.character(x))
                iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)

#=======================================================================================================

# SECCIÓN 2: CÁLCULO DE LAS ASIGNACIONES DE CUPO DEL PER 2 AL PER 12

asigna_p2_p12<-asignaciones_1 %>%
               filter(ccp_estado=="A") %>% 
               mutate(nvc_acepta_carrera=as.character(as.factor(nvc_acepta_carrera))) %>% 
               mutate(nvc_acepta_carrera=recode(nvc_acepta_carrera,
                                                "S"="SI",
                                                "N"="NO")) %>% 
               mutate(nvc_acepta_carrera=ifelse(is.na(nvc_acepta_carrera),"NO SE PRONUNCIA",
                                               nvc_acepta_carrera)) %>% 
               select(usu_id,provincia_campus,canton_campus,
                      parroquia_campus,ccp_cupos_reales,
                      nvc_acepta_carrera,ies_nombre_instit,per_id,
                      car_nombre_carrea,ies_tipo_ies,area_nombre) %>% 
               plyr::rename(c("car_nombre_carrea"="car_nombre_carrera"))


reporte_asigna_p2_p12<-dcast(asigna_p2_p12,
                             asigna_p2_p12$per_id+
                             asigna_p2_p12$provincia_campus+
                             asigna_p2_p12$canton_campus+
                             asigna_p2_p12$parroquia_campus+
                             asigna_p2_p12$ies_nombre_instit+
                             asigna_p2_p12$ies_tipo_ies+
                             asigna_p2_p12$area_nombre+   
                             asigna_p2_p12$car_nombre_carrera~
                             asigna_p2_p12$nvc_acepta_carrera)

names(reporte_asigna_p2_p12)<-c("per_id","provincia_campus","canton_campus","parroquia_campus",
                                "ies_nombre_instit","ies_tipo_ies","area","car_nombre_carrera","NO",
                                "NO SE PRONUNCIA","SI")

#=======================================================================================================

# SECCIÓN 3: CÁLCULO DE LA OFERTA DEL PER 2 AL PER 12:


oferta_p2_p12<-asignaciones_1 %>%
               filter(ccp_estado=="A") %>%
               select(ccp_id,provincia_campus,canton_campus,parroquia_campus,
                      parroquia_campus,ccp_cupos_reales,ies_nombre_instit,per_id,
                      car_nombre_carrea,area_nombre,ies_tipo_ies) %>%
               distinct() %>%
               group_by(per_id,provincia_campus,canton_campus,parroquia_campus,ies_nombre_instit,
                        ies_tipo_ies,area_nombre,car_nombre_carrea) %>%
               summarise_each(funs(sum),ccp_cupos_reales) %>%
               plyr::rename(c("car_nombre_carrea"="car_nombre_carrera",
                              "ccp_cupos_reales"="total_cupos")) %>% 
               filter(total_cupos!="NA") %>% 
               #La sentencia siguiente la tengo que añadir porque presenta errores el tipo 
               #factor del campo area y la única solución es crear un variable area de tipo
               #de tipo caracter
               mutate(area=as.character(as.factor(area_nombre)))
               
# La siguiente línea nos ayuda a forzar la eliminación de la variable area_nombre:
oferta_p2_p12<-oferta_p2_p12[c(-7)]

#=======================================================================================================
# SECCIÓN 4: DIVISIÓN Y JOIN DE BDD DE ASIGNACIONES Y DE OFERTA DEL PER 2 AL PER 12

reporte_1<-merge(reporte_asigna_p2_p12,oferta_p2_p12,by=c("per_id","provincia_campus",
                                                          "canton_campus","parroquia_campus",
                                                          "ies_nombre_instit","ies_tipo_ies",
                                                          "area","car_nombre_carrera"),all=F)

# Esta parte del script sirve para corregir en el caso de cupos ofertados igual a 0
reporte_1$total_cupos<-ifelse(reporte_1$total_cupos==0,
                              rowSums(reporte_1[,9:11]),
                              reporte_1$total_cupos)

#=======================================================================================================
# SECCIÓN 5: CÁLCULO DE LAS ASIGNACIONES DEL PER 13 Y 14

oferta_p13_p14<-asignaciones_2 %>% 
                select(usu_id,provincia_campus,canton_campus,parroquia_campus,ies_nombre_instit,
                       ies_tipo_ies,area,car_nombre_carrera,per_id,asa_acepta_cupo) %>% 
                mutate(asa_acepta_cupo=recode(asa_acepta_cupo,"0"="NO SE PRONUNCIA","1"="SI",
                                              "2"="NO"))

reporte_asigna_p13_p14<-dcast(oferta_p13_p14,
                              oferta_p13_p14$per_id+
                              oferta_p13_p14$provincia_campus+
                              oferta_p13_p14$canton_campus+
                              oferta_p13_p14$parroquia_campus+  
                              oferta_p13_p14$ies_nombre_instit+
                              oferta_p13_p14$ies_tipo_ies+
                              oferta_p13_p14$area+  
                              oferta_p13_p14$car_nombre_carrera~
                              oferta_p13_p14$asa_acepta_cupo)

names(reporte_asigna_p13_p14)<-c("per_id","provincia_campus","canton_campus",
                                 "parroquia_campus","ies_nombre_instit","ies_tipo_ies",
                                 "area","car_nombre_carrera","NO","NO SE PRONUNCIA","SI")

oferta_p13_p14<-asignaciones_2 %>% 
                select(ofa_id,provincia_campus,canton_campus,parroquia_campus,
                       ies_nombre_instit,ies_tipo_ies,area,car_nombre_carrera,
                       ccp_cupos_nivelacion,ccp_cupos_primer_semestre,ccp_cupos_exoneracion,
                       per_id) %>% 
                mutate(total_cupos=rowSums(.[9:11])) %>% 
                distinct() %>% 
                select(per_id,provincia_campus,canton_campus,parroquia_campus,ies_nombre_instit,
                       ies_tipo_ies,area,car_nombre_carrera,total_cupos)

reporte_2<-merge(oferta_p13_p14,reporte_asigna_p13_p14,by=c("per_id","provincia_campus",
                 "canton_campus","parroquia_campus","ies_nombre_instit","ies_tipo_ies",
                 "area","car_nombre_carrera"),all=F)

# FINALMENTE EN LA SIGUIENTE LÍNEA UNIFICO LAS BDD DE LAS ASIGNACIONES Y OFETA DEL PER 2 AL 14
reporte_oferta_asignacion<-rbind(reporte_1,reporte_2)

# La siguiente línea encuentra las asignaciones de cupo:

reporte_oferta_asignacion<-reporte_oferta_asignacion %>% 
                           mutate(asignaciones=reporte_oferta_asignacion$NO+
                                               reporte_oferta_asignacion$SI+
                                               reporte_oferta_asignacion$`NO SE PRONUNCIA`)

reporte_oferta_asignacion$periodos<-recode(reporte_oferta_asignacion$per_id,
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

reporte_oferta_asignacion$periodos<-factor(reporte_oferta_asignacion$periodos,
                                          levels=c("1er. Semestre 2012",
                                                   "2do. Semestre 2012",
                                                   "1er. Semestre 2013",
                                                   "2do. Semestre 2013",
                                                   "1er. Semestre 2014",
                                                   "2do. Semestre 2014",
                                                   "1er. Semestre 2015",
                                                   "2do. Semestre 2015",
                                                   "1er. Semestre 2016",
                                                   "2do. Semestre 2016",
                                                   "1er. Semestre 2017",
                                                   "2do. Semestre 2017"))

#=======================================================================================================
