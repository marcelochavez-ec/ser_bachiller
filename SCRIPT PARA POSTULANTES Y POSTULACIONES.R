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
library(sparklyr)

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

# PERIODOS DEL P2 AL P12:

# POSTULANTES:

postulantes_p2_p12<-postulaciones_1 %>% 
                    filter(oca_orden_prioridad==1 & oca_repostulacion==0) %>% 
                    select(usu_id,per_id,ies_nombre_instit,provincia_ies,
                           car_nombre_carrea) %>% 
                    mutate(contador=1) %>% 
                    distinct()

postulantes_p2_p12<-postulantes_p2_p12 %>% 
                    group_by(per_id,ies_nombre_instit,provincia_ies,car_nombre_carrea) %>% 
                    summarise(postulantes=sum(contador))

postulantes_p2_p12$periodos<-recode(postulantes_p2_p12$per_id,
                             "2"="1er. Semestre 2012",
                             "3"="2do. Semestre 2012",
                             "4"="1er. Semestre 2013",
                             "5"="2do. Semestre 2013",
                             "6"="1er. Semestre 2014",
                             "7"="2do. Semestre 2014",
                             "8"="1er. Semestre 2015",
                             "9"="2do. Semestre 2015",
                             "10"="1er. Semestre 2016",
                             "12"="2do. Semestre 2016")


postulantes_p2_p12$periodos<-factor(postulantes_p2_p12$periodos,
                                    levels=c("1er. Semestre 2012",
                                             "2do. Semestre 2012",
                                             "1er. Semestre 2013",
                                             "2do. Semestre 2013",
                                             "1er. Semestre 2014",
                                             "2do. Semestre 2014",
                                             "1er. Semestre 2015",
                                             "2do. Semestre 2015",
                                             "1er. Semestre 2016",
                                             "2do. Semestre 2016"))

# POSTULACIONES:

postulaciones_p2_p12<-postulaciones_1 %>% 
                      filter(oca_orden_prioridad<=5 & oca_repostulacion==0) %>% 
                      select(usu_id,per_id,ies_nombre_instit,provincia_ies,
                             car_nombre_carrea) %>% 
                      mutate(contador=1)

postulaciones_p2_p12<-postulaciones_p2_p12 %>% 
                      group_by(per_id,provincia_ies,ies_nombre_instit,
                               car_nombre_carrea) %>% 
                      summarise(postulaciones=sum(contador))

postulaciones_p2_p12$periodos<-recode(postulaciones_p2_p12$per_id,
                                    "2"="1er. Semestre 2012",
                                    "3"="2do. Semestre 2012",
                                    "4"="1er. Semestre 2013",
                                    "5"="2do. Semestre 2013",
                                    "6"="1er. Semestre 2014",
                                    "7"="2do. Semestre 2014",
                                    "8"="1er. Semestre 2015",
                                    "9"="2do. Semestre 2015",
                                    "10"="1er. Semestre 2016",
                                    "12"="2do. Semestre 2016")


postulaciones_p2_p12$periodos<-factor(postulaciones_p2_p12$periodos,
                                    levels=c("1er. Semestre 2012",
                                             "2do. Semestre 2012",
                                             "1er. Semestre 2013",
                                             "2do. Semestre 2013",
                                             "1er. Semestre 2014",
                                             "2do. Semestre 2014",
                                             "1er. Semestre 2015",
                                             "2do. Semestre 2015",
                                             "1er. Semestre 2016",
                                             "2do. Semestre 2016"))

#========================================================================================================

# POSTULACIONES PER 13:

# POSTULANTES p13:

postulantes_p13<-postulaciones_2 %>% 
                 filter(per_id==13 & pos_prioridad==1) %>% 
                 select(usu_id,per_id,ies_nombre_instit,provincia_campus,
                        car_nombre_carrera,prd_id_num_postulacion,pos_prioridad) %>% 
                 arrange(usu_id,prd_id_num_postulacion,pos_prioridad)

usu_id_post_p13<-postulantes_p13 %>% select(usu_id)
usu_id_post_p13<-usu_id_post_p13[order(usu_id_post_p13$usu_id),]
usu_id_post_p13<-data.frame(ID=usu_id_post_p13)
usu_id_post_p13_a<-split(usu_id_post_p13,f=usu_id_post_p13$ID)

df.c <- lapply(usu_id_post_p13_a, function( x ){ sai <- cbind(x, contador= 1:nrow(x)); sai })
df.c.c <- do.call( rbind, df.c )

postulantes_p13<-cbind(df.c.c,postulantes_p13)

postulantes_p13<-postulantes_p13 %>% filter(contador==1)

postulantes_p13_r<-postulantes_p13 %>% 
                 select(contador,provincia_campus,ies_nombre_instit,
                        car_nombre_carrera,per_id) %>% 
                 group_by(per_id,provincia_campus,ies_nombre_instit,
                          car_nombre_carrera) %>% 
                 summarise(postulantes=sum(contador)) %>% 
                 mutate(periodos="1er. Semestre 2017") %>% 
                 plyr::rename(c("provincia_campus"="provincia_ies",
                                "car_nombre_carrera"="car_nombre_carrea"))


# POSTULACIONES P13:

postulaciones_p13<-postulaciones_2 %>% 
                   filter(per_id==13 & pos_prioridad<=5) %>% 
                   select(usu_id,per_id,ies_nombre_instit,provincia_campus,
                          car_nombre_carrera,prd_id_num_postulacion,pos_prioridad) %>% 
                   arrange(usu_id,prd_id_num_postulacion,pos_prioridad)


usu_id_post_p13<-postulaciones_p13 %>% select(usu_id)
usu_id_post_p13<-usu_id_post_p13[order(usu_id_post_p13$usu_id),]
usu_id_post_p13<-data.frame(ID=usu_id_post_p13)
usu_id_post_p13_a<-split(usu_id_post_p13,f=usu_id_post_p13$ID)

df.c <- lapply(usu_id_post_p13_a, function( x ){ sai <- cbind(x, contador= 1:nrow(x)); sai })
df.c.c <- do.call( rbind, df.c )

postulaciones_p13<-cbind(df.c.c,postulaciones_p13)

postulaciones_p13<-postulaciones_p13 %>% filter(contador==1)

postulaciones_p13_r<-postulaciones_p13 %>% 
                     select(contador,provincia_campus,ies_nombre_instit,
                            car_nombre_carrera,per_id) %>% 
                     group_by(per_id,provincia_campus,ies_nombre_instit,
                              car_nombre_carrera) %>% 
                     summarise(postulaciones=sum(contador)) %>% 
                     mutate(periodos="1er. Semestre 2017")
  

#===========================================================================================================

# POSTULANTES P14:


postulantes_p14<-postulaciones_2 %>% 
                 filter(per_id==14 & pos_estado_new==1 & prd_id_num_postulacion==446 &
                        pos_prioridad==1) %>% 
                 select(usu_id,per_id,ies_nombre_instit,provincia_campus,car_nombre_carrera) %>% 
                 mutate(contador=1)


postulantes_p14<-postulantes_p14 %>% 
                 group_by(per_id,ies_nombre_instit,provincia_campus,car_nombre_carrera) %>% 
                 summarise(postulantes=sum(contador)) %>% 
                 mutate(periodos="2do. Semestre 2017") %>% 
                 plyr::rename(c("provincia_campus"="provincia_ies",
                                "car_nombre_carrera"="car_nombre_carrea"))

#POSTULACIONES P14:

postulaciones_p14<-postulaciones_2 %>% 
                   filter(per_id==14 & pos_estado_new==1 & prd_id_num_postulacion==446 &
                          pos_prioridad<=5) %>% 
                   select(usu_id,per_id,ies_nombre_instit,provincia_campus,car_nombre_carrera) %>% 
                   mutate(contador=1)

postulaciones_p14<-postulaciones_p14 %>% 
                   group_by(per_id,ies_nombre_instit,provincia_campus,car_nombre_carrera) %>% 
                   summarise(postulaciones=sum(contador)) %>% 
                   mutate(periodos="2do. Semestre 2017") %>% 
                   plyr::rename(c("provincia_campus"="provincia_ies",
                                 "car_nombre_carrera"="car_nombre_carrea"))

postulantes_p2_p12$periodos<-as.character(as.factor(postulantes_p2_p12$periodos))
postulantes_p2_p12$ies_nombre_instit<-as.character(as.factor(postulantes_p2_p12$ies_nombre_instit))
postulantes_p2_p12$car_nombre_carrea<-as.character(as.factor(postulantes_p2_p12$car_nombre_carrea))
postulantes_p2_p12$provincia_ies<-as.character(as.factor(postulantes_p2_p12$provincia_ies))

postulantes_p13_r$periodos<-as.character(as.factor(postulantes_p13_r$periodos))
postulantes_p13_r$ies_nombre_instit<-as.character(as.factor(postulantes_p13_r$ies_nombre_instit))
postulantes_p13_r$car_nombre_carrea<-as.character(as.factor(postulantes_p13_r$car_nombre_carrea))
postulantes_p13_r$provincia_ies<-as.character(as.factor(postulantes_p13_r$provincia_ies))


postulantes_p14$periodos<-as.character(as.factor(postulantes_p14$periodos))
postulantes_p14$ies_nombre_instit<-as.character(as.factor(postulantes_p14$ies_nombre_instit))
postulantes_p14$car_nombre_carrea<-as.character(as.factor(postulantes_p14$car_nombre_carrea))
postulantes_p14$provincia_ies<-as.character(as.factor(postulantes_p14$provincia_ies))

postulantes_p2_p14<-rbind(postulantes_p2_p12,postulantes_p13_r,postulantes_p14)


postulaciones_p2_p12$periodos<-as.character(as.factor(postulaciones_p2_p12$periodos))
postulaciones_p2_p12$ies_nombre_instit<-as.character(as.factor(postulaciones_p2_p12$ies_nombre_instit))
postulaciones_p2_p12$car_nombre_carrea<-as.character(as.factor(postulaciones_p2_p12$car_nombre_carrea))
postulaciones_p2_p12$provincia_ies<-as.character(as.factor(postulaciones_p2_p12$provincia_ies))


postulaciones_p13_r$periodos<-as.character(as.factor(postulaciones_p13_r$periodos))
postulaciones_p13_r$ies_nombre_instit<-as.character(as.factor(postulaciones_p13_r$ies_nombre_instit))
postulaciones_p13_r$car_nombre_carrera<-as.character(as.factor(postulaciones_p13_r$car_nombre_carrera))
postulaciones_p13_r$provincia_campus<-as.character(as.factor(postulaciones_p13_r$provincia_campus))

postulaciones_p14$periodos<-as.character(as.factor(postulaciones_p14$periodos))
postulaciones_p14$ies_nombre_instit<-as.character(as.factor(postulaciones_p14$ies_nombre_instit))
postulaciones_p14$car_nombre_carrea<-as.character(as.factor(postulaciones_p14$car_nombre_carrea))
postulaciones_p14$provincia_ies<-as.character(as.factor(postulaciones_p14$provincia_ies))



postulaciones_p2_p14<-rbind(postulaciones_p2_p12,postulaciones_p13_r,postulaciones_p14)










