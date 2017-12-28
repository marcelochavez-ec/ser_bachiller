post_1_p14<-postulaciones_2 %>% 
  filter(per_id==14 & prd_descripcion %in% c("PRIMERA POSTULACION",
                                             "SEGUNDA POSTULACIÓN",
                                             "TERCERA POSTULACION",
                                             "SEXTA POSTULACION")
         & pos_estado_new==1 & provincia_campus %in% c("SUCUMBIOS","NAPO","ORELLANA","PASTAZA",
                                                       "MORONA SANTIAGO","ZAMORA CHINCHIPE")) %>% 
  select(car_nombre_carrera,provincia_campus) %>% 
  mutate(contador=1)


load("/home/marcelo/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/postulaciones_1.RData")
load("/home/marcelo/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/postulaciones_2.RData")


postulaciones_1<-as.data.frame(lapply(postulaciones_1,function(x) if(is.character(x))
                 iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)

postulaciones_2<-as.data.frame(lapply(postulaciones_2,function(x) if(is.character(x))
                 iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)


#---------------------------------------------------------------------------------------------------

# PERIODO P10:

# POSTULANTES:

postulantes_p10<-postulaciones_1 %>% 
                 filter(oca_orden_prioridad==1 & per_id==10) %>% 
                 select(per_id,ies_nombre_instit,provincia_ies,
                        car_nombre_carrea) %>% 
                 mutate(contador=1) %>% 
                 distinct()
library(rpivotTable)

rpivotTable(postulantes_p10)

postulantes_p10_reporte<-postulantes_p10 %>% 
                         group_by(ies_nombre_instit,provincia_ies,car_nombre_carrea) %>% 
                         summarise(postulantes=sum(contador))

# POSTULACIONES:

postulaciones_p10<-postulaciones_1 %>% 
                   filter(oca_orden_prioridad<=5 & oca_repostulacion==1 & per_id==10) %>% 
                   select(usu_id,ies_nombre_instit,provincia_ies,
                          car_nombre_carrea,car_nombre_carrea) %>% 
                   mutate(contador=1)
 

postulaciones_p10<-postulaciones_p10 %>% 
                   group_by(provincia_ies,ies_nombre_instit,car_nombre_carrea) %>% 
                   summarise(postulaciones=sum(contador))

# PERIODO P13:

# POSTULANTES:

postulaciones_2$prd_descripcion<-trimws(postulaciones_2$prd_descripcion)

#post_p13<-postulaciones_2 %>% filter(per_id==13 & prd_descripcion=="PRIMERA POSTULACION")

postulantes_p13<-postulaciones_2 %>% 
                 filter(per_id==13 & pos_prioridad==1) %>% 
                 select(usu_id,per_id,ies_nombre_instit,provincia_campus,
                        car_nombre_carrera,prd_id_num_postulacion,pos_prioridad) %>% 
                 mutate(contador=1) %>% 
                 arrange(usu_id,prd_id_num_postulacion,pos_prioridad)





postulantes_p13<-postulantes_p13 %>% 
                 group_by(ies_nombre_instit,provincia_campus,car_nombre_carrera) %>% 
                 summarise(postulantes=sum(contador))

# POSTULACIONES:

postulaciones_p13<-postulaciones_2 %>% 
                   filter(pos_prioridad<=5 & prd_id_num_postulacion==446 & per_id==13) %>% 
                   select(usu_id,ies_nombre_instit,provincia_campus,
                   car_nombre_carrera) %>% 
                   mutate(contador=1)

postulaciones_p13<-postulaciones_p13 %>% 
                   group_by(provincia_campus,ies_nombre_instit,car_nombre_carrera) %>% 
                   summarise(postulaciones=sum(contador))
















reporte_post_efectivos<-post_pruebas %>% 
                        group_by(per_id,oca_repostulacion,provincia_ies) %>% 
                        summarise(postulantes=sum(contador))

#---------------------------------------------------------------------------------------------------

post_pruebas<-postulaciones_1 %>% 
              filter(oca_repostulacion==0 & per_id==2) %>% 
              select(per_id,usu_id,provincia_ies,car_nombre_carrea) %>% 
              mutate(contador=1) %>% 
              distinct()

reporte_post_efectivos<-post_pruebas %>% 
  group_by(per_id) %>% 
  summarise(postulantes=sum(contador))


#---------------------------------------------------------------------------------------------------

postulaciones_2$prd_descripcion<-trimws(postulaciones_2$prd_descripcion)

post_pruebas_p13<-postulaciones_2 %>% 
                  filter(prd_descripcion == "PRIMERA POSTULACION" & per_id==13) %>% 
                  select(usu_id,per_id) %>% 
                  mutate(contador=1) %>% 
                  distinct()


post_pruebas_p13_p14<-postulaciones_2 %>% 
                      filter(pos_estado_new %in% c("NA",1) & prd_descripcion %in% c("PRIMERA POSTULACION",
                                                                "SEGUNDA POSTULACIÓN",
                                                                "TERCERA POSTULACION",
                                                                "SEXTA POSTULACION")) %>% 
                      select(per_id,provincia_campus,car_nombre_carrera) %>% 
                      mutate(contador=1)





