
inscritos_base_p8_p12<-inscritos_p8_p12%>% 
  filter(provincia_nace=="GALAPAGOS" &
         recinto_asignado != 'NA') %>%
  select(usu_id,provincia_nace,rinde_enes,per_id)

inscritos_base_p8_p12$rinde_enes<-recode(inscritos_base_p8_p12$rinde_enes,"N"="SÍ",
                                         "M"="SÍ")

repetidos_p8_p12<-dcast(inscritos_base_p8_p12,
                        inscritos_base_p8_p12$usu_id~
                          inscritos_base_p8_p12$per_id)

repetidos_p8_p12$escolar<-rowSums(repetidos_p8_p12[,2:5],na.rm = T)

repetidos_p8_p12$escolar<-ifelse(repetidos_p8_p12$escolar==12,"ESCOLAR","NO ESCOLAR")

names(repetidos_p8_p12)[1]<-"usu_id"
names(repetidos_p8_p12)[6]<-"escolar"

#SELECCIONAR DE PERIODO 12

base_rinde_p12<-inscritos_p8_p12 %>% 
  filter(per_id==12) %>% 
  select(usu_id,rinde_enes)

ins_p12<-repetidos_p8_p12 %>% 
  select(usu_id,escolar)

base_insular<-merge(base_rinde_p12,ins_p12,by="usu_id",all = F)

reporte_2016<-data.frame(table(base_insular$escolar))

#-------------------------------------------------------------------------------------
ins_p13<-inscritos_p13_p14 %>%
         filter(per_id==13 & ins_estado=="T" & prov_residencia=="GALAPAGOS") %>% 
         select(per_id,cae_rinde_examen,ins_poblacion)

ins_p14<-inscritos_p13_p14 %>%
         filter(ins_sede_rec_asignado!="NA" & per_id==14 & prov_residencia=="GALAPAGOS") %>% 
         select(per_id,cae_rinde_examen,ins_poblacion)

reporte_2017<-rbind(ins_p13,ins_p14)

reporte_2017$ins_poblacion<-ifelse(is.na(reporte_2017$ins_poblacion),
                             "SIN REGISTRO",reporte_2017$ins_poblacion)

reporte_2017<-reporte_2017 %>% filter(cae_rinde_examen=="SI")

reporte_2017<-dcast(reporte_2017,
                    reporte_2017$per_id~
                    reporte_2017$ins_poblacion)

#-----------------------------------------------------------------------------------

# Número de Postulaciones por residencia de los bachilleres, por UEP y carrera año 2017

postulaciones_p13_p14<-postulaciones_2 %>% 
                       #filter(pos_estado_new==1) %>% 
                       select(usu_id,car_nombre_carrera,per_id)

ins_p13<-inscritos_3 %>%
         filter(per_id==13 & ins_estado=="T" & prov_residencia=="GALAPAGOS") %>% 
         select(usu_id,ued_tipo) %>% 
         mutate(ued_tipo=ifelse(is.na(ued_tipo),"SIN REGISTRO",ued_tipo))

ins_p14<-inscritos_3 %>%
         filter(ins_sede_rec_asignado!="NA" & per_id==14 & prov_residencia=="GALAPAGOS") %>% 
         select(usu_id,ued_tipo) %>% 
         mutate(ued_tipo=ifelse(is.na(ued_tipo),"SIN REGISTRO",ued_tipo))

ins_p13_p14<-rbind(ins_p13,ins_p14)

reporte_2<-merge(ins_p13_p14,postulaciones_p13_p14,by="usu_id",all=F)

reporte_2<-dcast(reporte_2,
                 reporte_2$per_id+
                 reporte_2$car_nombre_carrera~
                 reporte_2$ued_tipo)









