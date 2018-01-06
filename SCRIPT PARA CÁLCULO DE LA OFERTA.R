# load("/home/marcelo/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/asignaciones_1.RData")
# asignaciones_1<-as.data.frame(lapply(asignaciones_1,function(x) if(is.character(x))
#   iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)
# 
# 
# load("/home/marcelo/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/VISTAS/asignaciones_2.RData")
# asignaciones_2<-as.data.frame(lapply(asignaciones_2,function(x) if(is.character(x))
#   iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)


asigna_p2_p12<-asignaciones_1 %>% 
               filter(ccp_estado=="A") %>% 
               mutate(nvc_acepta_carrera=recode(nvc_acepta_carrera,
                                                "S"="SI",
                                                "N"="NO")) %>% 
               mutate(nvc_acepta_carrera=ifelse(is.na(nvc_acepta_carrera),"NO_SE_PRONUNCIA",
                                               nvc_acepta_carrera)) %>% 
               select(usu_id,provincia_campus,canton_campus,
                      parroquia_campus,ccp_cupos_reales,
                      nvc_acepta_carrera,ies_nombre_instit,per_id,
                      car_nombre_carrea)

reporte_asigna_p2_p12<-dcast(asigna_p2_p12,
                     asigna_p2_p12$per_id+
                     asigna_p2_p12$provincia_campus+
                     asigna_p2_p12$canton_campus+
                     asigna_p2_p12$ciudad_campus+ 
                     asigna_p2_p12$parroquia_campus+
                     asigna_p2_p12$ies_nombre_instit+
                     asigna_p2_p12$ies_tipo_ies+   
                     asigna_p2_p12$car_nombre_carrea~
                     asigna_p2_p12$nvc_acepta_carrera)

names(reporte_asigna_p2_p12)<-c("per_id","provincia_campus","canton_campus","ciudad_campus",
                                "parroquia_campus","ies_nombre_instit","ies_tipo_ies",
                                "car_nombre_carrera","SI","NO","NO_SE_PRONUNCIA")




oferta_p2_p12<-asignaciones_1 %>% 
               filter(ccp_estado=="A") %>% 
               select(ccp_id,provincia_campus,canton_campus,
               parroquia_campus,ccp_cupos_reales,ies_nombre_instit,per_id,
               car_nombre_carrea) %>% 
               distinct()

oferta_p2_p12<-oferta_p2_p12 %>% 
               group_by(per_id,provincia_campus,ies_nombre_instit,
                        car_nombre_carrea) %>% 
               summarise(total_cupos<-sum(ccp_cupos_reales))

names(oferta_p2_p12)[4]<-"car_nombre_carrera"
names(oferta_p2_p12)[5]<-"total_cupos"

oferta_p2_p12<-oferta_p2_p12 %>% filter(total_cupos!="NA")

base_1<-reporte_asigna_p2_p12 %>% 
        select(per_id,provincia_campus,ies_nombre_instit,car_nombre_carrera,
               NO,SI,NO_SE_PRONUNCIA)

base_2<-oferta_p2_p12 %>% 
        select(per_id,provincia_campus,ies_nombre_instit,car_nombre_carrera,total_cupos)

reporte_1<-merge(base_1,base_2,by=c("per_id","provincia_campus","ies_nombre_instit",
                                    "car_nombre_carrera"),all=F)

reporte_1$total_cupos<-ifelse(is.na(reporte_1$total_cupos),
                              rowSums(reporte_1[,5:7]),
                              reporte_1$total_cupos)

reporte_1$total_cupos<-ifelse(reporte_1$total_cupos==0,
                              rowSums(reporte_1[,5:7]),
                              reporte_1$total_cupos)

#-----------------------------------------------------------------------------------------------

oferta_p13_p14<-asignaciones_2 %>% 
                select(usu_id,provincia_campus,ies_nombre_instit,per_id,
                asa_acepta_cupo,car_nombre_carrera)

reporte_asigna_p13_p14<-dcast(oferta_p13_p14,
                              oferta_p13_p14$per_id+
                              oferta_p13_p14$provincia_campus+
                              oferta_p13_p14$ies_nombre_instit+
                              oferta_p13_p14$car_nombre_carrera~
                              oferta_p13_p14$asa_acepta_cupo)

names(reporte_asigna_p13_p14)<-c("per_id","provincia_campus","ies_nombre_instit",
                                 "car_nombre_carrera","NO_SE_PRONUNCIA","SI","NO")

oferta_p13_p14<-asignaciones_2 %>% 
                #filter(per_id==14) %>% 
                select(ofa_id,provincia_campus,ccp_cupos_nivelacion,
                ccp_cupos_primer_semestre,ccp_cupos_exoneracion,per_id,
                ies_nombre_instit,car_nombre_carrera) %>% 
                mutate(total_cupos=rowSums(.[3:5])) %>% 
                distinct() %>% 
                select(provincia_campus,total_cupos,ies_nombre_instit,per_id,
                       car_nombre_carrera)

reporte_2<-merge(oferta_p13_p14,reporte_asigna_p13_p14,
                 by=c("per_id","provincia_campus","ies_nombre_instit","car_nombre_carrera"),
                 all=F)

reporte_oferta_asignacion<-rbind(reporte_1,reporte_2)


#--------------------------------------------------------------------------------------------------------

oferta_p13_p14<-asignaciones_2 %>% 
  #filter(per_id==14) %>% 
                select(ofa_id,provincia_campus,ccp_cupos_nivelacion,
                ccp_cupos_primer_semestre,ccp_cupos_exoneracion,per_id,
                ies_nombre_instit) %>% 
                mutate(total_cupos=rowSums(.[3:5])) %>% 
                distinct() %>% 
                select(provincia_campus,total_cupos,ies_nombre_instit,per_id) %>% 
  
#--------------------------------------------------------------------------------------------------------

oferta_p13_p14<-asignaciones_2 %>%
  #filter(per_id==14) %>%
  select(ofa_id,provincia_campus,ccp_cupos_nivelacion,
         ccp_cupos_primer_semestre,ccp_cupos_exoneracion,per_id,
         ies_nombre_instit,ies_tipo_ies,car_nombre_carrera,provincia_campus,
         car_nombre_carrera) %>%
  mutate(total_cupos=rowSums(.[3:5])) %>%
  distinct() %>%
  select(total_cupos,per_id,car_nombre_carrera,provincia_campus) %>%
  group_by(per_id,car_nombre_carrera,provincia_campus) %>%
  summarise(total_cupos=sum(total_cupos))


cupos_per14<-oferta_p13_p14 %>% filter(per_id==14)

cupos_per14<-as.data.frame(lapply(cupos_per14,function(x) if(is.character(x))
  iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)

cupos_per14$provincia_campus<-trimws(cupos_per14$provincia_campus)

cupos_per14$region<-ifelse(cupos_per14$provincia_campus %in% c("SUCUMBIOS","NAPO","ORELLANA","PASTAZA",
                                                           "MORONA SANTIAGO","ZAMORA CHINCHIPE"),"REGIÓN AMAZÓNICA",
                    ifelse(cupos_per14$provincia_campus %in% c("PICHINCHA","CARCHI","TUNGURAHUA","CHIMBORAZO",
                                                           "CAÑAR","AZUAY","LOJA","IMBABURA","BOLIVAR","COTOPAXI"),"REGIÓN SIERRA",
                    ifelse(cupos_per14$provincia_campus %in% c("ESMERALDAS","SANTO DOMINGO DE LOS TSACHILAS","MANABI","LOS RIOS",
                                                           "GUAYAS","SANTA ELENA","EL ORO"),"REGIÓN COSTA","NO EXISTE")))

# RANKING NACIONAL:
# cupos_per14<-cupos_per14 %>% arrange(-total_cupos)


cupos_per14<-split(cupos_per14,cupos_per14$region)

cupos_p14_sierra<-data.frame(cupos_per14[[3]])
cupos_p14_costa<-data.frame(cupos_per14[[2]])
cupos_p14_amazonia<-data.frame(cupos_per14[[1]])

#--------------------------------------------------------------------------------------------------------------------

cupos_p14_sierra<-cupos_p14_sierra %>% 
                  select(-per_id,-provincia_campus,-region)


cupos_p14_sierra <- cupos_p14_sierra %>% 
                    group_by(car_nombre_carrera) %>% 
                    summarise(total=sum(total_cupos))

cupos_p14_costa<-cupos_p14_costa %>% 
                 select(-per_id,-provincia_campus,-region)


cupos_p14_costa <-cupos_p14_costa %>% 
                   group_by(car_nombre_carrera) %>% 
                   summarise(total=sum(total_cupos))

cupos_p14_amazonia<-cupos_p14_amazonia %>% 
                    select(-per_id,-provincia_campus,-region)

cupos_p14_amazonia <- cupos_p14_amazonia %>% 
                    group_by(car_nombre_carrera) %>% 
                    summarise(total=sum(total_cupos))

