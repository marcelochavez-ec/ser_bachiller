# PROCESO DE ACEPTACIÓN:

# PAQUETES:
library(RPostgreSQL)
library(dplyr)
library(reshape2)
library(xlsx)
library(tidyr)
library(dbplyr)
library(data.table)
library(ggplot2)
library(artyfarty)

load("/home/marcelo/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/asignacion_p2_p12.RData")# aceptación p2 al p12
load("/home/marcelo/Documents/PROYECTO_BI_SENESCYT_2/BDD_FUENTES/admision_asignacion_p13.RData")# aceptación p13


# CON ESTE PASO PUEDO CONVERTIR LOS CARACTERES UTF-8 SIN PROBLEMA A TODO EL DATA FRAME:
admision_asignacion_p13<-as.data.frame(lapply(admision_asignacion_p13, function(x) if(is.character(x))
                         iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)

asignacion_p2_p12<-as.data.frame(lapply(asignacion_p2_p12, function(x) if(is.character(x))
                         iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)


#----------------------------------------------------------------------------------------------------------------
# ANDREA:

# PERODOS DEL 2 AL 12:

acepta_p2_p12<-asignacion_p2_p12 %>%
               select(usu_id,usu_cc,per_id,provincia_campus,canton_campus,ies_nombre_instit,
                      car_nombre_carrea,nvc_estado,nvc_acepta_carrera,ccp_estado,ccp_cupos_reales,
                      ies_tipo_ies,ies_tipo_financiamiento,ies_nombre_instit,
                      segmentacion_persona,parroquia_campus) %>% 
               filter(nvc_estado=="A" & ccp_estado=="A")

acepta_p2_p12$nvc_acepta_carrera<-as.character(acepta_p2_p12$nvc_acepta_carrera)#Primero convierto a tipo character

acepta_p2_p12<-as.data.table(acepta_p2_p12)#Despues convierto a data.table

acepta_p2_p12$nvc_acepta_carrera<-recode(acepta_p2_p12$nvc_acepta_carrera,"S"="Sí","N"="No")

acepta_p2_p12[is.na(acepta_p2_p12)]<-"No se pronuncia"#Aki recien puedo recodificar los missing

names(acepta_p2_p12)[7]<-"nombre_carrera"

# PERODO 13:

acepta_p13<-admision_asignacion_p13 %>% 
            select(usu_id,usu_cc,per_id,provincia_campus,canton_campus,
                   asa_acepta_cupo,ies_tipo_ies,ies_tipo_financiamiento,
                   ies_nombre_instit,segmento_persona,provincia_campus,
                   canton_campus,parroquia_campus,car_nombre_carrera)

acepta_p13$asa_acepta_cupo<-recode(acepta_p13$asa_acepta_cupo,"1"="Sí",
                                   "2"="No","0"="No se pronuncia")            

names(acepta_p13)[6]<-"nvc_acepta_carrera"
names(acepta_p13)[10]<-"segmentacion_persona"
names(acepta_p13)[12]<-"nombre_carrera"

# PERODO 14:

senescyt_bi<-dbConnect("PostgreSQL", dbname="senescyt_bi",host="localhost",
                       port=5432,user="postgres",password="postgres")

aceptacion_p14<-dbGetQuery(senescyt_bi,statement=paste("select * from aceptacion_p14"))

aceptacion_p14<-as.data.frame(lapply(aceptacion_p14, function(x) if(is.character(x))
                   iconv(x,"UTF-8","UTF-8") else x),stringsAsFactors=TRUE)

aceptacion_p14$per_id<-14

aceptacion_p14$asignacion<-trimws(aceptacion_p14$asignacion)
aceptacion_p14$nro_aceptacion<-ifelse(aceptacion_p14$asignacion=="SEGUNDA ASIGNACI�N DE CUPOS",
                                      "Segunda aceptacion",
                                      ifelse(aceptacion_p14$asignacion=="PRIMERA POSTULACION",
                                      "Primera aceptación","XXX"))

acepta_p14<-aceptacion_p14 %>% 
            select(usu_id,per_id,provincia_vive,canton_vive,parroquia_vive,asa_acepta_cupo,
                   ies_nombre_instit,car_nombre_carrera,nro_aceptacion,ies_tipo_ies,
                   ies_tipo_financiamiento,ies_nombre_instit,segmentacion_persona)

acepta_p14$asa_acepta_cupo<-recode(acepta_p14$asa_acepta_cupo,
                                   "1.0"="Sí","2.0"="No","0.0"="No se pronuncia")

names(acepta_p14)[3]<-"provincia_campus"
names(acepta_p14)[4]<-"canton_campus"
names(acepta_p14)[5]<-"parroquia_campus"
names(acepta_p14)[6]<-"nvc_acepta_carrera"
names(acepta_p14)[8]<-"nombre_carrera"

#----------------------------------------------------------------------------------------------------------------
# DANIEL:

base_p2_p12<-acepta_p2_p12 %>% 
             select(usu_id,per_id,provincia_campus,canton_campus,parroquia_campus,nvc_acepta_carrera,
                    nombre_carrera,segmentacion_persona,ies_tipo_ies,ies_tipo_financiamiento,ies_nombre_instit)

base_p2_p12$nro_aceptacion<-"Única aceptación"

base_p13<-acepta_p13 %>% 
          select(usu_id,per_id,provincia_campus,canton_campus,parroquia_campus,nvc_acepta_carrera,
                 nombre_carrera,segmentacion_persona,ies_tipo_ies,ies_tipo_financiamiento,ies_nombre_instit)

base_p13$nro_aceptacion<-"Única aceptación"

base_p14<-acepta_p14 %>% 
          select(usu_id,per_id,provincia_campus,canton_campus,parroquia_campus,nvc_acepta_carrera,
                 nombre_carrera,segmentacion_persona,ies_tipo_ies,ies_tipo_financiamiento,ies_nombre_instit)

global_aceptaciones<-rbind(base_p2_p12,base_p13,acepta_p14)

global_aceptaciones$periodos<-recode(global_aceptaciones$per_id,
                                   "2"="I Semestre 2012",
                                   "3"="II Semestre 2012",
                                   "4"="I Semestre 2013",
                                   "5"="II Semestre 2013",
                                   "6"="I Semestre 2014",
                                   "7"="II Semestre 2014",
                                   "8"="I Semestre 2015",
                                   "9"="II Semestre 2015",
                                   "10"="I Semestre 2016",
                                   "12"="II Semestre 2016",
                                   "13"="I Semestre 2017",
                                   "14"="II Semestre 2017")

global_aceptaciones$periodos<-factor(global_aceptaciones$periodos,
                                     levels=c("I Semestre 2012",
                                              "II Semestre 2012",
                                              "I Semestre 2013",
                                              "II Semestre 2013",
                                              "I Semestre 2014",
                                              "II Semestre 2014",
                                              "I Semestre 2015",
                                              "II Semestre 2015",
                                              "I Semestre 2016",
                                              "II Semestre 2016",
                                              "I Semestre 2017",
                                              "II Semestre 2017"))

global_aceptaciones$nvc_acepta_carrera<-factor(global_aceptaciones$nvc_acepta_carrera,
                                               levels = c("Sí","No","No se pronuncia"))

reporte_aceptaciones<-dcast(global_aceptaciones,
                            global_aceptaciones$periodos+
                            #global_aceptaciones$provincia_campus+
                            #global_aceptaciones$canton_campus+
                            global_aceptaciones$ies_nombre_instit+
                            #global_aceptaciones$ies_tipo_ies+
                            global_aceptaciones$nombre_carrera~global_aceptaciones$nvc_acepta_carrera)

names(reporte_aceptaciones)[1]<-"PERIODOS"
names(reporte_aceptaciones)[2]<-"PROVINCIA DE LA IES"
names(reporte_aceptaciones)[3]<-"CANTÓN DE LA IES"
names(reporte_aceptaciones)[4]<-"NOMBRE DE LA IES"
names(reporte_aceptaciones)[5]<-"NOMBRE DE LA CARRERA"

write.xlsx2(reporte_aceptaciones,
            "c:/Users/rchavez/Documents/PROYECTO_BI_SENESCYT_2/BDD_REPORTES/REPORTE ACEPTACIONES POR IES Y NOMBRE CARRERA.xlsx",
            row.names = FALSE,append = TRUE)
#----------------------------------------------------------------------------------------------------------------
# GRÁFICO DE BARRAS:

ggplot(bdd_tercer_nivel,aes(x=anio, y=total, fill=nombre_amplio)) +
  geom_bar(stat = "identity") +
  #facet_wrap(~nombre_amplio,ncol = 6) +
  theme_replace() +
  theme(axis.text.x=element_text(angle=90,hjust=0.5,size=10)) +
  theme(axis.text.y=element_text(angle=0,hjust=0,size=10)) +
  theme(legend.title = element_text(colour="black", size=12, face="bold")) +
  # geom_text(data=bdd_tercer_nivel,
  #           aes(label=total),
  #           position=position_dodge(width=1),size=2,hjust=-0.1) +
  # scale_y_continuous(limits=c(0,1),labels=percent) +
  theme(legend.position="right") +
  labs(x="Año",y="Nro. de Titulados") +
  scale_fill_discrete(name="Campos Amplios:") +
  theme_economist()+
  labs(title = "Distribución del Nro. de Titulados", 
       subtitle = "Categoría: Tercer Nivel (Pregrado)", 
       caption = "Fuente: CGI - SNIESE") +
  geom_text(aes(label = total), size = 2.5, position = position_stack(vjust = 0.5))


global_aceptaciones$periodos<-recode(global_aceptaciones$per_id,
                                  "I Semestre 2012"=2,
                                  "II Semestre 2012"=3,
                                  "I Semestre 2013"=4,
                                  "II Semestre 2013"=5,
                                  "I Semestre 2014"=6,
                                  "II Semestre 2014"=7,
                                  "I Semestre 2015"=8,
                                  "II Semestre 2015"=9,
                                  "I Semestre 2016"=10,
                                  "II Semestre 2016"=12,
                                  "I Semestre 2017"=13,
                                  "II Semestre 2017"=14)

global_aceptaciones<-global_aceptaciones %>% 
                     filter(periodos>8)

global_aceptaciones<-global_aceptaciones %>% 
                     select(per)


ggplot(subset(global_aceptaciones,(provincia_campus=="AZUAY")),
       aes(x=per_id,y=nvc_acepta_carrera,fill=nvc_acepta_carrera)) +
       geom_bar(stat = "identity") +
       theme_replace() +
       theme(axis.text.x=element_text(angle=90,hjust=0.5,size=10)) +
       theme(axis.text.y=element_text(angle=0,hjust=0,size=10)) +
       theme(legend.title = element_text(colour="black", size=12, face="bold")) +
       theme(legend.position="right") +
       labs(x="",y="") +
       scale_fill_discrete(name="") +
       theme_economist() +
       geom_text(aes(label = nvc_acepta_carrera), size = 2.5, position = position_stack(vjust = 0.5))







































