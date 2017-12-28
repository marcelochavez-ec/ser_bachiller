post_p2_p12<-postulacion_p2_p12 %>% 
             select(per_id,segmentacion_persona) %>% 
             filter(segmentacion_persona==c("GAR PERIODO X","GAR PERIODOS ANTERIORES","GRUPO DE ALTO RENDIMIENTO"))

post_p13<-postulacion_p13 %>% 
          select(per_id,segmentacion_carrera) %>% 
          filter(segmentacion_carrera=="GAR")

names(post_p13)[2]<-"segmentacion_persona"

gar_p2_p13<-rbind(post_p2_p12,post_p13)
gar_p2_p13$segmentacion_persona<-recode(gar_p2_p13$segmentacion_persona,"GAR PERIODO X"="GAR","GAR PERIODOS ANTERIORES"="GAR",
                                        "GRUPO DE ALTO RENDIMIENTO"="GAR")

gar_p2_p13$periodos<-recode(gar_p2_p13$per_id,"2"="I Semestre 2012",
                             "3"="II Semestre 2012",
                             "4"="I Semestre 2013",
                             "5"="II Semestre 2013",
                             "6"="I Semestre 2014",
                             "7"="II Semestre 2014",
                             "8"="I Semestre 2015",
                             "9"="II Semestre 2015",
                             "10"="I Semestre 2016",
                             "12"="II Semestre 2016",
                             "13"="I Semestre 2017")            

gar_p2_p13$periodos<-factor(gar_p2_p13$periodos,levels=c("I Semestre 2012",
                                                           "II Semestre 2012",
                                                           "I Semestre 2013",
                                                           "II Semestre 2013",
                                                           "I Semestre 2014",
                                                           "II Semestre 2014",
                                                           "I Semestre 2015",
                                                           "II Semestre 2015",
                                                           "I Semestre 2016",
                                                           "II Semestre 2016",
                                                           "I Semestre 2017"))

reporte_gar<-dcast(gar_p2_p13,gar_p2_p13$periodos~gar_p2_p13$segmentacion_persona)


write.xlsx2(reporte_gar,"C:/Users/rchavez/Documents/PROYECTO_BI_SENESCYT_2/BDD_REPORTES/GAR P2 - P13.xlsx",
            row.names = FALSE,append = TRUE)




# gar<-data.frame(table(gar_p2_p13$segmentacion_persona))
# 
# gar<-gar %>% 
#      filter(Freq!=0)