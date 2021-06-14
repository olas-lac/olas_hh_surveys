###code OLAS clean

load("jefe231020.RDA")


##Generate Dummy Variables
###Pipedwater
jefes$pipedhouse <- ifelse(jefes$pais_c=="ARG" & jefes$iv6==1 & jefes$iv7==1,1, ###iv6 es locacion iv7 es caneria, una version anterior lo catalogaba como iv9 pero eso se refiere al bano 
                           ifelse(jefes$pais_c=="BOL" & jefes$s01a_10==1,1, ##separa locacion y origen en la misma
                                  ifelse(jefes$pais_c=="BRA" & jefes$s01007==1 & jefes$s01010==1,1, ##s010007 es fuente y s01010 es locacion
                                         ifelse(jefes$pais_c=="CHL" & jefes$v20<=3 & jefes$v22==1,1, ##v20 es fuente distinguen con medidor y v22 es donde
                                                ifelse(jefes$pais_c=="COL",NA, ##no tiene pregunta de locacion ##REPLACE WITH NA
                                                       ifelse(jefes$pais_c=="CRI" & jefes$v12<=4 & jefes$v11==1,1, ##v11 es locacion y v12 fuente
                                                              ifelse(jefes$pais_c=="DOM" & jefes$donde_proviene_agua==1,1, #la pregunta separa fuente y locacion
                                                                     ifelse(jefes$pais_c=="ECU" & jefes$vi16==1 & jefes$vi16c==1,1, ##vi16 es fuente y vi16c es locacion
                                                                            ifelse(jefes$pais_c=="GTM" & jefes$p02b03==1,1, ##la pregunta engloba ambas
                                                                                   ifelse(jefes$pais_c=="HND" & jefes$dv105==1 & jefes$dv106==1,1, ##v106 locacion y ##v105 fuente
                                                                                          ifelse(jefes$pais_c=="JAM" & jefes$i27==1,1, ##la pregunta engloba ambas
                                                                                                 ifelse(jefes$pais_c=="MEX" & jefes$disp_agua==1,1, ##la pregunta engloba ambas
                                                                                                        ifelse(jefes$pais_c=="NIC" & jefes$s1p15==1,1, ##la pregunta engloba ambas
                                                                                                               ifelse(jefes$pais_c=="PAN",NA, ##no distingue ##REPLACE WITH NA
                                                                                                                      ifelse(jefes$pais_c=="PER" & jefes$t110==1,1, ##la pregunta engloba ambos
                                                                                                                             ifelse(jefes$pais_c=="PRY" & jefes$v06<=4 & jefes$v07a==2,1, ##v06 es fuente y 07a es locacion por alguna razon dos es dentro de la vivienda
                                                                                                                                    ifelse(jefes$pais_c=="SLV" & jefes$r312<=2,1, ##versi?n anterior decia 1 pero cambiado ya que parece que eso se refiere a abastecimiento privado pero aun por tuberia
                                                                                                                                           ifelse(jefes$pais_c=="URY" & jefes$d11==1 & jefes$d12==1,1,0)))))))))))))))))) ##engloba las dos


###piped to premises-this includes only plot
jefes$pipedprem <-ifelse(jefes$pais_c=="ARG" & jefes$iv6==2 & jefes$iv7==1,1, ##iv6 es locacion 
                         ifelse(jefes$pais_c=="BOL" & jefes$s01a_10==2,1, ##engloba
                                ifelse(jefes$pais_c=="BRA" & jefes$s01007==1 & jefes$s01010==2,1, ##10 es locacion
                                       ifelse(jefes$pais_c=="CHL" & jefes$v20<=3 & jefes$v22==2,1, ##v22 es locacion
                                              ifelse(jefes$pais_c=="COL"&jefes$p5050==1,1, ##solo pregunta fuente pero no destino
                                                     ifelse(jefes$pais_c=="CRI" & jefes$v12<=4 & jefes$v11==2,1, ##v11 es igual a destino
                                                            ifelse(jefes$pais_c=="DOM" & jefes$donde_proviene_agua==2,1, ##engloba
                                                                   ifelse(jefes$pais_c=="ECU" & jefes$vi16==1 & jefes$vi16c==2,1, ##c es destino
                                                                          ifelse(jefes$pais_c=="GTM" & jefes$p02b03==2,1, ##engloba
                                                                                 ifelse(jefes$pais_c=="HND" & jefes$dv105==1 & jefes$dv106==2,1, ##106 es destino
                                                                                        ifelse(jefes$pais_c=="JAM" & jefes$i27==2,1, ##se toma outside pipe como dentro del terreno
                                                                                               ifelse(jefes$pais_c=="MEX" & jefes$disp_agua==2,1, ##engloba
                                                                                                      ifelse(jefes$pais_c=="NIC" & jefes$s1p15==2,1, ##engloba
                                                                                                             ifelse(jefes$pais_c=="PAN" & jefes$v1i_agua_b<=3 & jefes$v1j_ubicac==1,1, ##agua es origen y ubic es destino pero engloba dentro y fuera
                                                                                                                    ifelse(jefes$pais_c=="PER" & jefes$t110==2,1, ##engloba
                                                                                                                           ifelse(jefes$pais_c=="PRY" & jefes$v06<=4 & jefes$v07a==1,1, ##1 es terreno por alguna razon
                                                                                                                                  ifelse(jefes$pais_c=="SLV" & jefes$r312%in%c(3,4),1, ##no se toman otros tipos de abastecimiento
                                                                                                                                         ifelse(jefes$pais_c=="URY" & jefes$d11==1 & jefes$d12==2,1,0)))))))))))))))))) ##en realidad esto se refiere a menos de 100 metros d ela vivienda, no hay terreno como tal


###Non piped improved upper bound
jefes$npimpub<- ifelse(jefes$iv7%in%c(2,3,4),1, #ARG ##correcto porque el destino da igual, no ponemos otra ya que puede haber protegidas en otra no es claro
                       ifelse(jefes$s01a_10%in%c(3,4,5,6,7,9),1, #BOL bolivia clasifica bien
                              ifelse(jefes$s01007%in%c(2,3,4,5),1, ##BRA no ponemos otra ya que ahi ir?an rios etc, no hay muchas protegidas ahi
                                     ifelse(jefes$v20%in%c(4),1, ##CHILE otra especifica y en general no eran protegidas
                                            ifelse(jefes$p5050%in%c(3,4,5,6,7),1,##COL solo esas pueden ser pero dudas en 3,4 y 6
                                                   ifelse(jefes$v12%in%c(5,6,7),1, ##cr incluye otro 
                                                          ifelse(jefes$donde_proviene_agua%in%c(4,5,6,7,8),1, ##dOM incluye tubo en la calle 5,manantial, rio o arroyo y pozo
                                                                 ifelse(jefes$vi16%in%c(2,3,5),1, ##ECU incluir otra fuente por tuber?a
                                                                        ifelse(jefes$p02b03%in%c(3,4,5,7),1, #GUATE 4 es pozo y 5 lago,rio o manantial
                                                                               ifelse(jefes$dv105%in%c(2,3,4,5),1, #honduras correcto
                                                                                      ifelse(jefes$i27%in%c(3,4,5,6,7),1, #jam incluye pozos 4 y 5 spring and river
                                                                                             ifelse(jefes$disp_agua%in%c(3,4,7),1, #mex 7 engloba pozo,rio, arroyo
                                                                                                    ifelse(jefes$s1p15%in%c(3,4,5),1, #nic 4 es pozo sin especificacion
                                                                                                           ifelse(jefes$v1i_agua_b%in%c(4,6),1, #pan does classify protected unprotected
                                                                                                                  ifelse(jefes$t110%in%c(3,5,6),1, #peru no incluye otros
                                                                                                                         ifelse(jefes$v06%in%c(5,6,7,8,10),1, #PRY los pozos no 5 6 y 7 no queda claro si son o no mejorados
                                                                                                                                ifelse(jefes$r313%in%c(4.1,5,5.1,8,10,12),1, #svy does classify properly
                                                                                                                                       ifelse(jefes$d11%in%c(3,4),1,0)))))))))))))))))) #uruguay clasifica bien


### non piped improved lower bound 
jefes$npimplb<- ifelse(jefes$iv7%in%c(2,3),1, #elseARG diferencia es otro
                       ifelse(jefes$s01a_10%in%c(3,4,5,6,7,9),1, #BOL correctamente clasificado
                              ifelse(jefes$s01007%in%c(4,5),1, ##BRA los pozos no especificados
                                     ifelse(jefes$pais_c=="CHL",0, ##CHILE en todos engloba cosas que no
                                            ifelse(jefes$p5050%in%c(5,7),1,##COL 5 y 7 siempre son las otras engloban cosas que no
                                                   ifelse(jefes$pais_c=="CRI",0, ##cr correcto
                                                          ifelse(jefes$donde_proviene_agua%in%c(4,5,7),1,##dOM sin rio y arroyo y pozos
                                                                 ifelse(jefes$vi16%in%c(2),1,  ##ECU sin fuentes englobadas que no distinguen solo llave publica
                                                                        ifelse(jefes$p02b03%in%c('3','7'),1, #GUATE solo pila publica y agua de lluvia
                                                                               ifelse(jefes$dv105%in%c(4),1, #honduras unico termino que no engloba
                                                                                      ifelse(jefes$i27%in%c(3,6,7),1, #jam solo public y agua de lluvia
                                                                                             ifelse(jefes$disp_agua%in%c(3,4),1, #mex correcto
                                                                                                    ifelse(jefes$s1p15%in%c(3,5),1, #nic pozo puede no ser mejorado
                                                                                                           ifelse(jefes$v1i_agua_b%in%c(4,6),1, #pan does calssify protected unprotected
                                                                                                                  ifelse(jefes$t110%in%c(3,6),1, #peru correcto
                                                                                                                         ifelse(jefes$v06%in%c(8,10),1, #PRY correcto
                                                                                                                                ifelse(jefes$r313%in%c(4.1,5,5.1,8,10,12),1, #svy does classify properly
                                                                                                                                       ifelse(jefes$d11%in%c(3,4),1,0)))))))))))))))))) #uruguay clasifica bien




##Water 7 days a week....countries not listed do not include question
#Frequency
jefes$freq<- ifelse(jefes$pais_c == "ARG",NA, ##ARG doesnt ask about frequency
                    ifelse(jefes$s01a_11b%in%c(7),1, #BOL
                           ifelse(jefes$s01008%in%c(1),1, ##BRA
                                  ifelse(jefes$pais_c=="CHL",NA, ##CHILE doesnt ask about frequency
                                         ifelse(jefes$p4040%in%c(1),1, #col
                                                ifelse(jefes$pais_c=="CRI",NA, ##cr doesnt ask about frequency
                                                       ifelse(jefes$pais_c=="DOM",NA, ##DOM doesnt ask about frequency
                                                              ifelse(jefes$pais_c=="ECU",NA, ##ECU doesnt ask about frequency
                                                                     ifelse(jefes$pais_c=="GTM",NA, ##GTM doesnt ask about frequency
                                                                            ifelse(jefes$pais_c=="HND",NA, ##HND doesnt ask about frequency
                                                                                   ifelse(jefes$pais_c=="JAM",NA, ##JAM doesnt ask about frequency
                                                                                          ifelse(jefes$dotac_agua%in%c(1),1, #mex
                                                                                                 ifelse(jefes$pais_c=="NIC",NA, ##NIC doesnt ask about frequency
                                                                                                        ifelse(jefes$pais_c=="PAN"& jefes$v1j1_veran == 7 & jefes$v1j1_invie == 7, 1,##PAN does ask about frequency, need to review.
                                                                                                               ifelse(jefes$p110c%in%c(1),1, #peru
                                                                                                                      ifelse(jefes$pais_c=="PRY",NA, ##PRY does ask about frequency, but only hours per day, does not ask about days
                                                                                                                             ifelse(jefes$r312d%in%c(7),1, #SVD
                                                                                                                                    ifelse(jefes$pais_c=="URY",NA,0)))))))))))))))))) #Uruguay doesnt ask about frequency

p <-split(jefes, jefes$pais_c)

##R: If there isnt a question about exclusive, NA
##Sanitation
###Unshared bathroom
jefes$unshared<- ifelse(jefes$ii9%in%c(1),1, #ARG
                        ifelse(jefes$s01a_17%in%c(1),1, #BOL
                               ifelse(jefes$s01011a%in%c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),1, ##BRA
                                      ifelse(jefes$pais_c%in%c("CHL"),NA, ##CHILE ##CHANGE TO NA
                                             ifelse(jefes$p5030%in%c(1),1,##COL
                                                    ifelse(jefes$v13b%in%c(1),1, ##cr
                                                           ifelse(jefes$tipo_sanitario%in%c(1,3),1, ##dOM
                                                                  ifelse(jefes$vi15%in%c(2),1, ##ECU
                                                                         ifelse(jefes$pais_c%in%c("GTM"),NA, #GUATE
                                                                                ifelse(jefes$dh206%in%c(1),1, #honduras
                                                                                       ifelse(jefes$i6%in%c(1),1, #jam
                                                                                              ifelse(jefes$uso_compar%in%c(2),1, #mex
                                                                                                     ifelse(jefes$pais_c%in%c("NIC"),NA, #nic
                                                                                                            ifelse(jefes$v1l_uso_sa%in%c(1),1, #pan
                                                                                                                   ifelse(jefes$pais_c%in%c("PER"),NA, #peru
                                                                                                                          ifelse(jefes$pais_c%in%c("PRY"),NA, #PRY
                                                                                                                                 ifelse(jefes$r318%in%c(2),1, #svy does classify properly
                                                                                                                                        ifelse(jefes$d15%in%c(1),1,0))))))))))))))))))



#Sewer
jefes$sewer <- ifelse(jefes$iv11%in%c(1),1, #ARG
                      ifelse(jefes$s01a_16%in%c(1),1, #BOL
                             ifelse(jefes$s01012a%in%c(1),1, ##BRA
                                    ifelse(jefes$v23%in%c(1),1, ##CHILE
                                           ifelse(jefes$p5020%in%c(1),1,##COL
                                                  ifelse(jefes$v13a%in%c(1),1, ##cr
                                                         ifelse(jefes$se_encuentra_conectada_a%in%c(2),1, ##dOM
                                                                ifelse(jefes$vi13%in%c(1),1, ##ECU
                                                                       ifelse(jefes$p02b07%in%c(1),1, #GUATE
                                                                              ifelse(jefes$dh205%in%c(1),1, #honduras
                                                                                     ifelse(jefes$i5%in%c(1),1, #jam
                                                                                            ifelse(jefes$drenaje%in%c(1),1, #mex
                                                                                                   ifelse(jefes$s1p18%in%c(3),1, #nic
                                                                                                          ifelse(jefes$pais_c%in%c("PAN"),NA, #not separated from septic
                                                                                                                 ifelse(jefes$t111a%in%c(1,2),1, #peru
                                                                                                                        ifelse(jefes$v13%in%c(1),1, #PRY
                                                                                                                               ifelse(jefes$r317%in%c(1,3),1, #svy does classify properly
                                                                                                                                      ifelse(jefes$d16%in%c(1),1,0))))))))))))))))))

##R: If the country doesnt differentiate between the sewer and septic, all households are put in septic
##Septic        
jefes$septic <- ifelse(jefes$iv11%in%c(2),1, #ARG
                       ifelse(jefes$s01a_16%in%c(2),1, #BOL
                              ifelse(jefes$s01012a%in%c(2,3),1, ##BRA
                                     ifelse(jefes$v23%in%c(2),1, ##CHILE
                                            ifelse(jefes$p5020%in%c(2),1,##COL
                                                   ifelse(jefes$v13a%in%c(2,3),1, ##cr
                                                          ifelse(jefes$se_encuentra_conectada_a%in%c(1),1, ##dOM
                                                                 ifelse(jefes$vi13%in%c(2),1, ##ECU//jmp suma cxon 3 tmb
                                                                        ifelse(jefes$p02b07%in%c(2),1, #GUATE
                                                                               ifelse(jefes$dh205%in%c(2),1, #honduras
                                                                                      ifelse(jefes$i5%in%c(2),1, #jam this is just for not linked to anything
                                                                                             ifelse(jefes$drenaje%in%c(2),1, #mex
                                                                                                    ifelse(jefes$s1p18%in%c(4),1, #nic
                                                                                                           ifelse(jefes$v1k_servic%in%c(2),1, #not separated from septic
                                                                                                                  ifelse(jefes$t111a%in%c(4),1, #peru
                                                                                                                         ifelse(jefes$v13%in%c(2),1, #PRY
                                                                                                                                ifelse(jefes$r317%in%c(2,4),1, #svy does classify properly
                                                                                                                                       ifelse(jefes$d16%in%c(2),1,0))))))))))))))))))



##Improved latrine higher bound 
jefes$latrinehb <- ifelse(jefes$iv11%in%c(3),1, #ARG,lb seria 0
                          ifelse(jefes$s01a_15%in%c(2,4),1, #BOL lb es lo mismo
                                 ifelse(jefes$s01012a%in%c(4),1, ##BRA//lb es 0
                                        ifelse(jefes$v23%in%c(3,4,6,7),1, ##CHILE clas correcta lb es lo mismo
                                               ifelse(jefes$p5020%in%c(3,4),1,##COL lb es solo 3
                                                      ifelse(jefes$v13a%in%c(4),1, ##cr lb es 0
                                                             ifelse(jefes$tipo_sanitario%in%c(3,4),1, ##dOM lb es 0
                                                                    ifelse(jefes$vi13%in%c(3) | jefes$vi13b%in%c(1),1, ##ECU//jmp suma cxon 3 tmb como fosa, lb es 0
                                                                           ifelse(jefes$p02b07%in%c(4),1, #GUATE lb es 0
                                                                                  ifelse(jefes$dh205%in%c(5,6),1, #honduras bien clasificado
                                                                                         ifelse(jefes$i5%in%c(3),1, #jam lb is 0
                                                                                                ifelse(jefes$pais_c=="MEX",NA, #mex lb es 0, mex doesnt ask about toilet type
                                                                                                       ifelse(jefes$s1p18%in%c(2),1, #nic mismo que hb
                                                                                                              ifelse(jefes$v1k_servic%in%c(1),1, #lb es 0 PAN
                                                                                                                     ifelse(jefes$t111a%in%c(3,5),1, #peru lb is only 3
                                                                                                                            ifelse(jefes$v13%in%c(3,5,6,7),1, #PRY lb is only 5
                                                                                                                                   ifelse(jefes$r317%in%c(5,6,7,8,9,10),1, #svy lb is only 7-10
                                                                                                                                          ifelse(jefes$pais_c%in%c("URY"),NA,0)))))))))))))))))) #uruguay doesnt have a question


##Improved latrine lower bound 
jefes$latrinelb <- ifelse(jefes$s01a_15%in%c(2,4),1, #BOL lb es lo mismo
                          ifelse(jefes$v23%in%c(3,4,6,7),1, ##CHILE clas correcta lb es lo mismo
                                 ifelse(jefes$dh205%in%c(5,6),1, #honduras bien clasificado
                                        ifelse(jefes$s1p18%in%c(2),1, #nic mismo que hb
                                               ifelse(jefes$vi13%in%c(3) | jefes$vi13b%in%c(1),1, #ECU
                                                      ifelse(jefes$t111a%in%c(3),1, #peru lb is only 3
                                                             ifelse(jefes$v13%in%c(5),1, #PRY lb is only 5
                                                                    ifelse(jefes$r317%in%c(7,8,9,10),1, #svy lb is only 7-10
                                                                           ifelse(jefes$pais_c%in%c("URY"),NA,
                                                                                  ifelse(jefes$pais_c%in%c("MEX"),NA,0))))))))))




##open defecation apro
jefes$opdef <- ifelse(jefes$iv8%in%c(0,2,3,4,5,6,7,8,9),1, # ARG  
                      ifelse(jefes$s01a_15%in%c(5),1, #BOL 
                             ifelse(jefes$s01011c%in%c(2),1, ##BRA
                                    ifelse(jefes$v23%in%c(8),1, ##CHILE class correcta 
                                           ifelse(jefes$p5020%in%c(6),1,##COL
                                                  ifelse(jefes$v14a%in%c(0),0, ##cr 
                                                         ifelse(jefes$tipo_sanitario%in%c(5),1, ##dOM
                                                                ifelse(jefes$vi13d%in%c(1),1, ##ecuador
                                                                       ifelse(jefes$p02b07%in%c(5),1, #GUATE
                                                                              ifelse(jefes$dh204%in%c(2),1, #honduras bien clasificado
                                                                                     ifelse(jefes$i5%in%c(5),1, #jam
                                                                                            ifelse(jefes$sanit_agua%in%c(3),1, #mex 
                                                                                                   ifelse(jefes$s1p18%in%c(6),1, #nic 
                                                                                                          ifelse(jefes$v1k_servic%in%c(3),1, ##panama 
                                                                                                                 ifelse(jefes$pais_c=="PER",NA, #asks if toilet is connected to open field, but not if household does not have access to toilet
                                                                                                                        ifelse(jefes$v12%in%c(2),1, #PRY lb is only 5
                                                                                                                               ifelse(jefes$r316%in%c(2),1, #svy 
                                                                                                                                      ifelse(jefes$d13%in%c(2),1,0)))))))))))))))))) #URY




##Creation of income quintiles
# 
#   ###Income quintiles
#   ingreso<- ingreso%>%
#     mutate(itpc=sum(ing_hogar/nmiembros_ch))
#   ingresoh<- ingreso%>%
#     filter(jefes_ci)
#   ###CREATE quintiles
#   install.packages("hutils")
#   library(hutils)
#     table(is.na(ingreso$nmiembros_ch), ingreso$pais_c)
#     ingresohog<-mutate_ntile(ingreso, "ing_hogar", n=5, weights = "factor_ch", by = "pais_c", keyby = NULL,
#                          new.col = "quintilhogar", character.only = TRUE, overwrite = TRUE,
#                          check.na = FALSE)
#   rm(ingreso,DF)
#     ingresoitpc<-mutate_ntile(ingresohog, "itpc", n=5, weights = "factor_ch", by = "pais_c", keyby = NULL,
#                           new.col = "quintilpc", character.only = TRUE, overwrite = TRUE,
#                         V  check.na = FALSE)
# 
#     ##cleanup 
#     rm(ingresohog)
# 
#Load coded file and merge

##jefes1<-merge(ingresoitpc,jefes,by=c("pais_c","idh_ch"))
##rm(jefes,ingresoitpc)
##jefes<- jefes1 %>%
##  filter(jefes_ci==1)
##  rm(jefes1,jefes)
jefes$itpcclean <-ifelse(jefes$pais_c == "JAM", NA,jefes$itpc) ##Jamaica doesnt include income information
jefes$ing_hogar_clean <-ifelse(jefes$pais_c == "JAM", NA,jefes$ing_hogar) ##Jamaica doesnt include income information
jefes$quintil 

##Generate categorical variables with preferred names
jefes$scope<-factor(jefes$ur, levels=c(1,2), labels=c("urban","rural"))
jefes$sex<-factor(jefes$sexo_ci, levels=c(1,2), labels=c("male","female"))

##Generate other dummy variables: unshared sanitation, urban, rural
jefes$sewerunshared<- ifelse(is.na(jefes$unshared),NA,
                             ifelse(jefes$sewer==1 & jefes$unshared==1,1,0))

jefes$septicunshared<- ifelse(is.na(jefes$unshared),NA,
                              ifelse(jefes$septic==1 & jefes$unshared==1,1,0))

jefes$latrinehbunshared<-ifelse(is.na(jefes$unshared)|is.na(jefes$latrinehb),NA, 
                                ifelse(jefes$latrinehb==1 & jefes$unshared==1,1,0))

jefes$latrinelbunshared<- ifelse(is.na(jefes$unshared)|is.na(jefes$latrinelb),NA, 
                                 ifelse(jefes$latrinelb==1 & jefes$unshared==1,1,0))

jefes$pipedhousedaily <- ifelse(is.na(jefes$pipedhouse)|is.na(jefes$freq),NA, 
                                ifelse(jefes$pipedhouse==1 & jefes$freq==1,1,0))
jefes$pipedplotdaily <- ifelse(is.na(jefes$pipedprem)|is.na(jefes$freq),NA, 
                               ifelse(jefes$pipedprem==1 & jefes$freq==1,1,0))


jefes$urban<- ifelse(jefes$scope=="urban",1,0)
jefes$rural<- ifelse(jefes$scope=="rural",1,0)



library("dplyr")

##summary of household data
##creation of all the tables to be "unioned"
##summary table with no extra dimensions  
summary<-jefes %>%
  group_by(pais_c, drop=TRUE) %>%
  summarize(access_water_piped_house=weighted.mean(pipedhouse, factor_ch,na.rm=TRUE),
            access_water_piped_plot=weighted.mean(pipedprem, factor_ch,na.rm=TRUE),
            access_water_other_min=weighted.mean(npimplb, factor_ch,na.rm=TRUE),
            access_water_other_max=weighted.mean(npimpub, factor_ch,na.rm=TRUE),
            access_water_daily=weighted.mean(freq, factor_ch,na.rm=TRUE),
            access_san_exclusive=weighted.mean(unshared, factor_ch,na.rm=TRUE),
            access_sewer=weighted.mean(sewer, factor_ch,na.rm=TRUE),
            access_septic=weighted.mean(septic, factor_ch,na.rm=TRUE),
            access_latrine_min=weighted.mean(latrinelb, factor_ch,na.rm=TRUE),
            access_latrine_max=weighted.mean(latrinehb, factor_ch,na.rm=TRUE),
            access_sewer_exclusive=weighted.mean(sewerunshared, factor_ch,na.rm=TRUE),
            access_septic_exclusive=weighted.mean(septicunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_min=weighted.mean(latrinelbunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_max=weighted.mean(latrinehbunshared, factor_ch,na.rm=TRUE),
            access_water_piped_house_daily=weighted.mean(pipedhousedaily, factor_ch,na.rm=TRUE),
            access_water_piped_plot_daily= weighted.mean(pipedplotdaily, factor_ch,na.rm=TRUE),
            hygiene_defecation=weighted.mean(opdef, factor_ch,na.rm=TRUE))


##delete extra fields
summary <- subset(summary, select = -c(drop))

##Add population Data 
pop <- as.data.frame(read.csv("poblacion2.csv"))
pop<- rename(pop,pop_total = Total, pop_urban = Urbana, pop_rural = Rural)
summary<- merge(summary,pop,by="pais_c")

## Add WRI data  

#add empty column for eventual union 
summary$scope<-"country"
summary$sex<- "all"
summary$quintilitpc <- NA
summary$ave_income_pc <-NA
summary$max_income_pc <-NA
summary$ave_hh_income <-NA
summary$max_hh_income <-NA
summary$ruralpc <- NA
summary$urbanpc <- NA

##table with urban/rural breakdown                
summaryurban<-jefes %>%
  group_by(pais,pais_c,scope) %>%
  summarize(access_water_piped_house=weighted.mean(pipedhouse, factor_ch,na.rm=TRUE),
            access_water_piped_plot=weighted.mean(pipedprem, factor_ch,na.rm=TRUE),
            access_water_other_min=weighted.mean(npimplb, factor_ch,na.rm=TRUE),
            access_water_other_max=weighted.mean(npimpub, factor_ch,na.rm=TRUE),
            access_water_daily=weighted.mean(freq, factor_ch,na.rm=TRUE),
            access_san_exclusive=weighted.mean(unshared, factor_ch,na.rm=TRUE),
            access_sewer=weighted.mean(sewer, factor_ch,na.rm=TRUE),
            access_septic=weighted.mean(septic, factor_ch,na.rm=TRUE),
            access_latrine_min=weighted.mean(latrinelb, factor_ch,na.rm=TRUE),
            access_latrine_max=weighted.mean(latrinehb, factor_ch,na.rm=TRUE),
            access_sewer_exclusive=weighted.mean(sewerunshared, factor_ch,na.rm=TRUE),
            access_septic_exclusive=weighted.mean(septicunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_min=weighted.mean(latrinelbunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_max=weighted.mean(latrinehbunshared, factor_ch,na.rm=TRUE),
            access_water_piped_house_daily=weighted.mean(pipedhousedaily, factor_ch,na.rm=TRUE),
            access_water_piped_plot_daily= weighted.mean(pipedplotdaily, factor_ch,na.rm=TRUE),
            hygiene_defecation=weighted.mean(opdef, factor_ch,na.rm=TRUE))

## Create Argentina Rural Row 
argentina.ruralsummary <- data.frame(17, "ARG", "rural", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)      

#Naming the Data Frame columns
names(argentina.ruralsummary) <- c("pais","pais_c","scope", "access_water_piped_house","access_water_piped_plot",     
                                   "access_water_other_min","access_water_other_max","access_water_daily","access_san_exclusive","access_sewer",               
                                   "access_septic","access_latrine_min","access_latrine_max","access_sewer_exclusive","access_septic_exclusive",     
                                   "access_latrine_exclusive_min", "access_latrine_exclusive_max", "hygiene_defecation" )  

#Using rbind() function to insert above observation  
summaryurban <- rbind(summaryurban, argentina.ruralsummary)



##delete extra fields
summaryurban <- subset(summaryurban, select = -c(pais))


##create empty fields for union
summaryurban$quintilitpc <- NA
summaryurban$sex<- "all"
summaryurban$ave_income_pc <-NA
summaryurban$max_income_pc <-NA
summaryurban$ave_hh_income <-NA
summaryurban$max_hh_income <-NA
summaryurban$pop_total <-NA
summaryurban$pop_rural <-NA
summaryurban$pop_urban <-NA
summaryurban$Country.Name <-NA
summaryurban$ruralpc <- NA
summaryurban$urbanpc <- NA

##Table with urban/rural and quintile breakdown
urbanquintilepercents<-jefes %>%
  group_by(pais_c,quintilitpc) %>%
  summarize(urbanpc=weighted.mean(urban,factor_ch/nmiembros_ch,na.rm=TRUE),
            ruralpc=weighted.mean(rural,factor_ch/nmiembros_ch,na.rm=TRUE))    

summaryurbanquintiles<-jefes %>%
  group_by(pais,pais_c,scope,quintilitpc) %>%
  summarize(ave_income_pc=weighted.mean(itpcclean, factor_ch,na.rm=TRUE),
            max_income_pc=max(itpcclean, na.rm=FALSE),
            ave_hh_income=weighted.mean(ing_hogar_clean, factor_ch,na.rm=TRUE),
            max_hh_income=max(ing_hogar_clean,na.rm=FALSE),
            access_water_piped_house=weighted.mean(pipedhouse, factor_ch,na.rm=TRUE),
            access_water_piped_plot=weighted.mean(pipedprem, factor_ch,na.rm=TRUE),
            access_water_other_min=weighted.mean(npimplb, factor_ch,na.rm=TRUE),
            access_water_other_max=weighted.mean(npimpub, factor_ch,na.rm=TRUE),
            access_water_daily=weighted.mean(freq, factor_ch,na.rm=TRUE),
            access_san_exclusive=weighted.mean(unshared, factor_ch,na.rm=TRUE),
            access_sewer=weighted.mean(sewer, factor_ch,na.rm=TRUE),
            access_septic=weighted.mean(septic, factor_ch,na.rm=TRUE),
            access_latrine_min=weighted.mean(latrinelb, factor_ch,na.rm=TRUE),
            access_latrine_max=weighted.mean(latrinehb, factor_ch,na.rm=TRUE),
            access_sewer_exclusive=weighted.mean(sewerunshared, factor_ch,na.rm=TRUE),
            access_septic_exclusive=weighted.mean(septicunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_min=weighted.mean(latrinelbunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_max=weighted.mean(latrinehbunshared, factor_ch,na.rm=TRUE),
            access_water_piped_house_daily=weighted.mean(pipedhousedaily, factor_ch,na.rm=TRUE),
            access_water_piped_plot_daily= weighted.mean(pipedplotdaily, factor_ch,na.rm=TRUE),
            hygiene_defecation=weighted.mean(opdef, factor_ch,na.rm=TRUE))
##drop extra fields
summaryurbanquintiles <- subset(summaryurbanquintiles, select = -c(pais))

## Jamaica doesnt have income data / quintiles
## Remove Jamaica calc
jamtrue <- ifelse(summaryurbanquintiles$pais_c == "JAM",TRUE,FALSE)
summaryurbanquintiles <-summaryurbanquintiles[!jamtrue,]

## Create Jam NA values
jam.quinturbansummary <- data.frame(rbind(c("JAM", "urban",1, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                          c("JAM", "urban",2, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                          c("JAM", "urban",3, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                          c("JAM", "urban",4, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                          c("JAM", "urban",5, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                          c("JAM", "rural",1, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                          c("JAM", "rural",2, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                          c("JAM", "rural",3, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                          c("JAM", "rural",4, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                          c("JAM", "rural",5, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)))


#Naming the Data Frame columns
names(jam.quinturbansummary) <- c("pais_c","scope","quintilitpc","ave_income_pc",               
                                  "max_income_pc","ave_hh_income","max_hh_income","access_water_piped_house","access_water_piped_plot",     
                                  "access_water_other_min","access_water_other_max","access_water_daily","access_san_exclusive","access_sewer",                
                                  "access_septic","access_latrine_min","access_latrine_max","access_sewer_exclusive","access_septic_exclusive",     
                                  "access_latrine_exclusive_min", "access_latrine_exclusive_max", "hygiene_defecation" )  

##make data types compatible
jam.quinturbansummary[3:22] <- sapply(jam.quinturbansummary[3:22],as.numeric)


#Using rbind() function to insert above observation  
summaryurbanquintiles <- rbind(summaryurbanquintiles, jam.quinturbansummary)



summaryurbanquintiles <- left_join(summaryurbanquintiles, urbanquintilepercents, by = c("pais_c","quintilitpc"))



## add extra fields for the union      
summaryurbanquintiles$sex<- "all"
summaryurbanquintiles$pop_total <-NA
summaryurbanquintiles$pop_rural <-NA
summaryurbanquintiles$pop_urban <-NA
summaryurbanquintiles$Country.Name <-NA


##table with quintiles          
summaryquintiles<-jefes %>%
  group_by(pais,pais_c,quintilitpc) %>%
  summarize(urbanpc=weighted.mean(urban,factor_ch/nmiembros_ch,na.rm=TRUE),
            ruralpc=weighted.mean(rural,factor_ch/nmiembros_ch,na.rm=TRUE),
            ave_income_pc=weighted.mean(itpcclean, factor_ch,na.rm=TRUE),
            max_income_pc=max(itpcclean, na.rm=FALSE),
            ave_hh_income=weighted.mean(ing_hogar_clean, factor_ch,na.rm=TRUE),
            max_hh_income=max(ing_hogar_clean,na.rm=FALSE),
            access_water_piped_house=weighted.mean(pipedhouse, factor_ch,na.rm=TRUE),
            access_water_piped_plot=weighted.mean(pipedprem, factor_ch,na.rm=TRUE),
            access_water_other_min=weighted.mean(npimplb, factor_ch,na.rm=TRUE),
            access_water_other_max=weighted.mean(npimpub, factor_ch,na.rm=TRUE),
            access_water_daily=weighted.mean(freq, factor_ch,na.rm=TRUE),
            access_san_exclusive=weighted.mean(unshared, factor_ch,na.rm=TRUE),
            access_sewer=weighted.mean(sewer, factor_ch,na.rm=TRUE),
            access_septic=weighted.mean(septic, factor_ch,na.rm=TRUE),
            access_latrine_min=weighted.mean(latrinelb, factor_ch,na.rm=TRUE),
            access_latrine_max=weighted.mean(latrinehb, factor_ch,na.rm=TRUE),
            access_sewer_exclusive=weighted.mean(sewerunshared, factor_ch,na.rm=TRUE),
            access_septic_exclusive=weighted.mean(septicunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_min=weighted.mean(latrinelbunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_max=weighted.mean(latrinehbunshared, factor_ch,na.rm=TRUE),
            access_water_piped_house_daily=weighted.mean(pipedhousedaily, factor_ch,na.rm=TRUE),
            access_water_piped_plot_daily= weighted.mean(pipedplotdaily, factor_ch,na.rm=TRUE),
            hygiene_defecation=weighted.mean(opdef, factor_ch,na.rm=TRUE))

summaryquintiles <- subset(summaryquintiles, select = -c(pais))

## Remove Jamaica calc
jamquinttrue <- ifelse(summaryquintiles$pais_c == "JAM",TRUE,FALSE)
summaryquintiles <-summaryquintiles[!jamquinttrue,]

## Create Jam NA values
jam.quintsummary <- data.frame(rbind(c("JAM", 1, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                     c("JAM",2, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                     c("JAM",3, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                     c("JAM",4, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                                     c("JAM",5, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)))


#Naming the Data Frame columns
names(jam.quintsummary) <- c("pais_c","quintilitpc","urbanpc","ruralpc",
                             "ave_income_pc","max_income_pc","ave_hh_income","max_hh_income","access_water_piped_house",
                             "access_water_piped_plot","access_water_other_min","access_water_other_max","access_water_daily","access_san_exclusive",        
                             "access_sewer","access_septic","access_latrine_min","access_latrine_max","access_sewer_exclusive" ,     
                             "access_septic_exclusive","access_latrine_exclusive_min", "access_latrine_exclusive_max", "hygiene_defecation")  

##make data types compatible
jam.quintsummary[2:23] <- sapply(jam.quintsummary[2:23],as.numeric)


#Using rbind() function to insert above observation  
summaryquintiles <- rbind(summaryquintiles, jam.quintsummary)








##Add empty fields
summaryquintiles$sex<- "all"
summaryquintiles$scope <- "country"
summaryquintiles$pop_total <-NA
summaryquintiles$pop_rural <-NA
summaryquintiles$pop_urban <-NA
summaryquintiles$Country.Name <-NA

##table with sex breakdown
summarysexo<-jefes %>%
  group_by(pais,pais_c,sex) %>%
  summarize(urbanpc=weighted.mean(urban,factor_ch/nmiembros_ch,na.rm=TRUE),
            ruralpc=weighted.mean(rural,factor_ch/nmiembros_ch,na.rm=TRUE),
            access_water_piped_house=weighted.mean(pipedhouse, factor_ch,na.rm=TRUE),
            access_water_piped_plot=weighted.mean(pipedprem, factor_ch,na.rm=TRUE),
            access_water_other_min=weighted.mean(npimplb, factor_ch,na.rm=TRUE),
            access_water_other_max=weighted.mean(npimpub, factor_ch,na.rm=TRUE),
            access_water_daily=weighted.mean(freq, factor_ch,na.rm=TRUE),
            access_san_exclusive=weighted.mean(unshared, factor_ch,na.rm=TRUE),
            access_sewer=weighted.mean(sewer, factor_ch,na.rm=TRUE),
            access_septic=weighted.mean(septic, factor_ch,na.rm=TRUE),
            access_latrine_min=weighted.mean(latrinelb, factor_ch,na.rm=TRUE),
            access_latrine_max=weighted.mean(latrinehb, factor_ch,na.rm=TRUE),
            access_sewer_exclusive=weighted.mean(sewerunshared, factor_ch,na.rm=TRUE),
            access_septic_exclusive=weighted.mean(septicunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_min=weighted.mean(latrinelbunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_max=weighted.mean(latrinehbunshared, factor_ch,na.rm=TRUE),
            access_water_piped_house_daily=weighted.mean(pipedhousedaily, factor_ch,na.rm=TRUE),
            access_water_piped_plot_daily= weighted.mean(pipedplotdaily, factor_ch,na.rm=TRUE),
            hygiene_defecation=weighted.mean(opdef, factor_ch,na.rm=TRUE))
##  Add extra fields for union
summarysexo$scope <- "country"
summarysexo$quintilitpc <- NA
summarysexo$ave_income_pc <-NA
summarysexo$max_income_pc <-NA
summarysexo$ave_hh_income <-NA
summarysexo$max_hh_income <-NA
summarysexo$pop_total <-NA
summarysexo$pop_rural <-NA
summarysexo$pop_urban <-NA
summarysexo$Country.Name <-NA
## drop unneeded fields
summarysexo <- subset(summarysexo, select = -c(pais))

##table of data broken down by sex and urban/rural
summarysexourbano<-jefes %>%
  group_by(pais,pais_c,scope,sex) %>%
  summarize(access_water_piped_house=weighted.mean(pipedhouse, factor_ch,na.rm=TRUE),
            access_water_piped_plot=weighted.mean(pipedprem, factor_ch,na.rm=TRUE),
            access_water_other_min=weighted.mean(npimplb, factor_ch,na.rm=TRUE),
            access_water_other_max=weighted.mean(npimpub, factor_ch,na.rm=TRUE),
            access_water_daily=weighted.mean(freq, factor_ch,na.rm=TRUE),
            access_san_exclusive=weighted.mean(unshared, factor_ch,na.rm=TRUE),
            access_sewer=weighted.mean(sewer, factor_ch,na.rm=TRUE),
            access_septic=weighted.mean(septic, factor_ch,na.rm=TRUE),
            access_latrine_min=weighted.mean(latrinelb, factor_ch,na.rm=TRUE),
            access_latrine_max=weighted.mean(latrinehb, factor_ch,na.rm=TRUE),
            access_sewer_exclusive=weighted.mean(sewerunshared, factor_ch,na.rm=TRUE),
            access_septic_exclusive=weighted.mean(septicunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_min=weighted.mean(latrinelbunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_max=weighted.mean(latrinehbunshared, factor_ch,na.rm=TRUE),
            access_water_piped_house_daily=weighted.mean(pipedhousedaily, factor_ch,na.rm=TRUE),
            access_water_piped_plot_daily= weighted.mean(pipedplotdaily, factor_ch,na.rm=TRUE),
            hygiene_defecation=weighted.mean(opdef, factor_ch,na.rm=TRUE))



##  Add extra fields for union
summarysexourbano$quintilitpc <- NA
summarysexourbano$ave_income_pc <-NA
summarysexourbano$max_income_pc <-NA
summarysexourbano$ave_hh_income <-NA
summarysexourbano$max_hh_income <-NA
summarysexourbano$pop_total <-NA
summarysexourbano$pop_rural <-NA
summarysexourbano$pop_urban <-NA
summarysexourbano$Country.Name <-NA
summarysexourbano$ruralpc <- NA
summarysexourbano$urbanpc <- NA
## drop unneeded fields
summarysexourbano <- subset(summarysexourbano, select = -c(pais))

##table of data broken down by sex, urban/rural and quintiles
summarysexourbanoquintil<-jefes %>%
  group_by(pais,pais_c,scope,sex, quintilitpc) %>%
  summarize(
    ave_income_pc=weighted.mean(itpcclean, factor_ch,na.rm=TRUE),
    max_income_pc=max(itpcclean, na.rm=FALSE),
    ave_hh_income=weighted.mean(ing_hogar_clean, factor_ch,na.rm=TRUE),
    max_hh_income=max(ing_hogar_clean,na.rm=FALSE),
    access_water_piped_house=weighted.mean(pipedhouse, factor_ch,na.rm=TRUE),
    access_water_piped_plot=weighted.mean(pipedprem, factor_ch,na.rm=TRUE),
    access_water_other_min=weighted.mean(npimplb, factor_ch,na.rm=TRUE),
    access_water_other_max=weighted.mean(npimpub, factor_ch,na.rm=TRUE),
    access_water_daily=weighted.mean(freq, factor_ch,na.rm=TRUE),
    access_san_exclusive=weighted.mean(unshared, factor_ch,na.rm=TRUE),
    access_sewer=weighted.mean(sewer, factor_ch,na.rm=TRUE),
    access_septic=weighted.mean(septic, factor_ch,na.rm=TRUE),
    access_latrine_min=weighted.mean(latrinelb, factor_ch,na.rm=TRUE),
    access_latrine_max=weighted.mean(latrinehb, factor_ch,na.rm=TRUE),
    access_sewer_exclusive=weighted.mean(sewerunshared, factor_ch,na.rm=TRUE),
    access_septic_exclusive=weighted.mean(septicunshared, factor_ch,na.rm=TRUE),
    access_latrine_exclusive_min=weighted.mean(latrinelbunshared, factor_ch,na.rm=TRUE),
    access_latrine_exclusive_max=weighted.mean(latrinehbunshared, factor_ch,na.rm=TRUE),
    access_water_piped_house_daily=weighted.mean(pipedhousedaily, factor_ch,na.rm=TRUE),
    access_water_piped_plot_daily= weighted.mean(pipedplotdaily, factor_ch,na.rm=TRUE),
    hygiene_defecation=weighted.mean(opdef, factor_ch,na.rm=TRUE))

## drop unneeded fields
summarysexourbanoquintil <- subset(summarysexourbanoquintil, select = -c(pais))


## Remove Jamaica calc
jamquinturbsextrue <- ifelse(summarysexourbanoquintil$pais_c == "JAM",TRUE,FALSE)
summarysexourbanoquintil <-summarysexourbanoquintil[!jamquinturbsextrue,]

## Create Jam NA values
jam.quinturbsexsummary <- data.frame(rbind(
  c("JAM","urban", "male", 1, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","urban", "male", 2, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","urban", "male", 3, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","urban", "male", 4, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","urban", "male", 5, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  
  c("JAM","rural", "male", 1, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","rural", "male", 2, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","rural", "male", 3, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","rural", "male", 4, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","rural", "male", 5, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  
  
  c("JAM","urban", "female", 1, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","urban", "female", 2, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","urban", "female", 3, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","urban", "female", 4, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","urban", "female", 5, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  
  c("JAM","rural", "female", 1, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","rural", "female", 2, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","rural", "female", 3, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","rural", "female", 4, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM","rural", "female", 5, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
))


#Naming the Data Frame columns
names(jam.quinturbsexsummary) <- c("pais_c","scope","sex","quintilitpc",              
                                   "ave_income_pc","max_income_pc","ave_hh_income","max_hh_income","access_water_piped_house",
                                   "access_water_piped_plot","access_water_other_min","access_water_other_max","access_water_daily","access_san_exclusive",        
                                   "access_sewer","access_septic","access_latrine_min","access_latrine_max","access_sewer_exclusive",
                                   "access_septic_exclusive","access_latrine_exclusive_min", "access_latrine_exclusive_max", "hygiene_defecation" )  

##make data types compatible
jam.quinturbsexsummary[4:23] <- sapply(jam.quinturbsexsummary[4:23],as.numeric)


#Using rbind() function to insert above observation  
summarysexourbanoquintil <- rbind(summarysexourbanoquintil, jam.quinturbsexsummary)


##add empty fields

summarysexourbanoquintil$pop_total <-NA
summarysexourbanoquintil$pop_rural <-NA
summarysexourbanoquintil$pop_urban <-NA
summarysexourbanoquintil$Country.Name <-NA
summarysexourbanoquintil$ruralpc <- NA
summarysexourbanoquintil$urbanpc <- NA




##Table with data broken down by sex and quintile
summarysexoquintil<-jefes %>%
  group_by(pais,pais_c,quintilitpc,sex) %>%
  summarize(ave_income_pc=weighted.mean(itpcclean, factor_ch,na.rm=TRUE),
            max_income_pc=max(itpcclean, na.rm=FALSE),
            ave_hh_income=weighted.mean(ing_hogar_clean, factor_ch,na.rm=TRUE),
            max_hh_income=max(ing_hogar_clean,na.rm=FALSE),
            access_water_piped_house=weighted.mean(pipedhouse, factor_ch,na.rm=TRUE),
            access_water_piped_plot=weighted.mean(pipedprem, factor_ch,na.rm=TRUE),
            access_water_other_min=weighted.mean(npimplb, factor_ch,na.rm=TRUE),
            access_water_other_max=weighted.mean(npimpub, factor_ch,na.rm=TRUE),
            access_water_daily=weighted.mean(freq, factor_ch,na.rm=TRUE),
            access_san_exclusive=weighted.mean(unshared, factor_ch,na.rm=TRUE),
            access_sewer=weighted.mean(sewer, factor_ch,na.rm=TRUE),
            access_septic=weighted.mean(septic, factor_ch,na.rm=TRUE),
            access_latrine_min=weighted.mean(latrinelb, factor_ch,na.rm=TRUE),
            access_latrine_max=weighted.mean(latrinehb, factor_ch,na.rm=TRUE),
            access_sewer_exclusive=weighted.mean(sewerunshared, factor_ch,na.rm=TRUE),
            access_septic_exclusive=weighted.mean(septicunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_min=weighted.mean(latrinelbunshared, factor_ch,na.rm=TRUE),
            access_latrine_exclusive_max=weighted.mean(latrinehbunshared, factor_ch,na.rm=TRUE),
            access_water_piped_house_daily=weighted.mean(pipedhousedaily, factor_ch,na.rm=TRUE),
            access_water_piped_plot_daily= weighted.mean(pipedplotdaily, factor_ch,na.rm=TRUE),
            hygiene_defecation=weighted.mean(opdef, factor_ch,na.rm=TRUE))
#drop uneeded fields
summarysexoquintil <- subset(summarysexoquintil, select = -c(pais))


### Jamaica doesnt have quintile values, remove rows and replace with NAs    
jamsexquinttrue <- ifelse(summarysexoquintil$pais_c == "JAM",TRUE,FALSE)
summarysexoquintil <-summarysexoquintil[!jamsexquinttrue,]

## Create Jam NA values
jam.sexquintsummary <- data.frame(rbind(
  c("JAM", 1, "male", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM",2, "male", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM",3, "male", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM",4, "male", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM",5, "male", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM", 1, "female", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM",2, "female", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM",3, "female", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM",4, "female", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  c("JAM",5, "female", NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)))


#Naming the Data Frame columns
names(jam.sexquintsummary) <- c("pais_c",                       "quintilitpc",                  "sex",                          "ave_income_pc",                "max_income_pc",               
                                "ave_hh_income",                "max_hh_income",                "access_water_piped_house",     "access_water_piped_plot",      "access_water_other_min",      
                                "access_water_other_max",       "access_water_daily",           "access_san_exclusive",         "access_sewer",                 "access_septic",               
                                "access_latrine_min",           "access_latrine_max",           "access_sewer_exclusive",       "access_septic_exclusive",      "access_latrine_exclusive_min",
                                "access_latrine_exclusive_max", "hygiene_defecation"       )  

##make data types compatible
jam.sexquintsummary$quintilitpc <- as.numeric(jam.sexquintsummary$quintilitpc)
jam.sexquintsummary[4:22] <- sapply(jam.sexquintsummary[4:22],as.numeric)


#Using rbind() function to insert above observation  
summarysexoquintil <- rbind(summarysexoquintil, jam.sexquintsummary)




##add blank fields for union

summarysexoquintil$scope <- "country"
summarysexoquintil$pop_total <-NA
summarysexoquintil$pop_rural <-NA
summarysexoquintil$pop_urban <-NA
summarysexoquintil$Country.Name <-NA
summarysexoquintil$ruralpc <- NA
summarysexoquintil$urbanpc <- NA



##union of all tables

dftotal <- union(summary, summaryurban) 
dftotal2 <- union(summaryquintiles, summaryurbanquintiles)
sum1 <-union(dftotal,dftotal2)
dftotal3 <- union(summarysexo, summarysexourbano)
dftotal4 <- union(summarysexoquintil, summarysexourbanoquintil)
sum2 <-union(dftotal3, dftotal4)

##final draft dataset
finaldraft <- union(sum1, sum2)
## Tidying it up
##changing quintile to string and adding "total"
finaldraft$quintile <-as.character(finaldraft$quintilitpc)
finaldraft$quintile <- ifelse(is.na(finaldraft$quintile),"total",finaldraft$quintile)
##dropping extra fields
finaldraft<- subset(finaldraft, select = -c(quintilitpc, Country.Name))
##generating key-field

## add pop data to calculate population_prc
pop <- as.data.frame(read.csv("poblacion2.csv"))
pop<- rename(pop,pop_total = Total, pop_urban = Urbana, pop_rural = Rural)
pop$prcurban <- pop$pop_urban/pop$pop_total
pop$pop_rural <- as.numeric(pop$pop_rural)
pop$prcrural <- pop$pop_rural/pop$pop_total
finaldraft <-left_join(finaldraft, pop, c("pais_c" = "pais_c"))
finaldraft$quintilepercent <-.2
finaldraft$population <- ifelse(finaldraft$scope == "country"  & finaldraft$sex == "all" & finaldraft$quintile == "total",finaldraft$pop_total.x,NA)
finaldraft$population_prc <- ifelse(finaldraft$scope == "urban" & finaldraft$sex == "all" & finaldraft$quintile == "total", finaldraft$prcurban, 
                                    ifelse(finaldraft$scope == "rural" & finaldraft$sex == "all" & finaldraft$quintile == "total", finaldraft$prcrural,
                                           ifelse(finaldraft$scope == "country"  & finaldraft$sex == "all" & finaldraft$quintile == "total", 1,
                                                  ifelse(finaldraft$scope=="country"& finaldraft$sex=="all" & finaldraft$quintile != "total", .2 ,
                                                         ifelse(finaldraft$pais_c =="JAM" & finaldraft$scope!="country"& finaldraft$sex=="all" & finaldraft$quintile != "total",NA,
                                                           ifelse(finaldraft$scope == "urban" & finaldraft$sex == "all" & finaldraft$quintile != "total", finaldraft$urbanpc*finaldraft$quintilepercent,
                                                                ifelse(finaldraft$scope == "rural" & finaldraft$sex == "all" & finaldraft$quintile != "total", finaldraft$ruralpc*finaldraft$quintilepercent,NA)))))))



##unimproved latrine = 1-improved latrine-open def-sewer-septic
finaldraft$access_latrines_unimproved_min <- 1- (finaldraft$access_latrine_max + finaldraft$hygiene_defecation + finaldraft$access_sewer + finaldraft$access_septic)
finaldraft$access_latrines_unimproved_exclusive_min <- finaldraft$access_san_exclusive - (finaldraft$access_sewer_exclusive+finaldraft$access_septic_exclusive+finaldraft$access_latrine_exclusive_max)




finaldraft$access_water <- ifelse(is.na(finaldraft$access_water_piped_house),0,finaldraft$access_water_piped_house) + finaldraft$access_water_other_max + finaldraft$access_water_piped_plot
finaldraft <- relocate(finaldraft, pais_c, scope, quintile, sex,population, population_prc,ave_income_pc,	max_income_pc,	ave_hh_income,	max_hh_income)



## improved latrine data availability metric
##finaldraft$improved_latrine_da<-ifelse(finaldraft$access_latrine_max-finaldraft$access_latrine_min == 0,1,NA)


## improved non-piped water access
##finaldraft$improved_nonpiped_da<-ifelse(finaldraft$access_water_other_max-finaldraft$access_water_other_min == 0,1,NA)


##final (renaming columns)
finaldraft <- rename(finaldraft, iso = pais_c, gender = sex, population_perc = population_prc)
final <- subset(finaldraft, select = -c(pop_total.x,	pop_urban.x,	
                                        pop_rural.x,ruralpc,	urbanpc,	Country.Name,	pop_total.y,	
                                        pop_urban.y,	pop_rural.y,	prcurban,	prcrural,quintilepercent))


##replace all NaNs with NA
final$access_water_piped_house[is.nan(final$access_water_piped_house)]<-NA
final$access_water_piped_house_daily[is.nan(final$access_water_piped_house_daily)]<-NA
final$access_water_piped_plot_daily[is.nan(final$access_water_piped_plot_daily)]<-NA
final$access_water_daily[is.nan(final$access_water_daily)]<-NA
final$access_san_exclusive[is.nan(final$access_san_exclusive)]<-NA
final$access_sewer_exclusive[is.nan(final$access_sewer_exclusive)]<-NA
final$access_septic_exclusive[is.nan(final$access_septic_exclusive)]<-NA
final$access_latrine_min[is.nan(final$access_latrine_min)]<-NA
final$access_latrine_max[is.nan(final$access_latrine_max)]<-NA
final$access_latrine_exclusive_min[is.nan(final$access_latrine_exclusive_min)]<-NA
final$access_latrine_exclusive_max[is.nan(final$access_latrine_exclusive_max)]<-NA
final$access_water_piped_plot[is.nan(final$access_water_piped_plot)]<-NA
final$access_latrines_unimproved_min[is.nan(final$access_latrines_unimproved_min)]<-NA
final$access_latrines_unimproved_exclusive_min[is.nan(final$access_latrines_unimproved_exclusive_min)]<-NA
final$access_water[is.nan(final$access_water)]<-NA
final$access_sewer[is.nan(final$access_sewer)]<-NA
final$ave_income_pc[is.nan(final$ave_income_pc)]<-NA
final$ave_hh_income[is.nan(final$ave_hh_income)]<-NA


#Calculate data availability
dadummy <- final
dadummy$access_water<- ifelse(is.na(final$access_water), 0, 1)
dadummy$access_water_piped_house<- ifelse(is.na(final$access_water_piped_house), 0, 1)
dadummy$access_water_piped_plot<- ifelse(is.na(final$access_water_piped_plot), 0, 1)
dadummy$access_water_other_min<- ifelse(is.na(final$access_water_other_min), 0, 1)
dadummy$access_water_other_max<- ifelse(is.na(final$access_water_other_max), 0, 1)
dadummy$access_water_daily<- ifelse(is.na(final$access_water_daily), 0, 1)
dadummy$access_sewer <- ifelse(is.na(final$access_sewer), 0, 1)
dadummy$access_septic<- ifelse(is.na(final$access_septic), 0, 1)
dadummy$access_latrine_min<- ifelse(is.na(final$access_latrine_min), 0, 1)
dadummy$access_latrine_max<- ifelse(is.na(final$access_latrine_max), 0, 1)
dadummy$access_san_exclusive <- ifelse(is.na(final$access_san_exclusive), 0, 1)
dadummy$access_sewer_exclusive <- ifelse(is.na(final$access_sewer_exclusive), 0, 1)
dadummy$access_septic_exclusive<- ifelse(is.na(final$access_septic_exclusive), 0, 1)
dadummy$access_latrine_exclusive_min <- ifelse(is.na(final$access_latrine_exclusive_min), 0, 1)
dadummy$access_latrine_exclusive_max<- ifelse(is.na(final$access_latrine_exclusive_max), 0, 1)
dadummy$access_latrines_unimproved_exclusive_min <- ifelse(is.na(final$access_latrines_unimproved_exclusive_min), 0, 1)
dadummy$access_latrines_unimproved_min<- ifelse(is.na(final$access_latrines_unimproved_min), 0, 1)
dadummy$hygiene_defecation<- ifelse(is.na(final$hygiene_defecation), 0, 1)

dataavailability<-dadummy %>%
  group_by(scope) %>%
  summarize(access_water = mean(access_water),
            access_water_piped_house = mean(access_water_piped_house),
            access_water_piped_plot = mean(  access_water_piped_plot),
            access_water_other_min = mean( access_water_other_min),
            access_water_other_max = mean( access_water_other_max),
            access_water_daily = mean( access_water_daily),
            access_sewer = mean( access_sewer),
            access_septic = mean( access_septic),
            access_latrine_min = mean( access_latrine_min),
            access_latrine_max = mean( access_latrine_max),
            access_san_exclusive = mean( access_san_exclusive),
            access_sewer_exclusive = mean( access_sewer_exclusive),
            access_septic_exclusive = mean( access_septic_exclusive),
            access_latrine_exclusive_min = mean( access_latrine_exclusive_min),
            access_latrine_exclusive_max = mean( access_latrine_exclusive_max),
            access_latrines_unimproved_exclusive_min = mean( access_latrines_unimproved_exclusive_min),
            access_latrines_unimproved_min = mean( access_latrines_unimproved_min),
            hygiene_defecation = mean( hygiene_defecation))

## create validation dataset
v <- ifelse(final$scope == "country"&final$gender =="all" &final$quintile == "total", TRUE, FALSE)
validationdata <-final[v,]
write.csv(validationdata, file ="validation/data_validation.csv", row.names = FALSE)

###split by country and export
## add data handling for latrines ub&lb

write.csv(final, file ="countrydata/OLAS_Country_Surveys.csv", row.names = FALSE)
write.csv(dataavailability, file ="validation/data_availability.csv", row.names = FALSE)


z<-split(final,final$iso)
m <- length(z)
for(i in 1:m){
  write.csv(z[[i]], file = 
              paste0("countrydata/",names(z[i]),".csv"), row.names = FALSE)
}

