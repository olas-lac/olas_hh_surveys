###clean OLAS code
rm(list=ls())
load("jefe231020.RDA")


################### Generate Dummy Variables for variables of interest ##########################
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
jefes$npimpub<- ifelse(jefes$pais_c=="ARG" & jefes$iv7%in%c(2,3,4),1, #ARG ##correcto porque el destino da igual, no ponemos otra ya que puede haber protegidas en otra no es claro
                       ifelse(jefes$pais_c=="BOL" & jefes$s01a_10%in%c(3,4,5,6,7,9),1, #BOL bolivia clasifica bien
                              ifelse(jefes$pais_c=="BRA" &jefes$s01007%in%c(2,3,4,5),1, ##BRA no ponemos otra ya que ahi ir?an rios etc, no hay muchas protegidas ahi
                                     ifelse(jefes$pais_c=="CHL" & jefes$v20%in%c(4),1, ##CHILE otra especifica y en general no eran protegidas
                                            ifelse(jefes$pais_c=="COL" & jefes$p5050%in%c(3,4,5,6,7),1,##COL solo esas pueden ser pero dudas en 3,4 y 6
                                                   ifelse(jefes$pais_c=="CRI" & jefes$v12%in%c(5,6,7),1, ##cr incluye otro 
                                                          ifelse(jefes$pais_c=="DOM" &jefes$donde_proviene_agua%in%c(4,5,6,7,8),1, ##dOM incluye tubo en la calle 5,manantial, rio o arroyo y pozo
                                                                 ifelse(jefes$pais_c=="ECU" &jefes$vi16%in%c(2,3,5),1, ##ECU incluir otra fuente por tuber?a
                                                                        ifelse(jefes$pais_c=="GTM" &jefes$p02b03%in%c(3,4,5,7),1, #GUATE 4 es pozo y 5 lago,rio o manantial
                                                                               ifelse(jefes$pais_c=="HND" & jefes$dv105%in%c(2,3,4,5),1, #honduras correcto
                                                                                      ifelse(jefes$pais_c=="JAM" & jefes$i27%in%c(3,4,5,6,7),1, #jam incluye pozos 4 y 5 spring and river
                                                                                             ifelse(jefes$pais_c=="MEX" &jefes$disp_agua%in%c(3,4,7),1, #mex 7 engloba pozo,rio, arroyo
                                                                                                    ifelse(jefes$pais_c=="NIC" & jefes$s1p15%in%c(3,4,5),1, #nic 4 es pozo sin especificacion
                                                                                                           ifelse(jefes$pais_c=="PAN" & jefes$v1i_agua_b%in%c(4,6),1, #pan does classify protected unprotected
                                                                                                                  ifelse(jefes$pais_c=="PER" & jefes$t110%in%c(3,5,6),1, #peru no incluye otros
                                                                                                                         ifelse(jefes$pais_c=="PRY" & jefes$v06%in%c(5,6,7,8,10),1, #PRY los pozos no 5 6 y 7 no queda claro si son o no mejorados
                                                                                                                                ifelse(jefes$pais_c=="SLV" & jefes$r313%in%c(4.1,5,5.1,8,10,12),1, #svy does classify properly
                                                                                                                                       ifelse(jefes$pais_c=="URY" & jefes$d11%in%c(3,4),1,0)))))))))))))))))) #uruguay clasifica bien


### non piped improved lower bound 
jefes$npimplb<- ifelse(jefes$pais_c=="ARG" & jefes$iv7%in%c(2,3),1, #elseARG diferencia es otro
                       ifelse(jefes$pais_c=="BOL" & jefes$s01a_10%in%c(3,4,5,6,7,9),1, #BOL correctamente clasificado
                              ifelse(jefes$pais_c=="BRA" & jefes$s01007%in%c(4,5),1, ##BRA los pozos no especificados
                                     ifelse(jefes$pais_c=="CHL",0, ##CHILE en todos engloba cosas que no
                                            ifelse(jefes$pais_c=="COL" & jefes$p5050%in%c(5,7),1,##COL 5 y 7 siempre son las otras engloban cosas que no
                                                   ifelse(jefes$pais_c=="CRI",0, ##cr correcto
                                                          ifelse(jefes$pais_c=="DOM" & jefes$donde_proviene_agua%in%c(4,5,7),1,##dOM sin rio y arroyo y pozos
                                                                 ifelse(jefes$pais_c=="ECU" & jefes$vi16%in%c(2),1,  ##ECU sin fuentes englobadas que no distinguen solo llave publica
                                                                        ifelse(jefes$pais_c=="GTM" & jefes$p02b03%in%c('3','7'),1, #GUATE solo pila publica y agua de lluvia
                                                                               ifelse(jefes$pais_c=="HND" & jefes$dv105%in%c(4),1, #honduras unico termino que no engloba
                                                                                      ifelse(jefes$pais_c=="JAM" & jefes$i27%in%c(3,6,7),1, #jam solo public y agua de lluvia
                                                                                             ifelse(jefes$pais_c=="MEX" & jefes$disp_agua%in%c(3,4),1, #mex correcto
                                                                                                    ifelse(jefes$pais_c=="NIC" & jefes$s1p15%in%c(3,5),1, #nic pozo puede no ser mejorado
                                                                                                           ifelse(jefes$pais_c=="PAN" & jefes$v1i_agua_b%in%c(4,6),1, #pan does calssify protected unprotected
                                                                                                                  ifelse(jefes$pais_c=="PER" & jefes$t110%in%c(3,6),1, #peru correcto
                                                                                                                         ifelse(jefes$pais_c=="PRY" & jefes$v06%in%c(8,10),1, #PRY correcto
                                                                                                                                ifelse(jefes$pais_c=="SLV" & jefes$r313%in%c(4.1,5,5.1,8,10,12),1, #svy does classify properly
                                                                                                                                       ifelse(jefes$pais_c=="URY" & jefes$d11%in%c(3,4),1,0)))))))))))))))))) #uruguay clasifica bien




##Water 7 days a week....countries not listed do not include question
#Frequency
jefes$freq<- ifelse(jefes$pais_c == "ARG",NA, ##ARG doesnt ask about frequency
                    ifelse(jefes$pais_c=="BOL" & jefes$s01a_11b%in%c(7),1, #BOL
                           ifelse(jefes$pais_c=="BRA" & jefes$s01008%in%c(1),1, ##BRA
                                  ifelse(jefes$pais_c=="CHL",NA, ##CHILE doesnt ask about frequency
                                         ifelse(jefes$pais_c=="COL" & jefes$p4040%in%c(1),1, #col
                                                ifelse(jefes$pais_c=="CRI",NA, ##cr doesnt ask about frequency
                                                       ifelse(jefes$pais_c=="DOM",NA, ##DOM doesnt ask about frequency
                                                              ifelse(jefes$pais_c=="ECU",NA, ##ECU doesnt ask about frequency
                                                                     ifelse(jefes$pais_c=="GTM",NA, ##GTM doesnt ask about frequency
                                                                            ifelse(jefes$pais_c=="HND",NA, ##HND doesnt ask about frequency
                                                                                   ifelse(jefes$pais_c=="JAM",NA, ##JAM doesnt ask about frequency
                                                                                          ifelse(jefes$pais_c=="MEX" & jefes$dotac_agua%in%c(1),1, #mex
                                                                                                 ifelse(jefes$pais_c=="NIC",NA, ##NIC doesnt ask about frequency
                                                                                                        ifelse(jefes$pais_c=="PAN"& jefes$v1j1_veran == 7 & jefes$v1j1_invie == 7, 1,##PAN does ask about frequency, need to review.
                                                                                                               ifelse(jefes$pais_c=="PER" & jefes$p110c%in%c(1),1, #peru
                                                                                                                      ifelse(jefes$pais_c=="PRY",NA, ##PRY does ask about frequency, but only hours per day, does not ask about days
                                                                                                                             ifelse(jefes$pais_c=="SLV" & jefes$r312d%in%c(7),1, #SVD
                                                                                                                                    ifelse(jefes$pais_c=="URY",NA,0)))))))))))))))))) #Uruguay doesnt ask about frequency


##R: If there isnt a question about exclusive, NA
##Sanitation
###Unshared bathroom
jefes$unshared<- ifelse(jefes$pais_c=="ARG" & jefes$ii9%in%c(1),1, #ARG
                        ifelse(jefes$pais_c=="BOL" & jefes$s01a_17%in%c(1),1, #BOL
                               ifelse(jefes$pais_c=="BRA" & jefes$s01011a%in%c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),1, ##BRA
                                      ifelse(jefes$pais_c%in%c("CHL"),NA, ##CHILE ##CHANGE TO NA
                                             ifelse(jefes$pais_c=="COL" & jefes$p5030%in%c(1),1,##COL
                                                    ifelse(jefes$pais_c=="CRI" & jefes$v13b%in%c(1),1, ##cr
                                                           ifelse(jefes$pais_c=="DOM" & jefes$tipo_sanitario%in%c(1,3),1, ##dOM
                                                                  ifelse(jefes$pais_c=="ECU" & jefes$vi15%in%c(2),1, ##ECU
                                                                         ifelse(jefes$pais_c%in%c("GTM"),NA, #GUATE
                                                                                ifelse(jefes$pais_c=="HND" & jefes$dh206%in%c(1),1, #honduras
                                                                                       ifelse(jefes$pais_c=="JAM" & jefes$i6%in%c(1),1, #jam
                                                                                              ifelse(jefes$pais_c=="MEX" & jefes$uso_compar%in%c(2),1, #mex
                                                                                                     ifelse(jefes$pais_c%in%c("NIC"),NA, #nic
                                                                                                            ifelse(jefes$pais_c=="PAN" & jefes$v1l_uso_sa%in%c(1),1, #pan
                                                                                                                   ifelse(jefes$pais_c%in%c("PER"),NA, #peru
                                                                                                                          ifelse(jefes$pais_c%in%c("PRY"),NA, #PRY
                                                                                                                                 ifelse(jefes$pais_c=="SLV" & jefes$r318%in%c(2),1, #svy does classify properly
                                                                                                                                        ifelse(jefes$pais_c=="URY" & jefes$d15%in%c(1),1,0))))))))))))))))))



#Sewer
jefes$sewer <- ifelse(jefes$pais_c=="ARG" & jefes$iv11%in%c(1),1, #ARG
                      ifelse(jefes$pais_c=="BOL" & jefes$s01a_16%in%c(1),1, #BOL
                             ifelse(jefes$pais_c=="BRA" & jefes$s01012a%in%c(1),1, ##BRA
                                    ifelse(jefes$pais_c=="CHL" & jefes$v23%in%c(1),1, ##CHILE
                                           ifelse(jefes$pais_c=="COL" & jefes$p5020%in%c(1),1,##COL
                                                  ifelse(jefes$pais_c=="CRI" & jefes$v13a%in%c(1),1, ##cr
                                                         ifelse(jefes$pais_c=="DOM" & jefes$se_encuentra_conectada_a%in%c(2),1, ##dOM
                                                                ifelse(jefes$pais_c=="ECU" & jefes$vi13%in%c(1),1, ##ECU
                                                                       ifelse(jefes$pais_c=="GTM" & jefes$p02b07%in%c(1),1, #GUATE
                                                                              ifelse(jefes$pais_c=="HND" & jefes$dh205%in%c(1),1, #honduras
                                                                                     ifelse(jefes$pais_c=="JAM" & jefes$i5%in%c(1),1, #jam
                                                                                            ifelse(jefes$pais_c=="MEX" & jefes$drenaje%in%c(1),1, #mex
                                                                                                   ifelse(jefes$pais_c=="NIC" & jefes$s1p18%in%c(3),1, #nic
                                                                                                          ifelse(jefes$pais_c%in%c("PAN"),NA, #not separated from septic
                                                                                                                 ifelse(jefes$pais_c=="PER" & jefes$t111a%in%c(1,2),1, #peru
                                                                                                                        ifelse(jefes$pais_c=="PRY" & jefes$v13%in%c(1),1, #PRY
                                                                                                                               ifelse(jefes$pais_c=="SLV" & jefes$r317%in%c(1,3),1, #svy does classify properly
                                                                                                                                      ifelse(jefes$pais_c=="URY" & jefes$d16%in%c(1),1,0))))))))))))))))))

##R: If the country doesnt differentiate between the sewer and septic, all households are put in septic
##Septic        
jefes$septic <- ifelse(jefes$pais_c=="ARG" & jefes$iv11%in%c(2),1, #ARG
                       ifelse(jefes$pais_c=="BOL" & jefes$s01a_16%in%c(2),1, #BOL
                              ifelse(jefes$pais_c=="BRA" & jefes$s01012a%in%c(2,3),1, ##BRA
                                     ifelse(jefes$pais_c=="CHL" & jefes$v23%in%c(2),1, ##CHILE
                                            ifelse(jefes$pais_c=="COL" & jefes$p5020%in%c(2),1,##COL
                                                   ifelse(jefes$pais_c=="CRI" & jefes$v13a%in%c(2,3),1, ##cr
                                                          ifelse(jefes$pais_c=="DOM" & jefes$se_encuentra_conectada_a%in%c(1),1, ##dOM
                                                                 ifelse(jefes$pais_c=="ECU" & jefes$vi13%in%c(2),1, ##ECU//jmp suma cxon 3 tmb
                                                                        ifelse(jefes$pais_c=="GTM" &jefes$p02b07%in%c(2),1, #GUATE
                                                                               ifelse(jefes$pais_c=="HND" & jefes$dh205%in%c(2),1, #honduras
                                                                                      ifelse(jefes$pais_c=="JAM" & jefes$i5%in%c(2),1, #jam this is just for not linked to anything
                                                                                             ifelse(jefes$pais_c=="MEX" & jefes$drenaje%in%c(2),1, #mex
                                                                                                    ifelse(jefes$pais_c=="NIC" & jefes$s1p18%in%c(4),1, #nic
                                                                                                           ifelse(jefes$pais_c=="PAN" & jefes$v1k_servic%in%c(2),1, #not separated from septic
                                                                                                                  ifelse(jefes$pais_c=="PER" & jefes$t111a%in%c(4),1, #peru
                                                                                                                         ifelse(jefes$pais_c=="PRY" & jefes$v13%in%c(2),1, #PRY
                                                                                                                                ifelse(jefes$pais_c=="SLV" & jefes$r317%in%c(2,4),1, #svy does classify properly
                                                                                                                                       ifelse(jefes$pais_c=="URY" & jefes$d16%in%c(2),1,0))))))))))))))))))



##Improved latrine higher bound 
jefes$latrinehb <- ifelse(jefes$pais_c=="ARG" & jefes$iv11%in%c(3),1, #ARG,lb seria 0
                          ifelse(jefes$pais_c=="BOL" &jefes$s01a_15%in%c(2,4),1, #BOL lb es lo mismo
                                 ifelse(jefes$pais_c=="BRA" & jefes$s01012a%in%c(4),1, ##BRA//lb es 0
                                        ifelse(jefes$pais_c=="CHL" & jefes$v23%in%c(3,4,6,7),1, ##CHILE clas correcta lb es lo mismo
                                               ifelse(jefes$pais_c=="COL" &jefes$p5020%in%c(3,4),1,##COL lb es solo 3
                                                      ifelse(jefes$pais_c=="CRI" & jefes$v13a%in%c(4),1, ##cr lb es 0
                                                             ifelse(jefes$pais_c=="DOM" & jefes$tipo_sanitario%in%c(3,4),1, ##dOM lb es 0
                                                                    ifelse(jefes$pais_c=="ECU" & jefes$vi13%in%c(3) | jefes$vi13b%in%c(1),1, ##ECU//jmp suma cxon 3 tmb como fosa, lb es 0
                                                                           ifelse(jefes$pais_c=="GTM" & jefes$p02b07%in%c(4),1, #GUATE lb es 0
                                                                                  ifelse(jefes$pais_c=="HND" &jefes$dh205%in%c(5,6),1, #honduras bien clasificado
                                                                                         ifelse(jefes$pais_c=="JAM" & jefes$i5%in%c(3),1, #jam lb is 0
                                                                                                ifelse(jefes$pais_c=="MEX",NA, #mex lb es 0, mex doesnt ask about toilet type
                                                                                                       ifelse(jefes$pais_c=="NIC" &jefes$s1p18%in%c(2),1, #nic mismo que hb
                                                                                                              ifelse(jefes$pais_c=="PAN" & jefes$v1k_servic%in%c(1),1, #lb es 0 PAN
                                                                                                                     ifelse(jefes$pais_c=="PER" &jefes$t111a%in%c(3,5),1, #peru lb is only 3
                                                                                                                            ifelse(jefes$pais_c=="PRY" & jefes$v13%in%c(3,5,6,7),1, #PRY lb is only 5
                                                                                                                                   ifelse(jefes$pais_c=="SLV" & jefes$r317%in%c(5,6,7,8,9,10),1, #svy lb is only 7-10
                                                                                                                                          ifelse(jefes$pais_c%in%c("URY"),NA,0)))))))))))))))))) #uruguay doesnt have a question


##Improved latrine lower bound 
jefes$latrinelb <- ifelse(jefes$pais_c=="BOL" & jefes$s01a_15%in%c(2,4),1, #BOL lb es lo mismo
                          ifelse(jefes$pais_c=="CHL" & jefes$v23%in%c(3,4,6,7),1, ##CHILE clas correcta lb es lo mismo
                                 ifelse(jefes$pais_c=="HND" & jefes$dh205%in%c(5,6),1, #honduras bien clasificado
                                        ifelse(jefes$pais_c=="NIC" & jefes$s1p18%in%c(2),1, #nic mismo que hb
                                               ifelse(jefes$pais_c=="ECU" & jefes$vi13%in%c(3) | jefes$vi13b%in%c(1),1, #ECU
                                                      ifelse(jefes$pais_c=="PER" & jefes$t111a%in%c(3),1, #peru lb is only 3
                                                             ifelse(jefes$pais_c=="PRY" & jefes$v13%in%c(5),1, #PRY lb is only 5
                                                                    ifelse(jefes$pais_c=="SLV" &jefes$r317%in%c(7,8,9,10),1, #svy lb is only 7-10
                                                                           ifelse(jefes$pais_c%in%c("URY"),NA,
                                                                                  ifelse(jefes$pais_c%in%c("MEX"),NA,0))))))))))




##open defecation apro
jefes$opdef <- ifelse(jefes$pais_c=="ARG" & jefes$iv8%in%c(0,2,3,4,5,6,7,8,9),1, # ARG  
                      ifelse(jefes$pais_c=="BOL" & jefes$s01a_15%in%c(5),1, #BOL 
                             ifelse(jefes$pais_c=="BRA" &jefes$s01011c%in%c(2),1, ##BRA
                                    ifelse(jefes$pais_c=="CHL" & jefes$v23%in%c(8),1, ##CHILE class correcta 
                                           ifelse(jefes$pais_c=="COL" & jefes$p5020%in%c(6),1,##COL
                                                  ifelse(jefes$pais_c=="CRI" & jefes$v14a%in%c(0),1, ##cr 
                                                         ifelse(jefes$pais_c=="DOM" & jefes$tipo_sanitario%in%c(5),1, ##dOM
                                                                ifelse(jefes$pais_c=="ECU" & jefes$vi13d%in%c(1),1, ##ecuador
                                                                       ifelse(jefes$pais_c=="GTM" & jefes$p02b07%in%c(5),1, #GUATE
                                                                              ifelse(jefes$pais_c=="HND" & jefes$dh204%in%c(2),1, #honduras bien clasificado
                                                                                     ifelse(jefes$pais_c=="JAM" &jefes$i5%in%c(5),1, #jam
                                                                                            ifelse(jefes$pais_c=="MEX" &jefes$excusado%in%c(2),1, #mex 
                                                                                                   ifelse(jefes$pais_c=="NIC" &jefes$s1p18%in%c(6),1, #nic 
                                                                                                          ifelse(jefes$pais_c=="PAN" &jefes$v1k_servic%in%c(3),1, ##panama 
                                                                                                                 ifelse(jefes$pais_c=="PER",NA, #asks if toilet is connected to open field, but not if household does not have access to toilet
                                                                                                                        ifelse(jefes$pais_c=="PRY" & jefes$v12%in%c(6),1, #PRY
                                                                                                                               ifelse(jefes$pais_c=="SLV" &jefes$r316%in%c(2),1, #svy 
                                                                                                                                      ifelse(jefes$pais_c=="URY" & jefes$d13%in%c(2),1,0)))))))))))))))))) #URY



jefes_split <- split(jefes,jefes$pais_c)
unique(jefes_split$PRY$v12)


jefes$itpcclean <-ifelse(jefes$pais_c == "JAM", NA,jefes$itpc) ##Jamaica does not include income information
jefes$ing_hogar_clean <-ifelse(jefes$pais_c == "JAM", NA,jefes$ing_hogar) ##Jamaica does not include income information
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

################### Creation of final OLAS dataset #################################

## This section creates the final OLAS dataset according the the data 
## format specified by the developers of the site. 
## Here we create all the tables for different dimension combinations one by one and then bind them to 
## create one table
## Code was added to deal with missing data for Jamaica and Argentina

library(dplyr)
######################## Summary Data with No Extra Dimensions #################################
    ## This section creates the base table of summary indicators with no extra dimensions.

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
          
          # Argentina only surveys urban populations, so for country level data values should be NA. If there is 
          # reason to add this information again, comment out lines 316-328.
          summary <- summary[summary$pais_c != "ARG",]
          summary <-as_tibble(summary)
          summary <- add_row(summary,
                             pais_c = "ARG",access_water_piped_house = NA,
                             access_water_piped_plot = NA,access_water_other_min =NA,
                             access_water_other_max=NA,access_water_daily=NA,            
                             access_san_exclusive=NA,access_sewer=NA,
                             access_septic=NA,access_latrine_min=NA,
                             access_latrine_max=NA, access_sewer_exclusive = NA,
                             access_septic_exclusive=NA, access_latrine_exclusive_min =NA, 
                             access_latrine_exclusive_max= NA, access_water_piped_house_daily=NA,
                             access_water_piped_plot_daily=NA, hygiene_defecation=NA,.before = 1)
          
          
          
          ##Add population Data 
          pop <- as.data.frame(read.csv("poblacion2.csv"))
          pop<- rename(pop,pop_total = Total, pop_urban = Urbana, pop_rural = Rural)
          summary<- merge(summary,pop,by="pais_c")
        
        
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

        
######################## Summary Data with Scope Dimension #################################
        
    ## Table with urban/rural dimension added. Argentina does not have data on rural areas so the empty
      ## fields on rural info had to be added.    
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
        
        ## Create missing rural data for Argentina (platform needs this to adequately show values)
        
        argentina.ruralsummary <- data.frame("pais" = 17, "pais_c" = "ARG", "scope" = "rural", 
                                             "access_water_piped_house" = NA,
                                             "access_water_piped_plot" = NA,     
                                             "access_water_other_min" = NA,
                                             "access_water_other_max"= NA,
                                             "access_water_daily" = NA,
                                             "access_san_exclusive"= NA,
                                             "access_sewer"= NA,               
                                             "access_septic"= NA,
                                             "access_latrine_min"= NA,
                                             "access_latrine_max"= NA,
                                             "access_sewer_exclusive"= NA,
                                             "access_septic_exclusive"= NA,     
                                             "access_latrine_exclusive_min"= NA, 
                                             "access_latrine_exclusive_max"= NA, 
                                             "hygiene_defecation"= NA ) 
            
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

######################## Summary Data with Scope and Quintile Dimension ###############################

    ## This section creates a table with urban/rural and quintile breakdown
      ## Argentina does not have data on rural areas so the empty data is added. Jamaica does not have data 
      ## on income (quintiles) so the empty data records are added.

   ##create population percents from urban and rural dummy variables. Population percent data is required
        # by the OLAS data system for visualizations. These values are later multiplied by quintile percent
        # get the final percentages. 
        urbanquintilepercents<-jefes %>%
          group_by(pais_c,quintilitpc) %>%
          summarize(urbanpc=weighted.mean(urban,factor_ch/nmiembros_ch,na.rm=TRUE),
                    ruralpc=weighted.mean(rural,factor_ch/nmiembros_ch,na.rm=TRUE))    
        ## create the summaries indicators with the proper groupings
        summaryurbanquintiles<-jefes %>%
          group_by(pais,pais_c,scope,quintilitpc) %>%
          summarize(ave_income_pc=weighted.mean(itpcclean, factor_ch,na.rm=TRUE),
                    max_income_pc=max(itpcclean, na.rm=TRUE),
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
       
         ##drop extra fields, this will create some errors. They can be ignored. 
        summaryurbanquintiles <- subset(summaryurbanquintiles, select = -c(pais))
        
        ## Argentina is missing rural data
            argentina.summaryurbanquintiles <- data.frame("pais_c" = "ARG", "scope" = "rural", "quintilitpc" = 1:5, 
                                                 "access_water_piped_house"= NA,
                                                 "access_water_piped_plot" = NA,     
                                                 "access_water_other_min" = NA,
                                                 "access_water_other_max"= NA,
                                                 "access_water_daily" = NA,
                                                 "access_san_exclusive"= NA,
                                                 "access_sewer"= NA,               
                                                 "access_septic"= NA,
                                                 "access_latrine_min"= NA,
                                                 "access_latrine_max"= NA,
                                                 "access_sewer_exclusive"= NA,
                                                 "access_septic_exclusive"= NA,     
                                                 "access_latrine_exclusive_min"= NA, 
                                                 "access_latrine_exclusive_max"= NA, 
                                                 "hygiene_defecation"= NA )
            
            summaryurbanquintiles <- rbind(summaryurbanquintiles, argentina.summaryurbanquintiles)
          
        ##Jamaica doesnt have income data / quintiles
          
            jamtrue <- ifelse(summaryurbanquintiles$pais_c == "JAM",TRUE,FALSE)
            
            ## Jamaica does not have quintile information so we need to remove the Jamaica data from the dataframe
            ## and replace it with NAs. 
            summaryurbanquintiles <-summaryurbanquintiles[!jamtrue,]
            
            ## Jamaica does not have income data so we must create empty datasets for th
            jam.quinturbansummary <- data.frame("pais_c" = "JAM", 
                               "scope" = c("urban", "urban", "urban", "urban", "urban", "rural","rural","rural","rural","rural"), 
                               "quintilitpc" = 1:5 , 
                               "ave_income_pc" = NA,               
                               "max_income_pc" = NA,
                               "ave_hh_income"= NA,
                               "max_hh_income"= NA,
                               "access_water_piped_house"= NA,
                               "access_water_piped_plot"= NA,     
                               "access_water_other_min"= NA,
                               "access_water_other_max"= NA,
                               "access_water_daily"= NA,
                               "access_san_exclusive"= NA,
                               "access_sewer"= NA,                
                               "access_septic"= NA,
                               "access_latrine_min"= NA,
                               "access_latrine_max"= NA,
                               "access_sewer_exclusive"= NA,
                               "access_septic_exclusive"= NA,     
                               "access_latrine_exclusive_min"= NA, 
                               "access_latrine_exclusive_max"= NA, 
                               "hygiene_defecation"= NA)
            ##make data types compatible
            jam.quinturbansummary[3:22] <- sapply(jam.quinturbansummary[3:22],as.numeric)
            
            
            #Using rbind() function to insert above observations  
            summaryurbanquintiles <- rbind(summaryurbanquintiles, jam.quinturbansummary)
        
        ## join the indicator data to the population percent data as per system requirements. 
        summaryurbanquintiles <- left_join(summaryurbanquintiles, urbanquintilepercents, by = c("pais_c","quintilitpc"))
        
        
        
        ## add extra fields for the union      
        summaryurbanquintiles$sex<- "all"
        summaryurbanquintiles$pop_total <-NA
        summaryurbanquintiles$pop_rural <-NA
        summaryurbanquintiles$pop_urban <-NA
        summaryurbanquintiles$Country.Name <-NA
        
######################## Summary Data with Quintile Dimension ###################################

    ## This section creates a table with just quintile breakdowns. Empty data for Jamaica is added. 

    summaryquintiles<-jefes %>%
      group_by(pais,pais_c,quintilitpc) %>%
      summarize(urbanpc=weighted.mean(urban,factor_ch/nmiembros_ch,na.rm=TRUE),
                ruralpc=weighted.mean(rural,factor_ch/nmiembros_ch,na.rm=TRUE),
                ave_income_pc=weighted.mean(itpcclean, factor_ch,na.rm=TRUE),
                max_income_pc=max(itpcclean, na.rm=TRUE),
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
    
    
    jam.quintsummary <- data.frame("pais_c" = "JAM", 
                                        "quintilitpc" = 1:5 , 
                                        "ave_income_pc" = NA,               
                                        "max_income_pc" = NA,
                                        "ave_hh_income"= NA,
                                        "max_hh_income"= NA,
                                        "access_water_piped_house"= NA,
                                        "access_water_piped_plot"= NA,     
                                        "access_water_other_min"= NA,
                                        "access_water_other_max"= NA,
                                        "access_water_daily"= NA,
                                        "access_san_exclusive"= NA,
                                        "access_sewer"= NA,                
                                        "access_septic"= NA,
                                        "access_latrine_min"= NA,
                                        "access_latrine_max"= NA,
                                        "access_sewer_exclusive"= NA,
                                        "access_septic_exclusive"= NA,     
                                        "access_latrine_exclusive_min"= NA, 
                                        "access_latrine_exclusive_max"= NA, 
                                        "hygiene_defecation"= NA)
    
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
    
    
######################## Summary Data with Gender of Household Head Dimension #####################

  ## This section creates a table with data broken down by gender of the head of household
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

            
            
######################## Summary Data with Scope and Gender of HH Dimensions ###############################

## This section creates a summary of the data broken down by both gender of head of household and whether
## the houshold is urban or rural. Empty data for Argentina rural observations was added. 

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
      
            argentina.sexruralsummary <- data.frame("pais" = 17, "pais_c" = "ARG", "scope" = "rural",
                                                 "sex" = c("male", "female"),
                                                 "access_water_piped_house"= NA,
                                                 "access_water_piped_plot" = NA,     
                                                 "access_water_other_min" = NA,
                                                 "access_water_other_max"= NA,
                                                 "access_water_daily" = NA,
                                                 "access_san_exclusive"= NA,
                                                 "access_sewer"= NA,               
                                                 "access_septic"= NA,
                                                 "access_latrine_min"= NA,
                                                 "access_latrine_max"= NA,
                                                 "access_sewer_exclusive"= NA,
                                                 "access_septic_exclusive"= NA,     
                                                 "access_latrine_exclusive_min"= NA, 
                                                 "access_latrine_exclusive_max"= NA, 
                                                 "hygiene_defecation"= NA ) 
            
            #Using rbind() function to insert above observation  
            summarysexourbano <- rbind(summarysexourbano, argentina.sexruralsummary)
            
      
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

######################## Summary Data with Scope, Quintile, and Gender Dimensions #####################      

      ##table of data broken down by sex, urban/rural and quintiles
          summarysexourbanoquintil<-jefes %>%
            group_by(pais,pais_c,scope,sex, quintilitpc) %>%
            summarize(
              ave_income_pc=weighted.mean(itpcclean, factor_ch,na.rm=TRUE),
              max_income_pc=max(itpcclean, na.rm=TRUE),
              ave_hh_income=weighted.mean(ing_hogar_clean, factor_ch,na.rm=TRUE),
              max_hh_income=max(ing_hogar_clean,na.rm=TRUE),
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
          
          jam.qus1 <- data.frame("pais_c" = "JAM", 
                                  "scope" = c("urban","urban","urban","urban","urban", "rural","rural","rural","rural","rural"), 
                                  "sex" = "male",
                                  "quintilitpc" = 1:5 , 
                                  "ave_income_pc" = NA,               
                                  "max_income_pc" = NA,
                                  "ave_hh_income"= NA,
                                  "max_hh_income"= NA,
                                  "access_water_piped_house"= NA,
                                  "access_water_piped_plot"= NA,     
                                  "access_water_other_min"= NA,
                                  "access_water_other_max"= NA,
                                  "access_water_daily"= NA,
                                  "access_san_exclusive"= NA,
                                  "access_sewer"= NA,                
                                  "access_septic"= NA,
                                  "access_latrine_min"= NA,
                                  "access_latrine_max"= NA,
                                  "access_sewer_exclusive"= NA,
                                  "access_septic_exclusive"= NA,     
                                  "access_latrine_exclusive_min"= NA, 
                                  "access_latrine_exclusive_max"= NA, 
                                  "hygiene_defecation"= NA)
          jam.qus2 <- data.frame("pais_c" = "JAM", 
                                 "scope" = c("urban","urban","urban","urban","urban", "rural","rural","rural","rural","rural"), 
                                 "sex" = "female",
                                 "quintilitpc" = 1:5 , 
                                 "ave_income_pc" = NA,               
                                 "max_income_pc" = NA,
                                 "ave_hh_income"= NA,
                                 "max_hh_income"= NA,
                                 "access_water_piped_house"= NA,
                                 "access_water_piped_plot"= NA,     
                                 "access_water_other_min"= NA,
                                 "access_water_other_max"= NA,
                                 "access_water_daily"= NA,
                                 "access_san_exclusive"= NA,
                                 "access_sewer"= NA,                
                                 "access_septic"= NA,
                                 "access_latrine_min"= NA,
                                 "access_latrine_max"= NA,
                                 "access_sewer_exclusive"= NA,
                                 "access_septic_exclusive"= NA,     
                                 "access_latrine_exclusive_min"= NA, 
                                 "access_latrine_exclusive_max"= NA, 
                                 "hygiene_defecation"= NA)
          
          jam.quinturbsexsummary <- rbind(jam.qus2, jam.qus1)
                   ##make data types compatible
          jam.quinturbsexsummary[4:23] <- sapply(jam.quinturbsexsummary[4:23],as.numeric)
          
          
          #Using rbind() function to insert above observation  
          summarysexourbanoquintil <- rbind(summarysexourbanoquintil, jam.quinturbsexsummary)
          
          ### Adding missing rural argentina rows.
          argentina.s<- data.frame("pais_c" = "ARG", "scope" = "rural", "sex" = c("male","male","male","male","male","female","female","female","female","female"), 
                                                        "quintilitpc" = 1:5, 
                                                        "access_water_piped_house"= NA,
                                                        "access_water_piped_plot" = NA,     
                                                        "access_water_other_min" = NA,
                                                        "access_water_other_max"= NA,
                                                        "access_water_daily" = NA,
                                                        "access_san_exclusive"= NA,
                                                        "access_sewer"= NA,               
                                                        "access_septic"= NA,
                                                        "access_latrine_min"= NA,
                                                        "access_latrine_max"= NA,
                                                        "access_sewer_exclusive"= NA,
                                                        "access_septic_exclusive"= NA,     
                                                        "access_latrine_exclusive_min"= NA, 
                                                        "access_latrine_exclusive_max"= NA, 
                                                        "hygiene_defecation"= NA )
          
          summarysexourbanoquintil <- rbind(summarysexourbanoquintil, argentina.s)
          
          ##add empty fields
          
          summarysexourbanoquintil$pop_total <-NA
          summarysexourbanoquintil$pop_rural <-NA
          summarysexourbanoquintil$pop_urban <-NA
          summarysexourbanoquintil$Country.Name <-NA
          summarysexourbanoquintil$ruralpc <- NA
          summarysexourbanoquintil$urbanpc <- NA



######################## Summary Data with Gender of HH and Quintile Dimensions ###############################
  
    ##Table with data broken down by sex and quintile
        summarysexoquintil<-jefes %>%
          group_by(pais,pais_c,quintilitpc,sex) %>%
          summarize(ave_income_pc=weighted.mean(itpcclean, factor_ch,na.rm=TRUE),
                    max_income_pc=max(itpcclean, na.rm=TRUE),
                    ave_hh_income=weighted.mean(ing_hogar_clean, factor_ch,na.rm=TRUE),
                    max_hh_income=max(ing_hogar_clean,na.rm=TRUE),
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
        
        
        jam.sexquintsummary <- data.frame("pais_c" = "JAM", 
                               "sex" = c("male","male","male","male","male","female","female","female","female","female"),
                               "quintilitpc" = 1:5 , 
                               "ave_income_pc" = NA,               
                               "max_income_pc" = NA,
                               "ave_hh_income"= NA,
                               "max_hh_income"= NA,
                               "access_water_piped_house"= NA,
                               "access_water_piped_plot"= NA,     
                               "access_water_other_min"= NA,
                               "access_water_other_max"= NA,
                               "access_water_daily"= NA,
                               "access_san_exclusive"= NA,
                               "access_sewer"= NA,                
                               "access_septic"= NA,
                               "access_latrine_min"= NA,
                               "access_latrine_max"= NA,
                               "access_sewer_exclusive"= NA,
                               "access_septic_exclusive"= NA,     
                               "access_latrine_exclusive_min"= NA, 
                               "access_latrine_exclusive_max"= NA, 
                               "hygiene_defecation"= NA)
        
        
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
        
        length(names(summary))
        length(names(summaryquintiles))
        length(names(summarysexo))
        length(names(summarysexoquintil))
        length(names(summarysexourbano))
        length(names(summarysexourbanoquintil))
        length(names(summaryurban))
        length(names(summaryurbanquintiles))

        

######################## Uniting all tables into one data frame ###############################
    ##union of all tables
    
    dftotal <- union(summary, summaryurban) 
    dftotal2 <- union(summaryquintiles, summaryurbanquintiles)
    sum1 <-union(dftotal,dftotal2)
    dftotal3 <- union(summarysexo, summarysexourbano)
    dftotal4 <- union(summarysexoquintil, summarysexourbanoquintil)
    sum2 <-union(dftotal3, dftotal4)
    
    ##final draft dataset
    finaldraft <- union(sum1, sum2)


######################## Tidying up ###############################
  
    ##changing quintile to string and adding "total" category
    
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


        ## calculating population percentages for sankey diagrams
        finaldraft$population_prc <- ifelse(finaldraft$scope == "urban" & finaldraft$sex == "all" & finaldraft$quintile == "total", finaldraft$prcurban, 
                                    ifelse(finaldraft$scope == "rural" & finaldraft$sex == "all" & finaldraft$quintile == "total", finaldraft$prcrural,
                                           ifelse(finaldraft$scope == "country"  & finaldraft$sex == "all" & finaldraft$quintile == "total", 1,
                                                  ifelse(finaldraft$scope=="country"& finaldraft$sex=="all" & finaldraft$quintile != "total", .2 ,
                                                        ifelse(finaldraft$scope == "urban" & finaldraft$sex == "all" & finaldraft$quintile != "total", finaldraft$urbanpc*finaldraft$quintilepercent,
                                                                ifelse(finaldraft$scope == "rural" & finaldraft$sex == "all" & finaldraft$quintile != "total", finaldraft$ruralpc*finaldraft$quintilepercent,NA))))))



        
    ##Calculating unimproved latrine (general and exclusive) = 1 - improved latrine (max) - open def - sewer-septic
    finaldraft$access_latrines_unimproved_min <- 1- (finaldraft$access_latrine_max + finaldraft$hygiene_defecation + finaldraft$access_sewer + finaldraft$access_septic)
    finaldraft$access_latrines_unimproved_exclusive_min <- finaldraft$access_san_exclusive - (finaldraft$access_sewer_exclusive+finaldraft$access_septic_exclusive+finaldraft$access_latrine_exclusive_max)


    ## caculating access to "improved" water source 
    finaldraft$access_water_ub <- ifelse(is.na(finaldraft$access_water_piped_house),0, finaldraft$access_water_piped_house) + finaldraft$access_water_other_max + finaldraft$access_water_piped_plot
    finaldraft$access_water_lb <- ifelse(is.na(finaldraft$access_water_piped_house),0, finaldraft$access_water_piped_house) + finaldraft$access_water_other_min + finaldraft$access_water_piped_plot
    
    
    ## Arranging dataset to make it easier to check
    finaldraft <- relocate(finaldraft, pais_c, scope, quintile, sex,population, population_prc,ave_income_pc,	max_income_pc,	ave_hh_income,	max_hh_income)


    
    
    ##final (renaming columns)
    finaldraft <- rename(finaldraft, iso = pais_c, gender = sex, population_perc = population_prc)
    final <- subset(finaldraft, select = -c(pop_total.x,	pop_urban.x,	
                                            pop_rural.x,ruralpc,	urbanpc,	Country.Name,	pop_total.y,	
                                            pop_urban.y,	pop_rural.y,	prcurban,	prcrural,quintilepercent))
    
    
    
    
    #creating metadata table
      final_class <- sapply(final, class)
      col <- 1:ncol(final)
      metadata <- data.frame(col, final_class)
    
      
      
    ## create function to eliminate NaN
    is.nan.data.frame <- function(x)
      do.call(cbind, lapply(x, is.nan))
    
    ## eliminate NaN
    final[is.nan.data.frame(final)] <- NA


    ### add year of survey
    final$year <- ifelse(final$iso == "ARG",2018,
                          ifelse(final$iso == "BRA",2019,
                            ifelse(final$iso == "BOL",2018,
                                   ifelse(final$iso == "CHL",2017,
                                          ifelse(final$iso == "COL",2018,
                                                 ifelse(final$iso == "CRI",2018,
                                                        ifelse(final$iso == "DOM",2018,
                                                               ifelse(final$iso =="SLV",2018,
                                                                      ifelse(final$iso == "ECU",2017,
                                                                             ifelse(final$iso == "GTM",2018,
                                                                                    ifelse(final$iso == "HND",2018, 
                                                                                           ifelse(final$iso == "JAM",2015, 
                                                                                                  ifelse(final$iso == "MEX", 2018,
                                                                                                         ifelse(final$iso ==  "NIC",2014,
                                                                                                                ifelse(final$iso == "PAN",2018,
                                                                                                                       ifelse(final$iso =="PRY",2017,
                                                                                                                              ifelse(final$iso == "PER",2018,
                                                                                                                                     ifelse(final$iso == "URY",2018,NA))))))))))))))))))
                                                                                                                                                                                                           

  
    


######################## Data Availability Validation ###############################

#Calculate data availability

    
dadummy <- final
dadummy$access_water<- ifelse(is.na(final$access_water_lb), 0, 1)
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


## improved latrine data availability metric
##finaldraft$improved_latrine_da<-ifelse(finaldraft$access_latrine_max-finaldraft$access_latrine_min == 0,1,NA)

## improved non-piped water access
##finaldraft$improved_nonpiped_da<-ifelse(finaldraft$access_water_other_max-finaldraft$access_water_other_min == 0,1,NA)



## create validation dataset
v <- ifelse(final$scope == "country"&final$gender =="all" &final$quintile == "total", TRUE, FALSE)
validationdata <-final[v,]
write.csv(validationdata, file ="validation/data_validation.csv", row.names = FALSE)



######################## Save data set for each country and overall dataset ###########################


write.csv(final, file ="countrydata/OLAS_HH_Survey_Dataset.csv", row.names = FALSE)
write.csv(dataavailability, file ="validation/data_availability.csv", row.names = FALSE)


z<-split(final,final$iso)
m <- length(z)
for(i in 1:m){
  write.csv(z[[i]], file = 
              paste0("countrydata/",names(z[i]),".csv"), row.names = FALSE)
}

##f <- subset(z$CRI, select = c(pais_c,idh_ch,v14a,p5020,v23,s01011c,s01a_15,iv8,opdef))
