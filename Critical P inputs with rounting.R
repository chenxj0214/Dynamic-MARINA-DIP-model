rm(list=ls())
#regional boundaries for agricultural nitrogen pollution
#TP 0.2mg/L


#####critical DIP from agricultural sources by tributaty, TP*actual water discharge
inputs<-read.csv("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data/MARINAModel_inputs_2012.csv")
outputs<-read.csv("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data/MARINAModel_outputs_2012.csv")
#####critical agricultural DIP inputs to rivers for past year (2000-2019)
Results_past<-data.frame(inputs[,"code"],inputs[,"name"])
names(Results_past)<-c("code","name")
Results_past[,"Qac"]<-inputs[,"Rnat"]/1000*inputs[,"areacell"]*(1-inputs[,"FQrem"])#km3
Results_past[,"year"]<-rep(2000,11)

##critical Main channel
Results_past[,"Qac_acc"]<-Results_past[,"Qac"]
aggregate_subbasins_Qact <- function(df) {
  year<-unique(df$year)
  for (i in 1:length(year)) {
    # Step 1: Sum values of sub-basins 265, 263, 267, 270, and 271, update 265
    df[df$code == 5&df$year == year[i], "Qac_acc"] <- sum(df[df$code %in% c(1, 2, 3, 4, 5)&df$year == year[i], "Qac_acc"], na.rm = TRUE)
    
    # Step 2: Sum values of sub-basins 269, 268, 264, 266, and 265, update 269
    df[df$code == 9&df$year == year[i], "Qac_acc"] <- sum(df[df$code %in% c(5, 6, 7, 8, 9)&df$year == year[i], "Qac_acc"], na.rm = TRUE)
    
    # Step 3: Sum values of sub-basins 272, 273, and 269, update 272
    df[df$code == 11&df$year == year[i], "Qac_acc"] <- sum(df[df$code %in% c(9, 10, 11)&df$year == year[i], "Qac_acc"], na.rm = TRUE)
    
  }
  
  return(df)
}

Results_past<-aggregate_subbasins_Qact(Results_past)

Results_past[,"C_TP_load_acc"]<-Results_past[,"Qac_acc"]*1000*1000*1000*1000*0.2/1000/1000/1000/1000#critical TP export at outlets of sub-basins,kton

inputs[,"FErivDIPo"]<-(1-inputs[,"Ddip"])*(1-inputs[,"Ldip"])*(1-inputs[,"FQrem"])
inputs[,"FErivDOPo"]<-(1-inputs[,"FQrem"])

outputs[,"OLdifDIP"]<-(outputs[,"RSdifDIP.fe.j"]+outputs[,"RSdifDIP.ma.j"]+outputs[,"RSdifDIP.hum.j"]+outputs[,"RSdifDIP.wth.ant.j"])*inputs[,"FErivDIPo"]
outputs[,"OLpntDIP"]<-(outputs[,"RSdifDIP.wth.nat.j"]+outputs[,"RSpntDIP.ma.j"]+outputs[,"RSpntDIP.hum.uncon"]+outputs[,"RSpntDIP.hum.con.j"]+outputs[,"RSpntDIP.det.con.j"])*inputs[,"FErivDIPo"]
outputs[,"OLdifDOP"]<-(outputs[,"RSdifDOP.fe.j"]+outputs[,"RSdifDOP.ma.j"]+outputs[,"RSdifDOP.hum.j"]+outputs[,"RSdifDOP.lech.ant.j"]+outputs[,"RSdifDOP.lech.nat.j"])*inputs[,"FErivDOPo"]
outputs[,"OLpntDOP"]<-(outputs[,"RSpntDOP.ma.j"]+outputs[,"RSpntDOP.hum.uncon"]+outputs[,"RSpntDOP.hum.con.j"]+outputs[,"RSpntDOP.det.con.j"])*inputs[,"FErivDOPo"]
outputs[,"OLDIP"]<-outputs[,"OLdifDIP"]+outputs[,"OLpntDIP"]
outputs[,"OLDOP"]<-outputs[,"OLdifDOP"]+outputs[,"OLpntDOP"]

##
aggregate_subbasins_up <- function(df,varible) {
  # Step 1: Sum values of sub-basins 1, 2, 3, 4, and 5, update 5
  data_acc <- sum(df[df$code %in% c(1, 2, 3, 4, 5), varible], na.rm = TRUE)
  
  return(data_acc)
}

aggregate_subbasins_middle <- function(df,varible) {
  
  # Step 2: Sum values of sub-basins 1,2,3,4,5,6,7,8 and 9, update 9
  data_acc <- sum(df[df$code %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9),varible], na.rm = TRUE)
  
  return(data_acc)
}


aggregate_subbasins_down <- function(df,varible) {
  
  # Step 3: Sum values of sub-basins 272, 273, and 269, update 10
  data_acc <- sum(df[df$code %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),varible], na.rm = TRUE)
  
  return(data_acc)
}


##
#DIP and DOP export to outlets of upper stem
outputs[,"OLDIP_up"]<-outputs[,"OLDIP"]*(1-inputs$Ddip.juC1*inputs$AjuC1)*(1-inputs$Ldip.juC1*inputs$AjuC1)*(1-inputs$FQrem.juC1*inputs$AjuC1)
outputs[,"OLdifDIP_up"]<-outputs[,"OLdifDIP"]*(1-inputs$Ddip.juC1*inputs$AjuC1)*(1-inputs$Ldip.juC1*inputs$AjuC1)*(1-inputs$FQrem.juC1*inputs$AjuC1)
outputs[,"OLDOP_up"]<-outputs[,"OLDOP"]*(1-inputs$FQrem.juC1*inputs$AjuC1)

T_DIP_up_acc<-aggregate_subbasins_up(outputs,"OLDIP_up")
T_difDIP_up_acc<-aggregate_subbasins_up(outputs,"OLdifDIP_up")
T_DOP_up_acc<-aggregate_subbasins_up(outputs,"OLDOP_up")

#DIP and DOP export to outlets of middle stem
outputs[,"OLDIP_mid"]<-outputs[,"OLDIP_up"]*(1-inputs$Ddip.jmC1*inputs$AjmC1)*(1-inputs$Ldip.jmC1*inputs$AjmC1)*(1-inputs$FQrem.jmC1*inputs$AjmC1)
outputs[,"OLdifDIP_mid"]<-outputs[,"OLdifDIP_up"]*(1-inputs$Ddip.jmC1*inputs$AjmC1)*(1-inputs$Ldip.jmC1*inputs$AjmC1)*(1-inputs$FQrem.jmC1*inputs$AjmC1)
outputs[,"OLDOP_mid"]<-outputs[,"OLDOP_up"]*(1-inputs$FQrem.jmC1*inputs$AjmC1)

T_DIP_mid_acc<-aggregate_subbasins_middle(outputs,"OLDIP_mid")
T_difDIP_mid_acc<-aggregate_subbasins_middle(outputs,"OLdifDIP_mid")
T_DOP_mid_acc<-aggregate_subbasins_middle(outputs,"OLDOP_mid")

#DIP and DOP export to outlets of delta
outputs[,"OLDIP_down"]<-outputs[,"OLDIP_mid"]*(1-inputs$Ddip.jdC1*inputs$AjdC1)*(1-inputs$Ldip.jdC1*inputs$AjdC1)*(1-inputs$FQrem.jdC1*inputs$AjdC1)
outputs[,"OLdifDIP_down"]<-outputs[,"OLdifDIP_mid"]*(1-inputs$Ddip.jdC1*inputs$AjdC1)*(1-inputs$Ldip.jdC1*inputs$AjdC1)*(1-inputs$FQrem.jdC1*inputs$AjdC1)
outputs[,"OLDOP_down"]<-outputs[,"OLDOP_mid"]*(1-inputs$FQrem.jdC1*inputs$AjdC1)

T_DIP_down_acc<-aggregate_subbasins_down(outputs,"OLDIP_down")
T_difDIP_down_acc<-aggregate_subbasins_down(outputs,"OLdifDIP_down")
T_DOP_down_acc<-aggregate_subbasins_down(outputs,"OLDOP_down")

######
outputs$OLDIP_acc<-outputs$OLDIP
outputs$OLdifDIP_acc<-outputs$OLdifDIP
outputs$OLDOP_acc<-outputs$OLDOP

outputs[outputs$code == 5, "OLDIP_acc"] <-T_DIP_up_acc
outputs[outputs$code == 9, "OLDIP_acc"] <-T_DIP_mid_acc
outputs[outputs$code == 11, "OLDIP_acc"] <-T_DIP_down_acc

outputs[outputs$code == 5, "OLdifDIP_acc"] <-T_difDIP_up_acc
outputs[outputs$code == 9, "OLdifDIP_acc"] <-T_difDIP_mid_acc
outputs[outputs$code == 11, "OLdifDIP_acc"] <-T_difDIP_down_acc

outputs[outputs$code == 5, "OLDOP_acc"] <-T_DOP_up_acc
outputs[outputs$code == 9, "OLDOP_acc"] <-T_DOP_mid_acc
outputs[outputs$code == 11, "OLDOP_acc"] <-T_DOP_down_acc


outputs[,"OLTP_acc"]<-outputs[,"OLDIP_acc"]+outputs[,"OLDOP_acc"]

Results_past[,"C_difDIP_load_acc"]<-Results_past[,"C_TP_load_acc"]*(outputs[,"OLdifDIP_acc"]/outputs[,"OLTP_acc"])#kton

####calculate critical agricultural DIP inputs by sub-basin
  Results_past$C_difDIP_load_cor<-Results_past$C_difDIP_load_acc

routing_DIP<-function(df,tf){
  year<-unique(tf$year)
  for (i in 1:length(year)) {
    up_f<-(1-df$Ddip.juC1*df$AjuC1)*(1-df$Ldip.juC1*df$AjuC1)*(1-df$FQrem.juC1*df$AjuC1)
    up_f[which(df$code %in% c(5, 6, 7, 8, 9, 10, 11))]<-0
    critical_juc<-tf[tf$code == 5&tf$year == year[i],"C_difDIP_load_acc"]-sum(tf[tf$year == year[i],"C_difDIP_load_acc"]*up_f)
    
    mid_f<-(1-df$Ddip.jmC1*df$AjmC1)*(1-df$Ldip.jmC1*df$AjmC1)*(1-df$FQrem.jmC1*df$AjmC1)
    mid_f[which(df$code %in% c(1,2,3,4,9,10,11))]=0
    critical_jmc<-tf[tf$code == 9&tf$year == year[i],"C_difDIP_load_acc"]-sum(tf[tf$year == year[i],"C_difDIP_load_acc"]*mid_f)
    
    dow_f<-(1-df$Ddip.jdC1*df$AjdC1)*(1-df$Ldip.jdC1*df$AjdC1)*(1-df$FQrem.jdC1*df$AjdC1)
    dow_f[which(df$code %in% c(1:8,11))]=0
    critical_jdc<-tf[tf$code == 11&tf$year == year[i],"C_difDIP_load_acc"]-sum(tf[tf$year == year[i],"C_difDIP_load_acc"]*dow_f)
    
    tf[tf$code == 5&tf$year == year[i], "C_difDIP_load_cor"]<-critical_juc
    tf[tf$code == 9&tf$year == year[i], "C_difDIP_load_cor"]<-critical_jmc
    tf[tf$code == 11&tf$year == year[i], "C_difDIP_load_cor"]<-critical_jdc
  }
  
  return(tf)
  
}
Results_past<-routing_DIP(inputs,Results_past)



Results_past[,"C_difDIP_inputs_cor"]<-Results_past[,"C_difDIP_load_cor"]/inputs[,"FErivDIPo"]##kton
Results_past<-Results_past[rep(1:11,each=20),]
Results_past[,"year"]<-rep(2000:2019,)



####future critical DIP from agricultural sources by sub-basins
RCP_2.6<- read.csv("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data/RCP_2.6_future_runoff_2020-2099.csv")
RCP_8.5<- read.csv("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data/RCP_8.5_future_runoff_2020-2099.csv")

Results_BAU<-data.frame(rep(inputs[,"code"],each=80),rep(inputs[,"name"],each=80))
names(Results_BAU)<-c("code","name")
Results_BAU[,"year"]<-rep(c(2020:2099),11)

###safe level of agricultural DIP under RCP 2.6 with past DIP loss fraction from 2020 to 2099
Results_BAU_2.6<-Results_BAU
Results_BAU_2.6[,"Qac"]<-RCP_2.6[,"runoff"]/1000*rep(inputs[,"areacell"],each=80)*rep((1-inputs[,"FQrem"]),each=80)#km3
Results_BAU_2.6[,"Qac_acc"]<-Results_BAU_2.6[,"Qac"]
Results_BAU_2.6<-aggregate_subbasins_Qact(Results_BAU_2.6)
Results_BAU_2.6[,"C_TP_load_acc"]<-Results_BAU_2.6[,"Qac_acc"]*1000*1000*1000*1000*0.2/1000/1000/1000/1000#kton
Results_BAU_2.6[,"C_difDIP_load_acc"]<- Results_BAU_2.6[,"C_TP_load_acc"]*rep(outputs[,"OLdifDIP_acc"]/outputs[,"OLTP_acc"],each=80)

Results_BAU_2.6$C_difDIP_load_cor<-Results_BAU_2.6$C_difDIP_load_acc
Results_BAU_2.6<-routing_DIP(inputs,Results_BAU_2.6)
Results_BAU_2.6[,"C_difDIP_inputs_cor"]<-Results_BAU_2.6[,"C_difDIP_load_cor"]/rep(inputs[,"FErivDIPo"],each=80)
Results_2.6<-rbind(Results_past,Results_BAU_2.6)

  
###safe level of agricultural DIP under RCP 8.5 with past DIP loss fraction
Results_BAU_8.5<-Results_BAU

Results_BAU_8.5[,"Qac"]<-RCP_8.5[,"runoff"]/1000*rep(inputs[,"areacell"],each=80)*rep((1-inputs[,"FQrem"]),each=80)#km3
Results_BAU_8.5[,"Qac_acc"]<-Results_BAU_8.5[,"Qac"]
Results_BAU_8.5<-aggregate_subbasins_Qact(Results_BAU_8.5)
Results_BAU_8.5[,"C_TP_load_acc"]<-Results_BAU_8.5[,"Qac_acc"]*1000*1000*1000*1000*0.2/1000/1000/1000/1000#kton
Results_BAU_8.5[,"C_difDIP_load_acc"]<- Results_BAU_8.5[,"C_TP_load_acc"]*rep(outputs[,"OLdifDIP_acc"]/outputs[,"OLTP_acc"],each=80)

Results_BAU_8.5$C_difDIP_load_cor<-Results_BAU_8.5$C_difDIP_load_acc
Results_BAU_8.5<-routing_DIP(inputs,Results_BAU_8.5)
Results_BAU_8.5[,"C_difDIP_inputs_cor"]<-Results_BAU_8.5[,"C_difDIP_load_cor"]/rep(inputs[,"FErivDIPo"],each=80)

Results_8.5<-rbind(Results_past,Results_BAU_8.5)


######critical TP export at outlets of sub-basins at SSP5 scenario
#outputs_SSP5<-read.csv("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data/MARINAModel_outputs_SSP5RCP8p5.csv")
#inputs_SSP5<-read.csv("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data/MARINAModel_inputs_SSP5RCP8p5.csv")
outputs_SSP5<-read.csv("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data/MARINAModel_outputs_SSP5RCP8p5SE.csv")
inputs_SSP5<-read.csv("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data/MARINAModel_inputs_SSP5RCP8p5SE.csv")


Results_future<-data.frame(inputs_SSP5[,"code"],inputs_SSP5[,"name"])
names(Results_future)<-c("code","name")
Results_future[,"Qac"]<-inputs[,"Rnat"]/1000*inputs_SSP5[,"areacell"]*(1-inputs_SSP5[,"FQrem"])#km3
Results_future[,"year"]<-rep(2000,11)

Results_future[,"Qac_acc"]<-Results_future[,"Qac"]


Results_future<-aggregate_subbasins_Qact(Results_future)

Results_future[,"C_TP_load_acc"]<-Results_future[,"Qac_acc"]*1000*1000*1000*1000*0.2/1000/1000/1000/1000#critical TP export at outlets of sub-basins,kton


inputs_SSP5[,"FErivDIPo"]<-(1-inputs_SSP5[,"Ddip"])*(1-inputs_SSP5[,"Ldip"])*(1-inputs_SSP5[,"FQrem"])
inputs_SSP5[,"FErivDOPo"]<-(1-inputs_SSP5[,"FQrem"])
###DIP export to outlets
outputs_SSP5[,"OLdifDIP"]<-(outputs_SSP5[,"RSdifDIP.fe.j"]+outputs_SSP5[,"RSdifDIP.ma.j"]+outputs_SSP5[,"RSdifDIP.hum.j"]+outputs_SSP5[,"RSdifDIP.wth.ant.j"])*inputs_SSP5[,"FErivDIPo"]
outputs_SSP5[,"OLpntDIP"]<-(outputs_SSP5[,"RSdifDIP.wth.nat.j"]+outputs_SSP5[,"RSpntDIP.ma.j"]+outputs_SSP5[,"RSpntDIP.hum.uncon"]+outputs_SSP5[,"RSpntDIP.hum.con.j"]+outputs_SSP5[,"RSpntDIP.det.con.j"])*inputs_SSP5[,"FErivDIPo"]
outputs_SSP5[,"OLdifDOP"]<-(outputs_SSP5[,"RSdifDOP.fe.j"]+outputs_SSP5[,"RSdifDOP.ma.j"]+outputs_SSP5[,"RSdifDOP.hum.j"]+outputs_SSP5[,"RSdifDOP.lech.ant.j"]+outputs_SSP5[,"RSdifDOP.lech.nat.j"])*inputs_SSP5[,"FErivDOPo"]
outputs_SSP5[,"OLpntDOP"]<-(outputs_SSP5[,"RSpntDOP.ma.j"]+outputs_SSP5[,"RSpntDOP.hum.uncon"]+outputs_SSP5[,"RSpntDOP.hum.con.j"]+outputs_SSP5[,"RSpntDOP.det.con.j"])*inputs_SSP5[,"FErivDOPo"]
outputs_SSP5[,"OLDIP"]<-outputs_SSP5[,"OLdifDIP"]+outputs_SSP5[,"OLpntDIP"]
outputs_SSP5[,"OLDOP"]<-outputs_SSP5[,"OLdifDOP"]+outputs_SSP5[,"OLpntDOP"]

####
#DIP and DOP export to outlets of upper stem
outputs_SSP5[,"OLDIP_up"]<-outputs_SSP5[,"OLDIP"]*(1-inputs_SSP5$Ddip.juC1*inputs_SSP5$AjuC1)*(1-inputs_SSP5$Ldip.juC1*inputs_SSP5$AjuC1)*(1-inputs_SSP5$FQrem.juC1*inputs_SSP5$AjuC1)
outputs_SSP5[,"OLdifDIP_up"]<-outputs_SSP5[,"OLdifDIP"]*(1-inputs_SSP5$Ddip.juC1*inputs_SSP5$AjuC1)*(1-inputs_SSP5$Ldip.juC1*inputs_SSP5$AjuC1)*(1-inputs_SSP5$FQrem.juC1*inputs_SSP5$AjuC1)
outputs_SSP5[,"OLDOP_up"]<-outputs_SSP5[,"OLDOP"]*(1-inputs_SSP5$FQrem.juC1*inputs_SSP5$AjuC1)

T_DIP_up_acc_SSP5<-aggregate_subbasins_up(outputs_SSP5,"OLDIP_up")
T_difDIP_up_acc_SSP5<-aggregate_subbasins_up(outputs_SSP5,"OLdifDIP_up")
T_DOP_up_acc_SSP5<-aggregate_subbasins_up(outputs_SSP5,"OLDOP_up")

#DIP and DOP export to outlets of middle stem
outputs_SSP5[,"OLDIP_mid"]<-outputs_SSP5[,"OLDIP_up"]*(1-inputs_SSP5$Ddip.jmC1*inputs_SSP5$AjmC1)*(1-inputs_SSP5$Ldip.jmC1*inputs_SSP5$AjmC1)*(1-inputs_SSP5$FQrem.jmC1*inputs_SSP5$AjmC1)
outputs_SSP5[,"OLdifDIP_mid"]<-outputs_SSP5[,"OLdifDIP_up"]*(1-inputs_SSP5$Ddip.jmC1*inputs_SSP5$AjmC1)*(1-inputs_SSP5$Ldip.jmC1*inputs_SSP5$AjmC1)*(1-inputs_SSP5$FQrem.jmC1*inputs_SSP5$AjmC1)
outputs_SSP5[,"OLDOP_mid"]<-outputs_SSP5[,"OLDOP_up"]*(1-inputs_SSP5$FQrem.jmC1*inputs_SSP5$AjmC1)

T_DIP_mid_acc_SSP5<-aggregate_subbasins_middle(outputs_SSP5,"OLDIP_mid")
T_difDIP_mid_acc_SSP5<-aggregate_subbasins_middle(outputs_SSP5,"OLdifDIP_mid")
T_DOP_mid_acc_SSP5<-aggregate_subbasins_middle(outputs_SSP5,"OLDOP_mid")

#DIP and DOP export to outlets of delta
outputs_SSP5[,"OLDIP_down"]<-outputs_SSP5[,"OLDIP_mid"]*(1-inputs_SSP5$Ddip.jdC1*inputs$AjdC1)*(1-inputs_SSP5$Ldip.jdC1*inputs_SSP5$AjdC1)*(1-inputs_SSP5$FQrem.jdC1*inputs_SSP5$AjdC1)
outputs_SSP5[,"OLdifDIP_down"]<-outputs_SSP5[,"OLdifDIP_mid"]*(1-inputs_SSP5$Ddip.jdC1*inputs$AjdC1)*(1-inputs_SSP5$Ldip.jdC1*inputs_SSP5$AjdC1)*(1-inputs_SSP5$FQrem.jdC1*inputs_SSP5$AjdC1)
outputs_SSP5[,"OLDOP_down"]<-outputs_SSP5[,"OLDOP_mid"]*(1-inputs_SSP5$FQrem.jdC1*inputs_SSP5$AjdC1)

T_DIP_down_acc_SSP5<-aggregate_subbasins_down(outputs_SSP5,"OLDIP_down")
T_difDIP_down_acc_SSP5<-aggregate_subbasins_down(outputs_SSP5,"OLdifDIP_down")
T_DOP_down_acc_SSP5<-aggregate_subbasins_down(outputs_SSP5,"OLDOP_down")

######
outputs_SSP5$OLDIP_acc<-outputs_SSP5$OLDIP
outputs_SSP5$OLdifDIP_acc<-outputs_SSP5$OLdifDIP
outputs_SSP5$OLDOP_acc<-outputs_SSP5$OLDOP

outputs_SSP5[outputs_SSP5$code == 5, "OLDIP_acc"] <-T_DIP_up_acc_SSP5
outputs_SSP5[outputs_SSP5$code == 9, "OLDIP_acc"] <-T_DIP_mid_acc_SSP5
outputs_SSP5[outputs_SSP5$code == 11, "OLDIP_acc"] <-T_DIP_down_acc_SSP5

outputs_SSP5[outputs_SSP5$code == 5, "OLdifDIP_acc"] <-T_difDIP_up_acc_SSP5
outputs_SSP5[outputs_SSP5$code == 9, "OLdifDIP_acc"] <-T_difDIP_mid_acc_SSP5
outputs_SSP5[outputs_SSP5$code == 11, "OLdifDIP_acc"] <-T_difDIP_down_acc_SSP5

outputs_SSP5[outputs_SSP5$code == 5, "OLDOP_acc"] <-T_DOP_up_acc_SSP5
outputs_SSP5[outputs_SSP5$code == 9, "OLDOP_acc"] <-T_DOP_mid_acc_SSP5
outputs_SSP5[outputs_SSP5$code == 11, "OLDOP_acc"] <-T_DOP_down_acc_SSP5


outputs_SSP5[,"OLTP_acc"]<-outputs_SSP5[,"OLDIP_acc"]+outputs_SSP5[,"OLDOP_acc"]

####

Results_future[,"C_difDIP_load_acc"]<-Results_future[,"C_TP_load_acc"]*(outputs_SSP5[,"OLdifDIP_acc"]/outputs_SSP5[,"OLTP_acc"])#kton
Results_future$C_difDIP_load_cor<-Results_future$C_difDIP_load_acc
Results_future<-routing_DIP(inputs_SSP5,Results_future)
Results_future[,"C_difDIP_inputs_cor"]<-Results_future[,"C_difDIP_load_cor"]/inputs_SSP5[,"FErivDIPo"]##kton
Results_future<-Results_future[rep(1:11,each=20),]
Results_future[,"year"]<-rep(2000:2019,)



####future critical DIP from agricultural sources by sub-basins

Results_SSP5<-data.frame(rep(inputs_SSP5[,"code"],each=80),rep(inputs_SSP5[,"name"],each=80))
names(Results_SSP5)<-c("code","name")
Results_SSP5[,"year"]<-rep(c(2020:2099),11)

###safe level of agricultural DIP under RCP 2.6 with past DIP loss fraction from 2020 to 2099
Results_SSP5_2.6<-Results_SSP5
Results_SSP5_2.6[,"Qac"]<-RCP_2.6[,"runoff"]/1000*rep(inputs_SSP5[,"areacell"],each=80)*rep((1-inputs_SSP5[,"FQrem"]),each=80)#km3
Results_SSP5_2.6[,"Qac_acc"]<-Results_SSP5_2.6[,"Qac"]
Results_SSP5_2.6<-aggregate_subbasins_Qact(Results_SSP5_2.6)
Results_SSP5_2.6[,"C_TP_load_acc"]<-Results_SSP5_2.6[,"Qac_acc"]*1000*1000*1000*1000*0.2/1000/1000/1000/1000#kton
Results_SSP5_2.6[,"C_difDIP_load_acc"]<- Results_SSP5_2.6[,"C_TP_load_acc"]*rep(outputs_SSP5[,"OLdifDIP_acc"]/outputs_SSP5[,"OLTP_acc"],each=80)

Results_SSP5_2.6$C_difDIP_load_cor<-Results_SSP5_2.6$C_difDIP_load_acc
Results_SSP5_2.6<-routing_DIP(inputs_SSP5,Results_SSP5_2.6)
Results_SSP5_2.6[,"C_difDIP_inputs_cor"]<-Results_SSP5_2.6[,"C_difDIP_load_cor"]/rep(inputs_SSP5[,"FErivDIPo"],each=80)
Results_SSP5_2.6<-rbind(Results_future,Results_SSP5_2.6)


###safe level of agricultural DIP under RCP 8.5 with past DIP loss fraction
Results_SSP5_8.5<-Results_SSP5

Results_SSP5_8.5[,"Qac"]<-RCP_8.5[,"runoff"]/1000*rep(inputs_SSP5[,"areacell"],each=80)*rep((1-inputs_SSP5[,"FQrem"]),each=80)#km3
Results_SSP5_8.5[,"Qac_acc"]<-Results_SSP5_8.5[,"Qac"]
Results_SSP5_8.5<-aggregate_subbasins_Qact(Results_SSP5_8.5)
Results_SSP5_8.5[,"C_TP_load_acc"]<-Results_SSP5_8.5[,"Qac_acc"]*1000*1000*1000*1000*0.2/1000/1000/1000/1000#kton
Results_SSP5_8.5[,"C_difDIP_load_acc"]<- Results_SSP5_8.5[,"C_TP_load_acc"]*rep(outputs_SSP5[,"OLdifDIP_acc"]/outputs_SSP5[,"OLTP_acc"],each=80)

Results_SSP5_8.5$C_difDIP_load_cor<-Results_SSP5_8.5$C_difDIP_load_acc
Results_SSP5_8.5<-routing_DIP(inputs_SSP5,Results_SSP5_8.5)
Results_SSP5_8.5[,"C_difDIP_inputs_cor"]<-Results_SSP5_8.5[,"C_difDIP_load_cor"]/rep(inputs_SSP5[,"FErivDIPo"],each=80)

Results_SSP5_8.5<-rbind(Results_future,Results_SSP5_8.5)




###export resutls
Results_2.6_Yangtze<-aggregate(Results_2.6[,c("Qac","C_difDIP_load_cor","C_difDIP_inputs_cor")],by=list(Results_2.6$year),FUN=sum)
Results_2.6_Yangtze[,"code"]=rep(0,100)
Results_2.6_Yangtze[,"name"]=rep("Yangtze",100)
colnames(Results_2.6_Yangtze)[1]= "year"
Results_2.6_Yangtze[,"Qac_acc"]=NA
Results_2.6_Yangtze[,"C_TP_load_acc"]=NA
Results_2.6_Yangtze[,"C_difDIP_load_acc"]=NA

Results_2.6<-rbind(Results_2.6,Results_2.6_Yangtze)
Results_2.6<-Results_2.6[order(Results_2.6$code,Results_2.6$year,decreasing = F),]


Results_8.5_Yangtze<-aggregate(Results_8.5[,c("Qac","C_difDIP_load_cor","C_difDIP_inputs_cor")],by=list(Results_8.5$year),FUN=sum)
Results_8.5_Yangtze[,"code"]=rep(0,100)
Results_8.5_Yangtze[,"name"]=rep("Yangtze",100)
colnames(Results_8.5_Yangtze)[1]= "year"
Results_8.5_Yangtze[,"Qac_acc"]=NA
Results_8.5_Yangtze[,"C_TP_load_acc"]=NA
Results_8.5_Yangtze[,"C_difDIP_load_acc"]=NA
Results_8.5<-rbind(Results_8.5,Results_8.5_Yangtze)
Results_8.5<-Results_8.5[order(Results_8.5$code,Results_8.5$year,decreasing = F),]


Results_2.6_SSP5_Yangtze<-aggregate(Results_SSP5_2.6[,c("Qac","C_difDIP_load_cor","C_difDIP_inputs_cor")],by=list(Results_SSP5_2.6$year),FUN=sum)
Results_2.6_SSP5_Yangtze[,"code"]=rep(0,100)
Results_2.6_SSP5_Yangtze[,"name"]=rep("Yangtze",100)
colnames(Results_2.6_SSP5_Yangtze)[1]= "year"
Results_2.6_SSP5_Yangtze[,"Qac_acc"]=NA
Results_2.6_SSP5_Yangtze[,"C_TP_load_acc"]=NA
Results_2.6_SSP5_Yangtze[,"C_difDIP_load_acc"]=NA
Results_2.6_SSP5<-rbind(Results_SSP5_2.6,Results_2.6_SSP5_Yangtze)
Results_2.6_SSP5<-Results_2.6_SSP5[order(Results_2.6_SSP5$code,Results_2.6_SSP5$year,decreasing = F),]


Results_8.5_SSP5_Yangtze<-aggregate(Results_SSP5_8.5[,c("Qac","C_difDIP_load_cor","C_difDIP_inputs_cor")],by=list(Results_SSP5_8.5$year),FUN=sum)
Results_8.5_SSP5_Yangtze[,"code"]=rep(0,100)
Results_8.5_SSP5_Yangtze[,"name"]=rep("Yangtze",100)
colnames(Results_8.5_SSP5_Yangtze)[1]= "year"
Results_8.5_SSP5_Yangtze[,"Qac_acc"]=NA
Results_8.5_SSP5_Yangtze[,"C_TP_load_acc"]=NA
Results_8.5_SSP5_Yangtze[,"C_difDIP_load_acc"]=NA
Results_8.5_SSP5<-rbind(Results_SSP5_8.5,Results_8.5_SSP5_Yangtze)
Results_8.5_SSP5<-Results_8.5_SSP5[order(Results_8.5_SSP5$code,Results_8.5_SSP5$year,decreasing = F),]


setwd("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data/Outputs")
write.csv(Results_2.6,"Critical_DIP_RCP2.6.csv",row.names = F)
write.csv(Results_8.5,"Critical_DIP_RCP8.5.csv",row.names = F)
write.csv(Results_2.6_SSP5,"Critical_DIP_RCP2.6_SSP5.csv",row.names = F)
write.csv(Results_8.5_SSP5,"Critical_DIP_RCP8.5_SSP5.csv",row.names = F)

