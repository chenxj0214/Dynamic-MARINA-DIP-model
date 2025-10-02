# MARINA-P-dynamic 
# Author: Xuanjing Chen Jiechen Wu
# Date: 2022.11.23
# Email: cxj0214@email.swu.edu.cn


#set directory, setting the pathway of the folder with the model.
# MARINA-P-dynamic 
# Author: Xuanjing Chen Jiechen Wu
# Date: 2022.11.23
# Email: cxj0214@email.swu.edu.cn
# citation: 


#set directory, setting the pathway of the folder with the model.
setwd("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data") 

#import Pox.pool.sro.2000，Pox.pool.int.2000，kp
year_initial<-1960
source("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Stript/Initial_year_setting.R")

#import inputs data 
#scenario 
inputs_S1_2.6<-read.csv("Inputs/MARINA_2.0/inputs_data_S1_2.6.csv")
inputs_S1_8.5<-read.csv("Inputs/MARINA_2.0/inputs_data_S1_8.5.csv")
inputs_S2_2.6<-read.csv("Inputs/MARINA_2.0/inputs_data_S2_2.6.csv")
inputs_S2_8.5<-read.csv("Inputs/MARINA_2.0/inputs_data_S2_8.5.csv")
inputs_S3_2.6<-read.csv("Inputs/MARINA_2.0/inputs_data_S3_2.6.csv")
inputs_S3_8.5<-read.csv("Inputs/MARINA_2.0/inputs_data_S3_8.5.csv")
inputs_S1<- read.csv("Inputs/MARINA_2.0/inputs_data_S1.csv")
inputs_S2<- read.csv("Inputs/MARINA_2.0/inputs_data_S2.csv")
inputs_S3<- read.csv("Inputs/MARINA_2.0/inputs_data_S3.csv")



parameters<-read.csv("Inputs/MARINA_2.0/Paramters_data.csv")

#####calculate the fractions of water flow in the interflow and groundwater layer,
fQgwb<-parameters[,"fQgwb"]#the fraction of water flows in the groundwater layer
parameters[,"frint"]<-(1-parameters[,"fQsro"])*(1-fQgwb)
parameters[,"frgwb"]<- (1-parameters[,"fQsro"])*fQgwb

#fixed parameters 

Thickness<-read.csv("Thickness_subbasin.csv") #soil Thickness,m

Tsro=rep(0.3,11) #soil thickness of the surface layer,unit: m
Tint=Thickness[,"Thickness"]-Tsro #soil thickness of the interflow layers,unit: m
frwthag.gwb=0.5 #the fraction of the weathering phosphorus contributing to the groundwater layer

p<- read.csv("BD_subbasin.csv")
p<-p[,"BD"]#the bulk density of the soil, kg/m3



##########calculated parameter 

Psremax<- 0.5*1/3*(parameters[,"Alox.Feox"])# Calculated based on Schoumans and Groenendijk (2000)



P_dynamic_model<-function(inputs,parameters){
  

  #the function to calcuate Rsdifant.sro (F2)
  #i subbbasin
  #t  year  
  RSdifantwthag<-inputs[,"frAgr"]*0.26/(1+(inputs[,"Rnat"]/0.85)^-2) #DIP inputs from the weathering in agricultural areas, unit:kg/ha/year
  
  Pacc<-c()
  Ple<-c()
  Pox.pool.sro<-c()
  DIPsro<-c()
  RSdifantDIP.sro<-c()
  for (j in 1:11){
    Pox.pool.sro[1+100*(j-1)]<- Pox.pool.sro.2000[j] 
    for (t in 1:100){
      i=t+(j-1)*100
      DIPsro[i]<-Pox.pool.sro[i]/((0.93*p[j]*Tsro[j]*Psremax[i]-Pox.pool.sro[i])*kp[j]) #averaged DIP concentrations of the water flows in the surface layers,unit: mg/l or g/m3
      RSdifantDIP.sro[i]<-inputs[,"Rnat"][i]*parameters[,"fQsro"][i]*DIPsro[i]*10 # the amount of DIP input to rivers from the surface layer, unit: kg/ha/year
      Ple[i]<-DIPsro[i]*inputs[,"Rnat"][i]*(1-parameters[,"fQsro"][i])*10 # the amount of DIP leaching from the surface layer to the interflow layer, unit: kg/ha/year
      Pacc[i]<-inputs[,"DIPTant"][i]/(inputs[,"areacell"][i]*100)-Ple[i]-RSdifantDIP.sro[i] #the amount of DIP accumulated in the surface layer unit: kg/ha/year
      i=i+1
      if (round((i-1)/100) == (i-1)/100 ){
        next
      }else{
        Pox.pool.sro[i]<-Pox.pool.sro[i-1]+Pacc[i-1]-Ple[i-1]-RSdifantDIP.sro[i-1]
      }
      print(i)
    }
  }
  
  Results.sro<- cbind(inputs[,"code"],inputs[,"sub_basin"],inputs[,"year"], Pacc,Ple,RSdifantDIP.sro)
  
  
  
  
  
  #the function to calcuate Rsdifant.int (F5)
  RSdifantDIP.int<-c()
  Pox.pool.int<-c()
  DIPint<-c()
  for (j in 1:11){
    Pox.pool.int[1+100*(j-1)]<- Pox.pool.int.2000[j] 
    for (t in 1:100){
      i=t+(j-1)*100
      DIPint[i]<-Pox.pool.int[i]/((0.93*p[j]*Tint[j]*Psremax[i]-Pox.pool.int[i])*kp[j])  #averaged DIP concentrations of the water flows in the interflow layers,unit: mg/l or g/m3
      RSdifantDIP.int[i]<-inputs[,"Rnat"][i]*parameters[,"frint"][i]*DIPint[i]*10 # the amount of DIP input to rivers from the surface layer, unit: kg/ha/year
      
      i=i+1
      if (round((i-1)/100) == (i-1)/100 ){
        next
      }else{
        Pox.pool.int[i]<-Pox.pool.int[i-1]+Ple[i-1]+(1-frwthag.gwb)*RSdifantwthag[i-1]-RSdifantDIP.int[i-1]
      }
      print(i)
    }
  }
  
  
  
  
  
  #the function to calcuate Rsdifant.gwb (F6)
  RSdifantDIP.gwb<-c()
  DIPgwb<-c()
  for (j in 1:11){
    for (t in 1:100){
      i=t+(j-1)*100
      
      DIPgwb[i]<- RSdifantwthag[i]*frwthag.gwb/(inputs[,"Rnat"][i]*parameters[,"frgwb"][i]) #averaged DIP concentrations of the water flows in the interflow layers,unit: mg/l or g/m3
      
      
      if(DIPgwb[i]=="Inf"){
        DIPgwb[i]=0
      }else{
        DIPgwb[i]<-DIPgwb[i]
      }
      
      RSdifantDIP.gwb[i]<-inputs[,"Rnat"][i]*DIPgwb[i]*parameters[,"frgwb"][i]  # the amount of DIP input to rivers from the groundwater layer, unit: kg/ha/year
      
      
    }
  }
  
  
  code<-inputs[,"code"]
  sub_basin<-inputs[,"sub_basin"]
  year<-inputs[,"year"]
  RSdifantDIP<-RSdifantDIP.sro+RSdifantDIP.int+RSdifantDIP.gwb
  Results.outputs<-data.frame(code,sub_basin,year, Pacc,Ple,RSdifantDIP.sro,RSdifantDIP.int,RSdifantDIP.gwb,RSdifantDIP)
 
  
  ####################################results analysis
  
  RSdifantDIP.total<-RSdifantDIP*inputs[,"areacell"]*100
  RSdifantDIP.total<-data.frame(code,sub_basin,year,RSdifantDIP.total)
  
  RSdifantDIP.total.Yangtze<-aggregate(RSdifantDIP.total[,"RSdifantDIP.total"],by=list(year),FUN=sum)
  names(RSdifantDIP.total.Yangtze)<-c("year","RSdifantDIP.total")
  RSdifantDIP.total.Yangtze[,"code"]<-rep(0,100)
  RSdifantDIP.total.Yangtze[,"sub_basin"]<-rep("Yangtze",100)

  RSdifantDIP.total<-rbind(RSdifantDIP.total,RSdifantDIP.total.Yangtze)
  RSdifantDIP.total<-RSdifantDIP.total[order(RSdifantDIP.total$code,RSdifantDIP.total$year,decreasing = F),]
  return(RSdifantDIP.total)
  
}



Results.outputs.S1_2.6<- P_dynamic_model(inputs_S1_2.6,parameters)
Results.outputs.S1_8.5<- P_dynamic_model(inputs_S1_8.5,parameters)
Results.outputs.S2_2.6<- P_dynamic_model(inputs_S2_2.6,parameters)
Results.outputs.S2_8.5<- P_dynamic_model(inputs_S2_8.5,parameters)
Results.outputs.S3_2.6<- P_dynamic_model(inputs_S3_2.6,parameters)
Results.outputs.S3_8.5<- P_dynamic_model(inputs_S3_8.5,parameters)

Results.outputs.S1<- P_dynamic_model(inputs_S1,parameters)
Results.outputs.S2<- P_dynamic_model(inputs_S2,parameters)
Results.outputs.S3<- P_dynamic_model(inputs_S3,parameters)
                                         
Results.outputs.scenario<-data.frame(Results.outputs.S1_2.6[,"year"],Results.outputs.S1_2.6[,"code"],Results.outputs.S1_2.6[,"sub_basin"],Results.outputs.S1_2.6[,"RSdifantDIP.total"],Results.outputs.S1_8.5[,"RSdifantDIP.total"],Results.outputs.S2_2.6[,"RSdifantDIP.total"],Results.outputs.S2_8.5[,"RSdifantDIP.total"],Results.outputs.S3_2.6[,"RSdifantDIP.total"],Results.outputs.S3_8.5[,"RSdifantDIP.total"],Results.outputs.S1[,"RSdifantDIP.total"],Results.outputs.S2[,"RSdifantDIP.total"],Results.outputs.S3[,"RSdifantDIP.total"])
names(Results.outputs.scenario)<-c("year","code","sub_basin","S1_2.6","S1_8.5","S2_2.6","S2_8.5","S3_2.6","S3_8.5","S1","S2","S3")
View(Results.outputs.scenario)
#export results

write.csv(Results.outputs.scenario,"Outputs/Results.outputs.scenario.csv",row.names = F)


########
sub_basin_names<-unique(Results.outputs.scenario$sub_basin)
year<-c("2010s","2030s","2050s","2070s","2090s")



df<-Results.outputs.scenario

df[,"year_group"]<-floor((df$year - 2000) / 20) * 20 + 2010
df_avg <- aggregate(. ~ year_group+sub_basin, data = df, FUN = mean)
df_avg<-df_avg[,-3]
df_avg<-df_avg[order(df_avg$code,df_avg$year_group,decreasing = F),]
View(df_avg)
names(df_avg)<-c("year","sub_basin","code","S1_2.6","S1_8.5","S2_2.6","S2_8.5","S3_2.6","S3_8.5","S1","S2","S3")
write.csv(df_avg,"Outputs/Results.outputs.scenario_ave.csv",row.names = F)
