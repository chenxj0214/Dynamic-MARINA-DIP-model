# MARINA-P-dynamic 
# Author: Xuanjing Chen, Jiechen Wu
# Date: 2022.11.23
# Email: cxj0214@email.swu.edu.cn
# citation: 


#set directory, setting the pathway of the folder with the model.
setwd("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data") 

#import Pox.pool.sro.2000，Pox.pool.int.2000，kp
year_initial<-1960
source("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Stript/Initial_year_setting.R")

#import inputs data 
inputs<-read.csv("Inputs/MARINA_2.0/inputs_data.csv")
parameters<-read.csv("Inputs/MARINA_2.0/Paramters_data.csv")



#fixed parameters 
Thickness<-read.csv("Thickness_subbasin.csv") #soil Thickness,m

Tsro=rep(0.3,11) #soil thickness of the surface layer,unit: m
Tint=Thickness[,"Thickness"]-Tsro #soil thickness of the interflow layers,unit: m
frwthag.gwb=0.5 #the fraction of the weathering phosphorus contributing to the groundwater layer
  
p<- read.csv("BD_subbasin.csv")
p<-p[,"BD"]#the bulk density of the soil, kg/m3



##########calculated parameter 

Psremax<- 0.5*1/3*(parameters[,"Alox.Feox"])# Calculated based on Schoumans and Groenendijk (2000)

RSdifantwthag<-inputs[,"frAgr"]*0.26/(1+(inputs[,"Rnat"]/0.85)^-2) #DIP inputs from the weathering in agricultural areas, unit:kg/ha/year

#####calculate the fractions of water flow in the interflow and groundwater layer,
fQgwb<-parameters[,"fQgwb"]#the fraction of water flows in the groundwater layer
parameters[,"frint"]<-(1-parameters[,"fQsro"])*(1-fQgwb)
parameters[,"frgwb"]<- (1-parameters[,"fQsro"])*fQgwb


#the function to calcuate Rsdifant.sro (F2)
#i subbbasin
#t  year  
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
   #print(i)
  }
}

Results.sro<- cbind(inputs[,"code"],inputs[,"sub_basin"],inputs[,"year"], Pacc,Ple,RSdifantDIP.sro)


#write.csv(Results.sro,"Results.sro_1960.csv",row.names = F)


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
    #print(i)
  }
}

Results.int<- cbind(inputs[,"code"],inputs[,"sub_basin"],inputs[,"year"], RSdifantDIP.int)

#write.csv(Results.int,"Results.int_1960.csv",row.names = F)




#the function to calcuate Rsdifant.gwb (F6)
RSdifantDIP.gwb<-c()
DIPgwb<-c()
for (j in 1:11){
  for (t in 1:100){
    i=t+(j-1)*100
    
    DIPgwb[i]<- (RSdifantwthag[i]*frwthag.gwb/(inputs[,"Rnat"][i]*parameters[,"frgwb"][i]))/10 #averaged DIP concentrations of the water flows in the interflow layers,unit: mg/l or g/m3
    if(is.na(RSdifantwthag[i])){
      DIPgwb[i]<-NA
    }else{
      if(DIPgwb[i]=="Inf"){
        DIPgwb[i]=0
      }else{
        DIPgwb[i]<-DIPgwb[i]
      }
    }
    
   
    RSdifantDIP.gwb[i]<-inputs[,"Rnat"][i]*DIPgwb[i]*parameters[,"frgwb"][i]*10  # the amount of DIP input to rivers from the groundwater layer, unit: kg/ha/year
    
    
  }
}


Results.gwb<- cbind(inputs[,"code"],inputs[,"sub_basin"],inputs[,"year"], RSdifantDIP.gwb)
#write.csv(Results.gwb,"Results.gwb_1960.csv",row.names = F)
a1<-data.frame(Pox.pool.sro*inputs[,"areacell"]*100/1000/1000/1000,Pox.pool.int*inputs[,"areacell"]*100/1000/1000/1000)
a1<-na.omit(a1)
View(a1)
#write.csv(a1,"a1.")
code<-inputs[,"code"]
sub_basin<-inputs[,"sub_basin"]
year<-inputs[,"year"]
RSdifantDIP<-RSdifantDIP.sro+RSdifantDIP.int+RSdifantDIP.gwb
Results.outputs.ha<-data.frame(code,sub_basin,year, Pacc,Ple,RSdifantDIP.sro,RSdifantDIP.int,RSdifantDIP.gwb,RSdifantDIP)# results in kg/ha
Results.outputs.ha<-Results.outputs.ha[order(Results.outputs.ha$code,Results.outputs.ha$year,decreasing = F),]
####################################results analysis

RSdifantDIP.total<-RSdifantDIP*inputs[,"areacell"]*100
RSdifantDIP.total<-data.frame(code,sub_basin,year,RSdifantDIP.total)

RSdifantDIP.total.Yangtze<-aggregate(RSdifantDIP.total[,"RSdifantDIP.total"],by=list(year),FUN=sum)
names(RSdifantDIP.total.Yangtze)<-c("year","RSdifantDIP.total")
RSdifantDIP.total.Yangtze[,"code"]=rep(0,100)
RSdifantDIP.total.Yangtze[,"sub_basin"]=rep("Yangtze",100)


RSdifantDIP.total<-rbind(RSdifantDIP.total,RSdifantDIP.total.Yangtze)
RSdifantDIP.total<-RSdifantDIP.total[order(RSdifantDIP.total$code,RSdifantDIP.total$year,decreasing = F),]
View(RSdifantDIP.total)

#################export result
write.csv(Results.outputs.ha,"Outputs/Results.outputs.ha.csv",row.names = F)
write.csv(RSdifantDIP.total,"Outputs/RSdifantDIP.total.csv",row.names = F)



###################calculate DIP inputs to river using original MARINA model
inputs<-read.csv("Inputs/MARINA_2.0/inputs_data.csv")
aDIP<-0.85
eDIP<-0.29
R<-inputs[,"Rnat"]

bDIP<-2.0
ECdip<-26 #kg/km2/yr for weathering4
fRnatDIP <- ((1+(R/aDIP)^-bDIP)^-1)
FEwsDIP <- fRnatDIP*eDIP


RSdifagDIP<-inputs[,"DIPTant"]*FEwsDIP
DIPdifwthag<-inputs[,"frAgr"]*inputs[,"areacell"]*ECdip*fRnatDIP
RSdifantDIP<-RSdifagDIP+DIPdifwthag

RSdifantDIP<-data.frame(code=rep(inputs$code),sub_basin=rep(inputs$sub_basin),year=rep(inputs$year),RSdifantDIP)
Yangtze_RSdifantDIP<-aggregate(RSdifantDIP[,"RSdifantDIP"],by=list(year),FUN=sum)
names(Yangtze_RSdifantDIP)<-c("year","RSdifantDIP")

setwd("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data/Outputs/") 
write.csv(RSdifantDIP,"RSdifantDIP_MARINA_Original.csv",row.names = F )
write.csv(Yangtze_RSdifantDIP,"RSdifantDIP_MARINA_Yangtze_Original.csv",row.names = F )



