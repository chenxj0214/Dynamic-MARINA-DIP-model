# MARINA-P-dynamic 
# Author: Xuanjing Chen Jiechen Wu
# Date: 2022.11.23
# Email: cxj0214@email.swu.edu.cn
# citation: 

#set directory, setting the pathway of the folder with the model.
setwd("C:/Users/chenxj/OneDrive - 西南大学/Postdoc/Paper 5/Data") 

#import inputs data 
inputs<-read.csv("Inputs/MARINA_2.0/inputs_data.csv")
parameters<-read.csv("Inputs/MARINA_2.0/Paramters_data.csv")

#####calculate the fractions of water flow in the`` interflow and groundwater layer,
fQgwb<-parameters[,"fQgwb"]#the fraction of water flows in the groundwater layer
parameters[,"frint"]<-(1-parameters[,"fQsro"])*(1-fQgwb)
parameters[,"frgwb"]<- (1-parameters[,"fQsro"])*fQgwb


#####results for initial year, 2000
inputs.2000<-inputs[inputs$year==2000,]
parameters.2000<-parameters[parameters$year==2000,]


#fixed parameters 

Thickness<-read.csv("Thickness_subbasin.csv") #soil Thickness,m

Tsro=rep(0.3,11) #soil thickness of the surface layer,unit: m
Tint=Thickness[,"Thickness"]-Tsro #soil thickness of the interflow layers,unit: m
frwthag.gwb=0.5 #the fraction of the weathering phosphorus contributing to the groundwater layer

p<- read.csv("BD_subbasin.csv")
p<-p[,"BD"]#the bulk density of the soil, kg/m3


Psremax<- 0.5*1/3*(parameters.2000[,"Alox.Feox"])# Calculated based on Schoumans and Groenendijk (2000)
RSdifantwthag.2000<-(inputs.2000[,"frAgr"]*0.26/(1+(inputs.2000[,"Rnat"]/0.85)^-2)) #DIP inputs from the weathering in agricultural areas, unit:kg/ha
frwthag.gwb=0.5
#initial pool
# the oxalate extractable P pool in the surface layer in 2000, unit: kg/ha
aDIP<-0.85 # Strokal et al., 2016
bDIP<-2.0 # Strokal et al., 2016
eDIP<-0.29# Strokal et al., 2016



fRnatDIP <- ((1+(inputs.2000[,"Rnat"]/aDIP)^-bDIP)^-1)
FEwsDIP <- fRnatDIP*eDIP
DIPdifant.2000<-inputs.2000[,'DIPTant']*FEwsDIP ##Total DIP inputs to rivers for sub-basin in 2000,Calculated original MARINA model (Strokal et al., 2016)
#DIPant1970<-c(26371526.61,9904303.532,49353248.77,16525967.29,47982724.94, 41016113.15,19354781.95,17423968.94,41699193.25,23222065.33)#net P inputs (kg) for sub-basin in 1970,Calculated original MARINA model (Strokal et al., 2016),Per+Pma+Phum-Pex+Pwthag
             
  
Pox.sro.1960<-parameters.2000[,"Alox.Feox"]*0.02*0.031 ##following Strokal and Vries (2012).g/kg
Pox.pool.sro.1960<-p*Tsro*10*Pox.sro.1960
frPac<-(inputs.2000[,'DIPTant']+RSdifantwthag.2000*inputs.2000[,"areacell"]*100-DIPdifant.2000-RSdifantwthag.2000*inputs.2000[,"areacell"]*100)/(inputs.2000[,'DIPTant']+RSdifantwthag.2000*inputs.2000[,"areacell"]*100)
DIPant1960.2000.total<-(0+inputs.2000[,"DIPTant"]+RSdifantwthag.2000*inputs.2000[,"areacell"]*100)*(2000-year_initial)/2#kg
DIPant1960.2000<- DIPant1960.2000.total/inputs.2000[,"areacell"]/100#kg/ha
Pox.pool.sro.2000<-Pox.pool.sro.1960+frPac*DIPant1960.2000# the oxalate extractable P pool in the surface layer in 2000, unit: kg/ha


# the oxalate extractable P pool in the interflow layer in 2000, unit: kg/ha
Pox.int.2000<-Pox.sro.1960 ###Oxalate extractable P concentration in the interflow layer for sub-basin j in 2000.
Pox.pool.int.2000<-p*Tint*10*Pox.int.2000  # the oxalate extractable P pool in the interflow layer in 2000, unit: kg/ha

  
  
  
############calculate the Langmuir adsorption constant,m3/g P

DIPGN.2000<-((DIPdifant.2000+RSdifantwthag.2000*inputs.2000[,"areacell"]*100)/(inputs.2000[,"areacell"]*100))/inputs.2000[,"Rnat"]/10#mg/L
DIPgwb.2000<- (RSdifantwthag.2000*frwthag.gwb/(inputs.2000[,"Rnat"]*parameters.2000[,"frgwb"]))/10#mg/L
DIPgwb.2000[which(DIPgwb.2000=="Inf")]=0
Pox.sro.2000<-Pox.pool.sro.2000/p/Tsro/10
kp=(parameters.2000[,"fQsro"]*(Pox.pool.sro.2000/(0.93*p*Tsro*Psremax-Pox.pool.sro.2000))+parameters.2000[,"frint"]*Pox.pool.int.2000/(0.93*p*Tint*Psremax-Pox.pool.int.2000))/
   (DIPGN.2000-parameters.2000[,"frgwb"]*DIPgwb.2000)

