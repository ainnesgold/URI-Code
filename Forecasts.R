library(tidyverse)
library(gridExtra)
library(viridis)
library(ggpubr)

#HalfForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/Half_output.csv", skip=1)

#names(HalfForecast)[names(HalfForecast) == "Data"] <- "Year"
#names(HalfForecast)[names(HalfForecast) == "X1..Phytoplankton"] <- "PredPhyto"
#names(HalfForecast)[names(HalfForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
#names(HalfForecast)[names(HalfForecast) == "X3..Zooplankton"] <- "PredZoo"
#names(HalfForecast)[names(HalfForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
#names(HalfForecast)[names(HalfForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
#names(HalfForecast)[names(HalfForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
#names(HalfForecast)[names(HalfForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
#names(HalfForecast)[names(HalfForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
#names(HalfForecast)[names(HalfForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
#names(HalfForecast)[names(HalfForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
#names(HalfForecast)[names(HalfForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
#names(HalfForecast)[names(HalfForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
#names(HalfForecast)[names(HalfForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
#names(HalfForecast)[names(HalfForecast) == "X14..Seabirds"] <- "PredSeabirds"
#names(HalfForecast)[names(HalfForecast) == "X15..Detritus"] <- "PredDetritus"

#colnames(HalfForecast) <- paste(colnames(HalfForecast), sep = "_", "1")
#HalfForecast<-HalfForecast[-542,]


StatusQuoForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE simulations - no abm/statusquo_output2.csv", skip=1)

names(StatusQuoForecast)[names(StatusQuoForecast) == "Data"] <- "Year"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X3..Zooplankton"] <- "PredZoo"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(StatusQuoForecast)[names(StatusQuoForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(StatusQuoForecast) <- paste(colnames(StatusQuoForecast), sep = "_", "2")
StatusQuoForecast<-StatusQuoForecast[-542,]


ZeroForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE simulations - no abm/zero_output2.csv", skip=1)

names(ZeroForecast)[names(ZeroForecast) == "Data"] <- "Year"
names(ZeroForecast)[names(ZeroForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(ZeroForecast)[names(ZeroForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(ZeroForecast)[names(ZeroForecast) == "X3..Zooplankton"] <- "PredZoo"
names(ZeroForecast)[names(ZeroForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(ZeroForecast)[names(ZeroForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(ZeroForecast)[names(ZeroForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(ZeroForecast)[names(ZeroForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(ZeroForecast)[names(ZeroForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(ZeroForecast)[names(ZeroForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(ZeroForecast)[names(ZeroForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(ZeroForecast)[names(ZeroForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(ZeroForecast)[names(ZeroForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(ZeroForecast)[names(ZeroForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(ZeroForecast)[names(ZeroForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(ZeroForecast)[names(ZeroForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(ZeroForecast) <- paste(colnames(ZeroForecast), sep = "_", "3")

ZeroForecast<-ZeroForecast[-542,]

##
Forecast2000<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE simulations - no abm/2000_output2.csv", skip=1)

names(Forecast2000)[names(Forecast2000) == "Data"] <- "Year"
names(Forecast2000)[names(Forecast2000) == "X1..Phytoplankton"] <- "PredPhyto"
names(Forecast2000)[names(Forecast2000) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Forecast2000)[names(Forecast2000) == "X3..Zooplankton"] <- "PredZoo"
names(Forecast2000)[names(Forecast2000) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Forecast2000)[names(Forecast2000) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Forecast2000)[names(Forecast2000) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Forecast2000)[names(Forecast2000) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Forecast2000)[names(Forecast2000) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Forecast2000)[names(Forecast2000) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Forecast2000)[names(Forecast2000) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Forecast2000)[names(Forecast2000) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Forecast2000)[names(Forecast2000) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Forecast2000)[names(Forecast2000) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Forecast2000)[names(Forecast2000) == "X14..Seabirds"] <- "PredSeabirds"
names(Forecast2000)[names(Forecast2000) == "X15..Detritus"] <- "PredDetritus"

colnames(Forecast2000) <- paste(colnames(Forecast2000), sep = "_", "4")

Forecast2000<-Forecast2000[-542,]

##not 2012 anymore, just an intermediate avg of SQ and extreme. dont feel like changing all the names
Forecast2012<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE simulations - no abm/intermediateoutput.csv", skip=1)

names(Forecast2012)[names(Forecast2012) == "Data"] <- "Year"
names(Forecast2012)[names(Forecast2012) == "X1..Phytoplankton"] <- "PredPhyto"
names(Forecast2012)[names(Forecast2012) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Forecast2012)[names(Forecast2012) == "X3..Zooplankton"] <- "PredZoo"
names(Forecast2012)[names(Forecast2012) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Forecast2012)[names(Forecast2012) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Forecast2012)[names(Forecast2012) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Forecast2012)[names(Forecast2012) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Forecast2012)[names(Forecast2012) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Forecast2012)[names(Forecast2012) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Forecast2012)[names(Forecast2012) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Forecast2012)[names(Forecast2012) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Forecast2012)[names(Forecast2012) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Forecast2012)[names(Forecast2012) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Forecast2012)[names(Forecast2012) == "X14..Seabirds"] <- "PredSeabirds"
names(Forecast2012)[names(Forecast2012) == "X15..Detritus"] <- "PredDetritus"

colnames(Forecast2012) <- paste(colnames(Forecast2012), sep = "_", "5")

Forecast2012<-Forecast2012[-542,]

ecosim<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE simulations - no abm/ecosim_model.csv", skip=1)

names(ecosim)[names(ecosim) == "Data"] <- "Year"
names(ecosim)[names(ecosim) == "X1..Phytoplankton"] <- "PredPhyto"
names(ecosim)[names(ecosim) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(ecosim)[names(ecosim) == "X3..Zooplankton"] <- "PredZoo"
names(ecosim)[names(ecosim) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(ecosim)[names(ecosim) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(ecosim)[names(ecosim) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(ecosim)[names(ecosim) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(ecosim)[names(ecosim) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(ecosim)[names(ecosim) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(ecosim)[names(ecosim) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(ecosim)[names(ecosim) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(ecosim)[names(ecosim) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(ecosim)[names(ecosim) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(ecosim)[names(ecosim) == "X14..Seabirds"] <- "PredSeabirds"
names(ecosim)[names(ecosim) == "X15..Detritus"] <- "PredDetritus"

colnames(ecosim) <- paste(colnames(ecosim), sep = "_", "6")





#temp <- HalfForecast %>%
  #select(Year_1, PredPlank_1, PredPisc_1, PredBenth_1, PredCB_1, PredZoo_1)
#colnames(temp)[1] <- "Year"

temp2 <- StatusQuoForecast %>%
  select(Year_2, PredPlank_2, PredPisc_2, PredBenth_2, PredCB_2, PredZoo_2,
         PredLgPIP_2, PredSmPIP_2)
colnames(temp2)[1] <- "Year"

temp3 <- ZeroForecast %>%
  select(Year_3, PredPlank_3, PredPisc_3, PredBenth_3, PredCB_3, PredZoo_3,
         PredLgPIP_3, PredSmPIP_3)
colnames(temp3)[1] <- "Year"

temp4 <- Forecast2000 %>%
  select(Year_4, PredPlank_4, PredPisc_4, PredBenth_4, PredCB_4, PredZoo_4,
         PredLgPIP_4, PredSmPIP_4)
colnames(temp4)[1] <- "Year"

temp5 <- Forecast2012 %>%
  select(Year_5, PredPlank_5, PredPisc_5, PredBenth_5, PredCB_5, PredZoo_5,
         PredLgPIP_5, PredSmPIP_5)
colnames(temp5)[1] <- "Year"

temp6 <- ecosim %>%
  select(Year_6, PredPlank_6, PredPisc_6, PredBenth_6, PredCB_6, PredZoo_6,
         PredLgPIP_6, PredSmPIP_6)
colnames(temp6)[1] <- "Year"


#temp<-left_join(temp, temp2)
temp<-left_join(temp2, temp3, by="Year")
temp<-left_join(temp, temp4, by="Year")
temp<-left_join(temp, temp5, by="Year")
temp<-left_join(temp, temp6, by="Year")



ObservedTimeSeries<-read.csv("~/Google Drive/URI/DATA/Ecosim/V9_Obs_TimeSeries.csv", skip=2,3)
colnames(ObservedTimeSeries)[1] <- "Year"
colnames(ObservedTimeSeries)[2] <- "BenthFish"
colnames(ObservedTimeSeries)[3] <- "PiscFish"
colnames(ObservedTimeSeries)[4] <- "PlankFish"
colnames(ObservedTimeSeries)[5] <- "CB"
colnames(ObservedTimeSeries)[6] <- "SmallSquid"
colnames(ObservedTimeSeries)[7] <- "LargeSquid"
colnames(ObservedTimeSeries)[8] <- "Benth_F"
colnames(ObservedTimeSeries)[9] <- "Pisc_F"
colnames(ObservedTimeSeries)[10] <- "CB_F"
colnames(ObservedTimeSeries)[11] <- "LgPIP_F"
colnames(ObservedTimeSeries)[12] <- "Plank_F"
colnames(ObservedTimeSeries)[13] <- "Cultured Shellfish"
colnames(ObservedTimeSeries)[14] <- "Phytoplankton"
colnames(ObservedTimeSeries)[15] <- "Ctenophores"

#ObservedTimeSeries<-gather(ObservedTimeSeries, key="FG", value="Biomass", -Year)
#ObservedTimeSeries<-ObservedTimeSeries %>%
  #filter(FG!="Benth_F") %>%
  #filter(FG!="Pisc_F") %>%
  #filter(FG!="CB_F") %>%
  #filter(FG!="LgPIP_F") %>%
  #filter(FG!="Plank_F")

#ggplot(data=ObservedTimeSeries, aes(x=Year, y=Biomass, col=FG)) + geom_line()

ObservedTimeSeries<- ObservedTimeSeries %>%
  select(Year, BenthFish, PiscFish, PlankFish, CB, SmallSquid, LargeSquid)




##Plankivorous fish Forecasts
PlankABSBio<-temp %>%
  filter(Year <= 2030) %>%
  select(Year, PredPlank_2, PredPlank_3, PredPlank_4, PredPlank_5, PredPlank_6)
PlankABSBio<-gather(PlankABSBio, key=Simulation, value=Value, -Year)

PlankABSBio$Simulation <- factor(PlankABSBio$Simulation, 
                                     levels=c( 
                                       "PredPlank_3", 
                                       "PredPlank_2", "PredPlank_5", "PredPlank_4", "PredPlank_6"), 
                                     labels=c("Zero", "Status Quo", "Intermediate", 
                                              "Extreme", "Model"))


p1<-ggplot(ObservedTimeSeries, aes(x=Year, y=PlankFish)) + geom_point()+
  geom_line(data=PlankABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(values=c("slateblue3", "thistle", "sandybrown", "darkorange3", "black"), 
                     name="Forage Fish \nHarvest Scenario")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Forage Fish")

##Piscivorous Fish forecasts
PiscABSBio<-temp %>%
  filter(Year <= 2030) %>%
  select(Year, PredPisc_2, PredPisc_3, PredPisc_4, PredPisc_5, PredPisc_6)
PiscABSBio<-gather(PiscABSBio, key=Simulation, value=Value, -Year)

PiscABSBio$Simulation <- factor(PiscABSBio$Simulation, 
                                     levels=c( 
                                       "PredPisc_3", 
                                       "PredPisc_2", "PredPisc_5", "PredPisc_4", "PredPisc_6"), 
                                     labels=c("Zero", "Status Quo", 
                                              "Intermediate", "Extreme", "Model"))


p2<-ggplot(ObservedTimeSeries, aes(x=Year, y=PiscFish)) + geom_point()+
  geom_line(data=PiscABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(values=c("slateblue3", "thistle", "sandybrown", "darkorange3", "black"), 
       name="Planktivorous Fish \nHarvest Scenario")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Piscivorous Fish")








##Benthivorous Fish forecasts
BenthABSBio<-temp %>%
  filter(Year <= 2030) %>%
  select(Year, PredBenth_2, PredBenth_3, PredBenth_4, PredBenth_5, PredBenth_6)
BenthABSBio<-gather(BenthABSBio, key=Simulation, value=Value, -Year)

BenthABSBio$Simulation <- factor(BenthABSBio$Simulation, 
                                     levels=c( 
                                       "PredBenth_3", 
                                       "PredBenth_2", "PredBenth_5", "PredBenth_4", "PredBenth_6"), 
                                     labels=c("Zero", "Status Quo", 
                                              "Intermediate", "Extreme", "Model"))


p3<-ggplot(ObservedTimeSeries, aes(x=Year, y=BenthFish)) + geom_point()+
  geom_line(data=BenthABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(values=c("slateblue3", "thistle", "sandybrown", "darkorange3", "black"), 
                     name="Planktivorous Fish \nHarvest Scenario") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Benthivorous Fish")


##Carn Benthos forecasts
CBABSBio<-temp %>%
  filter(Year <= 2030) %>%
  select(Year, PredCB_2, PredCB_3, PredCB_4, PredCB_5, PredCB_6)
CBABSBio<-gather(CBABSBio, key=Simulation, value=Value, -Year)

CBABSBio$Simulation <- factor(CBABSBio$Simulation, 
                                     levels=c( 
                                       "PredCB_3",
                                       "PredCB_2", "PredCB_5", "PredCB_4", "PredCB_6"), 
                                     labels=c("Zero", "Status Quo", 
                                              "Intermediate", "Extreme", "Model"))




p4<-ggplot(ObservedTimeSeries, aes(x=Year, y=CB)) + geom_point()+
  geom_line(data=CBABSBio, aes(x=Year, y=Value, color=Simulation), position=position_dodge(width=0.1))+
  scale_color_manual(values=c("slateblue3", "thistle", "sandybrown", "darkorange3", "black"), 
                     name="Planktivorous Fish \nHarvest Scenario")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Carnivorous Benthos")


####SQUID

LgSquidBio<-temp %>%
  filter(Year <= 2030) %>%
  select(Year, PredLgPIP_2, PredLgPIP_3, PredLgPIP_4, PredLgPIP_5, PredLgPIP_6)
LgSquidBio<-gather(LgSquidBio, key=Simulation, value=Value, -Year)

LgSquidBio$Simulation <- factor(LgSquidBio$Simulation, 
                              levels=c( 
                                "PredLgPIP_3",
                                "PredLgPIP_2", "PredLgPIP_5", "PredLgPIP_4", "PredLgPIP_6"), 
                              labels=c("Zero", "Status Quo", 
                                       "Intermediate", "Extreme", "Model"))




p5<-ggplot(ObservedTimeSeries, aes(x=Year, y=LargeSquid)) + geom_point()+
  geom_line(data=LgSquidBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(values=c("slateblue3", "thistle", "sandybrown", "darkorange3", "black"), 
                     name="Planktivorous Fish \nHarvest Scenario")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Large Squid")

SmSquidBio<-temp %>%
  filter(Year <= 2030) %>%
  select(Year, PredSmPIP_2, PredSmPIP_3, PredSmPIP_4, PredSmPIP_5, PredSmPIP_6)
SmSquidBio<-gather(SmSquidBio, key=Simulation, value=Value, -Year)

SmSquidBio$Simulation <- factor(SmSquidBio$Simulation, 
                                levels=c( 
                                  "PredSmPIP_3",
                                  "PredSmPIP_2", "PredSmPIP_5", "PredSmPIP_4", "PredSmPIP_6"), 
                                labels=c("Zero", "Status Quo", 
                                         "Intermediate", "Extreme", "Model"))




p6<-ggplot(ObservedTimeSeries, aes(x=Year, y=SmallSquid)) + geom_point()+
  geom_line(data=SmSquidBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(values=c("slateblue3", "thistle", "sandybrown", "darkorange3", "black"), 
                     name="Planktivorous Fish \nHarvest Scenario")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Small Squid")






ggarrange(p1, p2, p3, p4, p5, p6, 
          nrow=3, ncol=2, common.legend = TRUE, legend="bottom")













#######Making a plot of just ecosim predictions ABSOLUTE
HalfForecast<-read.csv("~/Desktop/URI/DATA/Ecosim/forecast/Half_output.csv", skip=1)

names(HalfForecast)[names(HalfForecast) == "Data"] <- "Year"
names(HalfForecast)[names(HalfForecast) == "X1..Phytoplankton"] <- "Phytoplankton"
names(HalfForecast)[names(HalfForecast) == "X2..Benthic.Algae"] <- "Benthic Algae"
names(HalfForecast)[names(HalfForecast) == "X3..Zooplankton"] <- "Zooplankton"
names(HalfForecast)[names(HalfForecast) == "X4..Gelatinous.Zooplankton"] <- "Gelatinous Zooplankton"
names(HalfForecast)[names(HalfForecast) == "X5..Deposit.Feeding.Benthos"] <- "DFB"
names(HalfForecast)[names(HalfForecast) == "X6..Suspension.Feeding.Benthos"] <- "SFB"
names(HalfForecast)[names(HalfForecast) == "X7..Cultured.Shellfish"] <- "Cultured Shellfish"
names(HalfForecast)[names(HalfForecast) == "X8..Carnivorous.Benthos"] <- "Carnivorous Benthos"
names(HalfForecast)[names(HalfForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "Small Squid"
names(HalfForecast)[names(HalfForecast) == "X10..Lg.PIP..12cm"] <- "Large Squid"
names(HalfForecast)[names(HalfForecast) == "X11..Planktivorous.Fish"] <- "Planktivorous Fish"
names(HalfForecast)[names(HalfForecast) == "X12..Benthivorous.Fish"] <- "Benthivorous Fish"
names(HalfForecast)[names(HalfForecast) == "X13..Piscivorous.Fish"] <- "Piscivorous Fish"
names(HalfForecast)[names(HalfForecast) == "X14..Seabirds"] <- "Seabirds"
names(HalfForecast)[names(HalfForecast) == "X15..Detritus"] <- "Detritus"

HalfForecast<-HalfForecast[-542,]

EcosimModel<-HalfForecast %>%
  filter(Year<=2018)

EcosimModel<-gather(EcosimModel, key="FG", value="Biomass", -Year)

ggplot(EcosimModel, aes(x=Year, y=Biomass, col=FG)) + geom_line()

ggplot(ObservedTimeSeries, aes(x=Year, y=Biomass, col=FG)) + geom_point() +
  geom_line(EcosimModel, mapping = aes(x=Year, y=Biomass, col=FG)) +
  scale_color_viridis_d(name = "Functional Group")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)))

##RELATIVE VERSION
RelEcosim<-read.csv("~/Desktop/URI/DATA/Ecosim/V9_Final_RelEcosim.csv")
Observed<-RelEcosim [,-c(9:23)]
names(Observed)[names(Observed) == "ObsCteno"] <- "Ctenophores"
names(Observed)[names(Observed) == "ObsLgPIP"] <- "Large Squid"
names(Observed)[names(Observed) == "ObsSmPIP"] <- "Small Squid"
names(Observed)[names(Observed) == "ObsCB"] <- "Carnivorous Benthos"
names(Observed)[names(Observed) == "ObsPlank"] <- "Planktivorous Fish"
names(Observed)[names(Observed) == "ObsPisc"] <- "Piscivorous Fish"
names(Observed)[names(Observed) == "ObsBenth"] <- "Benthivorous Fish"

Observed<-gather(Observed, key="FG", value="Biomass", -Year)
Observed<-Observed[-2114,]
Observed<-Observed %>%
  filter(Year<=2019)


Predicted<-RelEcosim [, -c(2:8)]  
names(Predicted)[names(Predicted) == "PredPhyto"] <- "Phytoplankton"
names(Predicted)[names(Predicted) == "PredBenthicAlgae"] <- "Benthic Algae"
names(Predicted)[names(Predicted) == "PredZoo"] <- "Zooplankton"
names(Predicted)[names(Predicted) == "PredGelZoo"] <- "Gelatinous Zooplankton"
names(Predicted)[names(Predicted) == "PredDFB"] <- "Deposit Feeding Benthos"
names(Predicted)[names(Predicted) == "PredSFB"] <- "Suspension Feeding Benthos"
names(Predicted)[names(Predicted) == "PredCultShell"] <- "Cultured Shellfish"
names(Predicted)[names(Predicted) == "PredCB"] <- "Carnivorous Benthos"
names(Predicted)[names(Predicted) == "PredSmPIP"] <- "Small Squid"
names(Predicted)[names(Predicted) == "PredLgPIP"] <- "Large Squid"
names(Predicted)[names(Predicted) == "PredPlank"] <- "Planktivorous Fish"
names(Predicted)[names(Predicted) == "PredBenth"] <- "Benthivorous Fish"
names(Predicted)[names(Predicted) == "PredPisc"] <- "Piscivorous Fish"
names(Predicted)[names(Predicted) == "PredBirds"] <- "Seabirds"
names(Predicted)[names(Predicted) == "PredDetritus"] <- "Detritus"
Predicted<-gather(Predicted, key="FG", value="Biomass", -Year)
Predicted<-Predicted[-4530,]
Predicted<-Predicted %>%
  filter(Year<=2019) %>%
  filter(FG!="Cultured Shellfish")

ggplot(Observed, aes(x=Year, y=Biomass, col=FG)) + geom_point() +
  geom_line(Predicted, mapping = aes(x=Year, y=Biomass, col=FG)) +
  scale_color_discrete(name = "Functional Group")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "Relative Biomass")






  
  
  