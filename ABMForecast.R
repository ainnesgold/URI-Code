
library(tidyverse)
library(gridExtra)
library(viridis)
library(ggpubr)
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


ObservedTimeSeries<- ObservedTimeSeries %>%
  select(Year, BenthFish, PiscFish, PlankFish, CB, SmallSquid, LargeSquid)


##trying to add in the forecast made with the ABM
sqABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/SQ0.1.2030.csv", skip=1)

names(sqABMForecast)[names(sqABMForecast) == "Data"] <- "Year"
names(sqABMForecast)[names(sqABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(sqABMForecast)[names(sqABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(sqABMForecast)[names(sqABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(sqABMForecast)[names(sqABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(sqABMForecast)[names(sqABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(sqABMForecast)[names(sqABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(sqABMForecast)[names(sqABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(sqABMForecast)[names(sqABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(sqABMForecast)[names(sqABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(sqABMForecast)[names(sqABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(sqABMForecast)[names(sqABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(sqABMForecast)[names(sqABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(sqABMForecast)[names(sqABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(sqABMForecast)[names(sqABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(sqABMForecast)[names(sqABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(sqABMForecast) <- paste(colnames(sqABMForecast), sep = "_", "abm")
sqABMForecast<-sqABMForecast[-434,]

##now the second ABM forecast of 0.5 enth loss
sq2ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/SQ0.5.2030.csv", skip=1)

names(sq2ABMForecast)[names(sq2ABMForecast) == "Data"] <- "Year"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(sq2ABMForecast)[names(sq2ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(sq2ABMForecast) <- paste(colnames(sq2ABMForecast), sep = "_", "abm2")
sq2ABMForecast<-sq2ABMForecast[-434,]


##add in ABM forecast -1
sq3ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/SQ1.2030.csv", skip=1)

names(sq3ABMForecast)[names(sq3ABMForecast) == "Data"] <- "Year"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(sq3ABMForecast)[names(sq3ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(sq3ABMForecast) <- paste(colnames(sq3ABMForecast), sep = "_", "abm3")
sq3ABMForecast<-sq3ABMForecast[-434,]

##add in ABM with 0 enth loss

sq4ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/SQ0.2030.csv", skip=1)

names(sq4ABMForecast)[names(sq4ABMForecast) == "Data"] <- "Year"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(sq4ABMForecast)[names(sq4ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(sq4ABMForecast) <- paste(colnames(sq4ABMForecast), sep = "_", "abm4")
sq4ABMForecast<-sq4ABMForecast[-434,]




##this is the one for only 10 years
sqForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/statusquo_shortened.csv", skip=1)

names(sqForecast)[names(sqForecast) == "Data"] <- "Year"
names(sqForecast)[names(sqForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(sqForecast)[names(sqForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(sqForecast)[names(sqForecast) == "X3..Zooplankton"] <- "PredZoo"
names(sqForecast)[names(sqForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(sqForecast)[names(sqForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(sqForecast)[names(sqForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(sqForecast)[names(sqForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(sqForecast)[names(sqForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(sqForecast)[names(sqForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(sqForecast)[names(sqForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(sqForecast)[names(sqForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(sqForecast)[names(sqForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(sqForecast)[names(sqForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(sqForecast)[names(sqForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(sqForecast)[names(sqForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(sqForecast) <- paste(colnames(sqForecast), sep = "_", "3")
sqForecast<-sqForecast[-434,]


temp3 <- sqForecast %>%
  select(Year_3, PredPlank_3, PredPisc_3, PredBenth_3, PredCB_3, PredZoo_3,
         PredLgPIP_3, PredSmPIP_3)
colnames(temp3)[1] <- "Year"

tempABM <- sqABMForecast %>%
  select(Year_abm, PredPlank_abm, PredPisc_abm, PredBenth_abm, 
         PredCB_abm, PredZoo_abm,
         PredLgPIP_abm, PredSmPIP_abm)
colnames(tempABM)[1] <- "Year"


temp3<-left_join(temp3, tempABM, by="Year")

tempABM2 <- sq2ABMForecast %>%
  select(Year_abm2, PredPlank_abm2, PredPisc_abm2, PredBenth_abm2, 
         PredCB_abm2, PredZoo_abm2,
         PredLgPIP_abm2, PredSmPIP_abm2)
colnames(tempABM2)[1] <- "Year"

temp3<-left_join(temp3, tempABM2, by="Year")

tempABM3 <- sq3ABMForecast %>%
  select(Year_abm3, PredPlank_abm3, PredPisc_abm3, PredBenth_abm3, 
         PredCB_abm3, PredZoo_abm3,
         PredLgPIP_abm3, PredSmPIP_abm3)
colnames(tempABM3)[1] <- "Year"

temp3<-left_join(temp3, tempABM3, by="Year")

tempABM4 <- sq4ABMForecast %>%
  select(Year_abm4, PredPlank_abm4, PredPisc_abm4, PredBenth_abm4, 
         PredCB_abm4, PredZoo_abm4,
         PredLgPIP_abm4, PredSmPIP_abm4)
colnames(tempABM4)[1] <- "Year"

temp3<-left_join(temp3, tempABM4, by="Year")

##pisc fish plot
PiscABSBio<-temp3 %>%
  select(Year, PredPisc_3, PredPisc_abm, PredPisc_abm2, PredPisc_abm3, PredPisc_abm4)

PiscABSBio<-gather(PiscABSBio, key=Simulation, value=Value, -Year)

PiscABSBio$Simulation <- factor(PiscABSBio$Simulation, 
                                levels=c("PredPisc_abm3", "PredPisc_abm2", "PredPisc_abm",
                                         "PredPisc_abm4", "PredPisc_3"), 
                                labels=c("ABM -1", "ABM -0.5",  "ABM -0.1",  "ABM -0", "EwE"))


ggplot(ObservedTimeSeries, aes(x=Year, y=PiscFish)) + geom_point()+
  geom_line(data=PiscABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Forecast",
                     values=c("springgreen3", "tomato4", "black", "blue", "orange"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Piscivorous Fish")

###ZOOMING IN
Observed2030<-ObservedTimeSeries %>%
  filter(Year > 2019)

Pisc2030<-PiscABSBio %>%
  filter(Year > 2019)
ggplot(Observed2030, aes(x=Year, y=PiscFish)) + geom_point()+
  geom_line(data=Pisc2030, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Forecast",
                     values=c("springgreen3", "tomato4", "black", "blue", "orange"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Piscivorous Fish")


























##plank fish
PlankABSBio<-temp3 %>%
  select(Year, PredPlank_3, PredPlank_abm)

PlankABSBio<-gather(PlankABSBio, key=Simulation, value=Value, -Year)

PlankABSBio$Simulation <- factor(PlankABSBio$Simulation, 
                                 levels=c("PredPlank_3", 
                                          "PredPlank_abm"), 
                                 labels=c("sq Plank Fish / \nConstant 2018 Pisc Fish", 
                                          "sq Plank Fish / \nPisc Fish linked to ABM"))


p2<-ggplot(ObservedTimeSeries, aes(x=Year, y=PlankFish)) + geom_point()+
  geom_line(data=PlankABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Fishing Pressure",
                     values=c("springgreen3", "tomato4"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Planktivorous Fish")


ggarrange(p1, p2, ncol = 1, nrow = 2)

##benth fish
BenthABSBio<-temp3 %>%
  select(Year, PredBenth_3, PredBenth_Zabm)

BenthABSBio<-gather(BenthABSBio, key=Simulation, value=Value, -Year)

BenthABSBio$Simulation <- factor(BenthABSBio$Simulation, 
                                 levels=c("PredBenth_3", 
                                          "PredBenth_Zabm"), 
                                 labels=c("sq Benth Fish / \nConstant 2018 Pisc Fish", 
                                          "sq Benth Fish / \nPisc Fish linked to ABM"))


p3<-ggplot(ObservedTimeSeries, aes(x=Year, y=BenthFish)) + geom_point()+
  geom_line(data=BenthABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Fishing Pressure",
                     values=c("springgreen3", "tomato4"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Benthivorous Fish")



CBABSBio<-temp3 %>%
  select(Year, PredCB_3, PredCB_Zabm)

CBABSBio<-gather(CBABSBio, key=Simulation, value=Value, -Year)

CBABSBio$Simulation <- factor(CBABSBio$Simulation, 
                                 levels=c("PredCB_3", 
                                          "PredCB_Zabm"), 
                                 labels=c("sq CB / \nConstant 2018 Pisc Fish", 
                                          "sq CB / \nPisc Fish linked to ABM"))


p4<-ggplot(ObservedTimeSeries, aes(x=Year, y=CB)) + geom_point()+
  geom_line(data=CBABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Fishing Pressure",
                     values=c("springgreen3", "tomato4"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="CB")


#squid
SmPIPABSBio<-temp3 %>%
  select(Year, PredSmPIP_3, PredSmPIP_Zabm)

SmPIPABSBio<-gather(SmPIPABSBio, key=Simulation, value=Value, -Year)

SmPIPABSBio$Simulation <- factor(SmPIPABSBio$Simulation, 
                                 levels=c("PredSmPIP_3", 
                                          "PredSmPIP_Zabm"), 
                                 labels=c("sq SmPIP Fish / \nConstant 2018 Pisc Fish", 
                                          "sq SmPIP Fish / \nPisc Fish linked to ABM"))


p5<-ggplot(ObservedTimeSeries, aes(x=Year, y=SmallSquid)) + geom_point()+
  geom_line(data=SmPIPABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Fishing Pressure",
                     values=c("springgreen3", "tomato4"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Small Squid")

#lg squid
LgPIPABSBio<-temp3 %>%
  select(Year, PredLgPIP_3, PredLgPIP_Zabm)

LgPIPABSBio<-gather(LgPIPABSBio, key=Simulation, value=Value, -Year)

LgPIPABSBio$Simulation <- factor(LgPIPABSBio$Simulation, 
                                 levels=c("PredLgPIP_3", 
                                          "PredLgPIP_Zabm"), 
                                 labels=c("sq LgPIP / \nConstant 2018 Pisc Fish", 
                                          "sq LgPIP / \nPisc Fish linked to ABM"))


p6<-ggplot(ObservedTimeSeries, aes(x=Year, y=SmallSquid)) + geom_point()+
  geom_line(data=LgPIPABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Fishing Pressure",
                     values=c("springgreen3", "tomato4"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Large Squid")






















