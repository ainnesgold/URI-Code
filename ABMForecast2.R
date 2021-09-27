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


##STATUS QUO
##EwE forecast
sqForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/statusquo/statusquo_shortened.csv", skip=1)

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

##Adding ABM: Power curve / 0.5 enth loss
sqABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/statusquo/SQPower.2030.csv", skip=1)

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

tempABM <- sqABMForecast %>%
  select(Year_abm, PredPlank_abm, PredPisc_abm, PredBenth_abm, 
         PredCB_abm, PredZoo_abm,
         PredLgPIP_abm, PredSmPIP_abm)
colnames(tempABM)[1] <- "Year"


temp3<-left_join(temp3, tempABM, by="Year")

##Adding ABM2: Linear / 0.5 enth loss

sq2ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/statusquo/SQLinear.2030.csv", skip=1)

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

tempABM2 <- sq2ABMForecast %>%
  select(Year_abm2, PredPlank_abm2, PredPisc_abm2, PredBenth_abm2, 
         PredCB_abm2, PredZoo_abm2,
         PredLgPIP_abm2, PredSmPIP_abm2)
colnames(tempABM2)[1] <- "Year"

temp3<-left_join(temp3, tempABM2, by="Year")

##add in static 
sq3ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/statusquo/SQStatic.2030.csv", skip=1)

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

tempABM3 <- sq3ABMForecast %>%
  select(Year_abm3, PredPlank_abm3, PredPisc_abm3, PredBenth_abm3, 
         PredCB_abm3, PredZoo_abm3,
         PredLgPIP_abm3, PredSmPIP_abm3)
colnames(tempABM3)[1] <- "Year"

temp<-left_join(temp3, tempABM3, by="Year")

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

temp6 <- ecosim %>%
  select(Year_6, PredPlank_6, PredPisc_6, PredBenth_6, PredCB_6, PredZoo_6,
         PredLgPIP_6, PredSmPIP_6)
colnames(temp6)[1] <- "Year"
temp<-left_join(temp, temp6, by="Year")



#make plot
PiscABSBio<-temp %>%
  select(Year, PredPisc_3, PredPisc_abm, PredPisc_abm2, PredPisc_abm3, PredPisc_6)

PiscABSBio<-gather(PiscABSBio, key=Simulation, value=Value, -Year)

PiscABSBio$Simulation <- factor(PiscABSBio$Simulation, 
                                levels=c("PredPisc_abm2", "PredPisc_abm3", "PredPisc_abm", 
                                         "PredPisc_3", "PredPisc_6"), 
                                labels=c("ABM Linear", "ABM Static", "ABM Power",  "EwE", "Model"))

#Plotting SQ Different Catch Abundance relationships
a<-ggplot(ObservedTimeSeries, aes(x=Year, y=PiscFish)) + geom_point()+
  geom_line(data=PiscABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Forecast",
                     values=c("#9cd8cf", "#2b7a74", "#013f3a", "black", "black"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "", y = "", 
       title="Status Quo")




#################SQ Different enthusiasm loss#############
#SQ EwE forecast
sqForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/statusquo/statusquo_shortened.csv", skip=1)

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

#0.5 enth loss
sqABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/statusquo/SQPower.2030.csv", skip=1)

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

tempABM <- sqABMForecast %>%
  select(Year_abm, PredPlank_abm, PredPisc_abm, PredBenth_abm, 
         PredCB_abm, PredZoo_abm,
         PredLgPIP_abm, PredSmPIP_abm)
colnames(tempABM)[1] <- "Year"

temp3<-left_join(temp3, tempABM, by="Year")

#0 enth loss
sq2ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/statusquo/SQ0.2030.csv", skip=1)

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

tempABM2 <- sq2ABMForecast %>%
  select(Year_abm2, PredPlank_abm2, PredPisc_abm2, PredBenth_abm2, 
         PredCB_abm2, PredZoo_abm2,
         PredLgPIP_abm2, PredSmPIP_abm2)
colnames(tempABM2)[1] <- "Year"

temp3<-left_join(temp3, tempABM2, by="Year")

##add in enth loss 1 
sq3ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/statusquo/SQ1.2030.csv", skip=1)

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

tempABM3 <- sq3ABMForecast %>%
  select(Year_abm3, PredPlank_abm3, PredPisc_abm3, PredBenth_abm3, 
         PredCB_abm3, PredZoo_abm3,
         PredLgPIP_abm3, PredSmPIP_abm3)
colnames(tempABM3)[1] <- "Year"

temp3<-left_join(temp3, tempABM3, by="Year")
temp3<-left_join(temp3, temp6)


#make plot
PiscABSBio<-temp3 %>%
  select(Year, PredPisc_3, PredPisc_abm, PredPisc_abm2, PredPisc_abm3, PredPisc_6)

PiscABSBio<-gather(PiscABSBio, key=Simulation, value=Value, -Year)

PiscABSBio$Simulation <- factor(PiscABSBio$Simulation, 
                                levels=c("PredPisc_abm3","PredPisc_abm", "PredPisc_abm2", 
                                         "PredPisc_3", "PredPisc_6"), 
                                labels=c("ABM 1", "ABM 0.5", "ABM 0", "EwE", "Model"))
#ObservedTimeSeries<-ObservedTimeSeries %>%
 # filter(Year>=2015)
#PiscABSBio<-PiscABSBio %>%
 # filter(Year>=2015)

#Plotting SQ enthusiasm loss
b<-ggplot(ObservedTimeSeries, aes(x=Year, y=PiscFish)) + geom_point()+
  geom_line(data=PiscABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Forecast",
                     values=c("#452805","#BF812D", "#DFC27D", "black", "black"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "", y = "", title="Status Quo")












##ZERO SCENARIO
#Zero EwE Forecast
ZeroForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/zero/zero_output_shortened.csv", skip=1)

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
ZeroForecast<-ZeroForecast[-434,]


temp3 <- ZeroForecast %>%
  select(Year_3, PredPlank_3, PredPisc_3, PredBenth_3, PredCB_3, PredZoo_3,
         PredLgPIP_3, PredSmPIP_3)
colnames(temp3)[1] <- "Year"


##Adding ABM: Power curve / 0.5 enth loss
ZeroABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/zero/ZeroPower.2030.csv", skip=1)

names(ZeroABMForecast)[names(ZeroABMForecast) == "Data"] <- "Year"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(ZeroABMForecast) <- paste(colnames(ZeroABMForecast), sep = "_", "abm")
ZeroABMForecast<-ZeroABMForecast[-434,]

tempABM <- ZeroABMForecast %>%
  select(Year_abm, PredPlank_abm, PredPisc_abm, PredBenth_abm, 
         PredCB_abm, PredZoo_abm,
         PredLgPIP_abm, PredSmPIP_abm)
colnames(tempABM)[1] <- "Year"


temp3<-left_join(temp3, tempABM, by="Year")

##Adding ABM2: Linear / 0.5 enth loss

Zero2ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/zero/ZeroLinear.2030.csv", skip=1)

names(Zero2ABMForecast)[names(Zero2ABMForecast) == "Data"] <- "Year"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(Zero2ABMForecast) <- paste(colnames(Zero2ABMForecast), sep = "_", "abm2")
Zero2ABMForecast<-Zero2ABMForecast[-434,]

tempABM2 <- Zero2ABMForecast %>%
  select(Year_abm2, PredPlank_abm2, PredPisc_abm2, PredBenth_abm2, 
         PredCB_abm2, PredZoo_abm2,
         PredLgPIP_abm2, PredSmPIP_abm2)
colnames(tempABM2)[1] <- "Year"

temp3<-left_join(temp3, tempABM2, by="Year")

##add in static 
Zero3ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/zero/ZeroStatic.2030.csv", skip=1)

names(Zero3ABMForecast)[names(Zero3ABMForecast) == "Data"] <- "Year"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(Zero3ABMForecast) <- paste(colnames(Zero3ABMForecast), sep = "_", "abm3")
Zero3ABMForecast<-Zero3ABMForecast[-434,]

tempABM3 <- Zero3ABMForecast %>%
  select(Year_abm3, PredPlank_abm3, PredPisc_abm3, PredBenth_abm3, 
         PredCB_abm3, PredZoo_abm3,
         PredLgPIP_abm3, PredSmPIP_abm3)
colnames(tempABM3)[1] <- "Year"

temp3<-left_join(temp3, tempABM3, by="Year")
temp3<-left_join(temp3, temp6)

#make plot
PiscABSBio<-temp3 %>%
  select(Year, PredPisc_3, PredPisc_abm, PredPisc_abm2, PredPisc_abm3, PredPisc_6)

PiscABSBio<-gather(PiscABSBio, key=Simulation, value=Value, -Year)

PiscABSBio$Simulation <- factor(PiscABSBio$Simulation,
                                levels=c("PredPisc_abm2", "PredPisc_abm3", "PredPisc_abm", 
                                         "PredPisc_3", "PredPisc_6"), 
                                labels=c("ABM Linear", "ABM Static", "ABM Power",  "EwE", "Model"))

#Plotting zero Different Catch Abundance relationships
c<-ggplot(ObservedTimeSeries, aes(x=Year, y=PiscFish)) + geom_point()+
  geom_line(data=PiscABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Forecast",
                     values=c("#9cd8cf", "#2b7a74", "#013f3a", "black", "black"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "", y = bquote('Biomass'~(g/m^2)), title="Zero")


##zero enthusiasm level trials
#Zero EwE Forecast
ZeroForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/zero/zero_output_shortened.csv", skip=1)

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
ZeroForecast<-ZeroForecast[-434,]


temp3 <- ZeroForecast %>%
  select(Year_3, PredPlank_3, PredPisc_3, PredBenth_3, PredCB_3, PredZoo_3,
         PredLgPIP_3, PredSmPIP_3)
colnames(temp3)[1] <- "Year"

##Zero 0.5 enth loss
##Adding ABM: Power curve / 0.5 enth loss
ZeroABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/zero/ZeroPower.2030.csv", skip=1)

names(ZeroABMForecast)[names(ZeroABMForecast) == "Data"] <- "Year"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(ZeroABMForecast)[names(ZeroABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(ZeroABMForecast) <- paste(colnames(ZeroABMForecast), sep = "_", "abm")
ZeroABMForecast<-ZeroABMForecast[-434,]

tempABM <- ZeroABMForecast %>%
  select(Year_abm, PredPlank_abm, PredPisc_abm, PredBenth_abm, 
         PredCB_abm, PredZoo_abm,
         PredLgPIP_abm, PredSmPIP_abm)
colnames(tempABM)[1] <- "Year"


temp3<-left_join(temp3, tempABM, by="Year")


##Zero 0 enth loss

Zero2ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/zero/Zero0.2030.csv", skip=1)

names(Zero2ABMForecast)[names(Zero2ABMForecast) == "Data"] <- "Year"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(Zero2ABMForecast)[names(Zero2ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(Zero2ABMForecast) <- paste(colnames(Zero2ABMForecast), sep = "_", "abm2")
Zero2ABMForecast<-Zero2ABMForecast[-434,]

tempABM2 <- Zero2ABMForecast %>%
  select(Year_abm2, PredPlank_abm2, PredPisc_abm2, PredBenth_abm2, 
         PredCB_abm2, PredZoo_abm2,
         PredLgPIP_abm2, PredSmPIP_abm2)
colnames(tempABM2)[1] <- "Year"

temp3<-left_join(temp3, tempABM2, by="Year")

##add in static 
Zero3ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/zero/Zero1.2030.csv", skip=1)

names(Zero3ABMForecast)[names(Zero3ABMForecast) == "Data"] <- "Year"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(Zero3ABMForecast)[names(Zero3ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(Zero3ABMForecast) <- paste(colnames(Zero3ABMForecast), sep = "_", "abm3")
Zero3ABMForecast<-Zero3ABMForecast[-434,]

tempABM3 <- Zero3ABMForecast %>%
  select(Year_abm3, PredPlank_abm3, PredPisc_abm3, PredBenth_abm3, 
         PredCB_abm3, PredZoo_abm3,
         PredLgPIP_abm3, PredSmPIP_abm3)
colnames(tempABM3)[1] <- "Year"

temp3<-left_join(temp3, tempABM3, by="Year")
temp3<-left_join(temp3, temp6)

#make plot
PiscABSBio<-temp3 %>%
  select(Year, PredPisc_3, PredPisc_abm, PredPisc_abm2, PredPisc_abm3, PredPisc_6)

PiscABSBio<-gather(PiscABSBio, key=Simulation, value=Value, -Year)

PiscABSBio$Simulation <- factor(PiscABSBio$Simulation, 
                                levels=c("PredPisc_abm3","PredPisc_abm", "PredPisc_abm2", 
                                         "PredPisc_3", "PredPisc_6"), 
                                labels=c("ABM 1", "ABM 0.5", "ABM 0", "EwE", "Model"))

#Plotting zero Different enth loss
d<-ggplot(ObservedTimeSeries, aes(x=Year, y=PiscFish)) + geom_point()+
  geom_line(data=PiscABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Forecast",
                     values=c("#452805","#BF812D", "#DFC27D", "black", "black"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "", y = bquote('Biomass'~(g/m^2)), title="Zero")




###########INTERMEDIATE SCENARIO
##EwE forecast
IntForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/intermediate/intermediateoutput_shortened.csv", skip=1)

names(IntForecast)[names(IntForecast) == "Data"] <- "Year"
names(IntForecast)[names(IntForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(IntForecast)[names(IntForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(IntForecast)[names(IntForecast) == "X3..Zooplankton"] <- "PredZoo"
names(IntForecast)[names(IntForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(IntForecast)[names(IntForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(IntForecast)[names(IntForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(IntForecast)[names(IntForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(IntForecast)[names(IntForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(IntForecast)[names(IntForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(IntForecast)[names(IntForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(IntForecast)[names(IntForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(IntForecast)[names(IntForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(IntForecast)[names(IntForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(IntForecast)[names(IntForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(IntForecast)[names(IntForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(IntForecast) <- paste(colnames(IntForecast), sep = "_", "3")
IntForecast<-IntForecast[-434,]


temp3 <- IntForecast %>%
  select(Year_3, PredPlank_3, PredPisc_3, PredBenth_3, PredCB_3, PredZoo_3,
         PredLgPIP_3, PredSmPIP_3)
colnames(temp3)[1] <- "Year"

##Adding ABM: Power curve
IntABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/Intermediate/IntPower.2030.csv", skip=1)

names(IntABMForecast)[names(IntABMForecast) == "Data"] <- "Year"
names(IntABMForecast)[names(IntABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(IntABMForecast)[names(IntABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(IntABMForecast)[names(IntABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(IntABMForecast)[names(IntABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(IntABMForecast)[names(IntABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(IntABMForecast)[names(IntABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(IntABMForecast)[names(IntABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(IntABMForecast)[names(IntABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(IntABMForecast)[names(IntABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(IntABMForecast)[names(IntABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(IntABMForecast)[names(IntABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(IntABMForecast)[names(IntABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(IntABMForecast)[names(IntABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(IntABMForecast)[names(IntABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(IntABMForecast)[names(IntABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(IntABMForecast) <- paste(colnames(IntABMForecast), sep = "_", "abm")
IntABMForecast<-IntABMForecast[-434,]

tempABM <- IntABMForecast %>%
  select(Year_abm, PredPlank_abm, PredPisc_abm, PredBenth_abm, 
         PredCB_abm, PredZoo_abm,
         PredLgPIP_abm, PredSmPIP_abm)
colnames(tempABM)[1] <- "Year"


temp3<-left_join(temp3, tempABM, by="Year")

##Int: Linear

Int2ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/intermediate/IntLinear.2030.csv", skip=1)

names(Int2ABMForecast)[names(Int2ABMForecast) == "Data"] <- "Year"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(Int2ABMForecast) <- paste(colnames(Int2ABMForecast), sep = "_", "abm2")
Int2ABMForecast<-Int2ABMForecast[-434,]

tempABM2 <- Int2ABMForecast %>%
  select(Year_abm2, PredPlank_abm2, PredPisc_abm2, PredBenth_abm2, 
         PredCB_abm2, PredZoo_abm2,
         PredLgPIP_abm2, PredSmPIP_abm2)
colnames(tempABM2)[1] <- "Year"

temp3<-left_join(temp3, tempABM2, by="Year")

##Int: add in static 
Int3ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/intermediate/IntStatic.2030.csv", skip=1)

names(Int3ABMForecast)[names(Int3ABMForecast) == "Data"] <- "Year"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(Int3ABMForecast) <- paste(colnames(Int3ABMForecast), sep = "_", "abm3")
Int3ABMForecast<-Int3ABMForecast[-434,]

tempABM3 <- Int3ABMForecast %>%
  select(Year_abm3, PredPlank_abm3, PredPisc_abm3, PredBenth_abm3, 
         PredCB_abm3, PredZoo_abm3,
         PredLgPIP_abm3, PredSmPIP_abm3)
colnames(tempABM3)[1] <- "Year"

temp3<-left_join(temp3, tempABM3, by="Year")
temp3<-left_join(temp3, temp6)

PiscABSBio<-temp3 %>%
  select(Year, PredPisc_3, PredPisc_abm, PredPisc_abm2, PredPisc_abm3, PredPisc_6)

PiscABSBio<-gather(PiscABSBio, key=Simulation, value=Value, -Year)

PiscABSBio$Simulation <- factor(PiscABSBio$Simulation, 
                                levels=c("PredPisc_abm2", "PredPisc_abm3", "PredPisc_abm", 
                                         "PredPisc_3", "PredPisc_6"), 
                                labels=c("ABM Linear", "ABM Static", "ABM Power",  "EwE", "Model"))

#Plotting zero Different Catch Abundance relationships
e<-ggplot(ObservedTimeSeries, aes(x=Year, y=PiscFish)) + geom_point()+
  geom_line(data=PiscABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Forecast",
                     values=c("#9cd8cf", "#2b7a74", "#013f3a", "black", "black"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Intermediate")

##Intermediate enthusiasm scenarios
##EwE forecast
IntForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/intermediate/intermediateoutput_shortened.csv", skip=1)

names(IntForecast)[names(IntForecast) == "Data"] <- "Year"
names(IntForecast)[names(IntForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(IntForecast)[names(IntForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(IntForecast)[names(IntForecast) == "X3..Zooplankton"] <- "PredZoo"
names(IntForecast)[names(IntForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(IntForecast)[names(IntForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(IntForecast)[names(IntForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(IntForecast)[names(IntForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(IntForecast)[names(IntForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(IntForecast)[names(IntForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(IntForecast)[names(IntForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(IntForecast)[names(IntForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(IntForecast)[names(IntForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(IntForecast)[names(IntForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(IntForecast)[names(IntForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(IntForecast)[names(IntForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(IntForecast) <- paste(colnames(IntForecast), sep = "_", "3")
IntForecast<-IntForecast[-434,]


temp3 <- IntForecast %>%
  select(Year_3, PredPlank_3, PredPisc_3, PredBenth_3, PredCB_3, PredZoo_3,
         PredLgPIP_3, PredSmPIP_3)
colnames(temp3)[1] <- "Year"

##Adding ABM: 0.5 enth loss
IntABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/Intermediate/IntPower.2030.csv", skip=1)

names(IntABMForecast)[names(IntABMForecast) == "Data"] <- "Year"
names(IntABMForecast)[names(IntABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(IntABMForecast)[names(IntABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(IntABMForecast)[names(IntABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(IntABMForecast)[names(IntABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(IntABMForecast)[names(IntABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(IntABMForecast)[names(IntABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(IntABMForecast)[names(IntABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(IntABMForecast)[names(IntABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(IntABMForecast)[names(IntABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(IntABMForecast)[names(IntABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(IntABMForecast)[names(IntABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(IntABMForecast)[names(IntABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(IntABMForecast)[names(IntABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(IntABMForecast)[names(IntABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(IntABMForecast)[names(IntABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(IntABMForecast) <- paste(colnames(IntABMForecast), sep = "_", "abm")
IntABMForecast<-IntABMForecast[-434,]

tempABM <- IntABMForecast %>%
  select(Year_abm, PredPlank_abm, PredPisc_abm, PredBenth_abm, 
         PredCB_abm, PredZoo_abm,
         PredLgPIP_abm, PredSmPIP_abm)
colnames(tempABM)[1] <- "Year"

temp3<-left_join(temp3, tempABM, by="Year")

##enth loss 0
Int2ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/Intermediate/Int0.2030.csv", skip=1)

names(Int2ABMForecast)[names(Int2ABMForecast) == "Data"] <- "Year"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(Int2ABMForecast)[names(Int2ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(Int2ABMForecast) <- paste(colnames(Int2ABMForecast), sep = "_", "abm2")
Int2ABMForecast<-Int2ABMForecast[-434,]

tempABM2 <- Int2ABMForecast %>%
  select(Year_abm2, PredPlank_abm2, PredPisc_abm2, PredBenth_abm2, 
         PredCB_abm2, PredZoo_abm2,
         PredLgPIP_abm2, PredSmPIP_abm2)
colnames(tempABM2)[1] <- "Year"

temp3<-left_join(temp3, tempABM2, by="Year")
temp3<-left_join(temp3, temp6)

###Adding ABM -1
Int3ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/Intermediate/Int1.2030.csv", skip=1)

names(Int3ABMForecast)[names(Int3ABMForecast) == "Data"] <- "Year"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(Int3ABMForecast)[names(Int3ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(Int3ABMForecast) <- paste(colnames(Int3ABMForecast), sep = "_", "abm3")
Int3ABMForecast<-Int3ABMForecast[-434,]

tempABM3 <- Int3ABMForecast %>%
  select(Year_abm3, PredPlank_abm3, PredPisc_abm3, PredBenth_abm3, 
         PredCB_abm3, PredZoo_abm3,
         PredLgPIP_abm3, PredSmPIP_abm3)
colnames(tempABM3)[1] <- "Year"

temp3<-left_join(temp3, tempABM3, by="Year")


#make plot
PiscABSBio<-temp3 %>%
  select(Year, PredPisc_3, PredPisc_abm, PredPisc_abm2, PredPisc_abm3, PredPisc_6)

PiscABSBio<-gather(PiscABSBio, key=Simulation, value=Value, -Year)

PiscABSBio$Simulation <- factor(PiscABSBio$Simulation, 
                                levels=c("PredPisc_abm3","PredPisc_abm", "PredPisc_abm2", 
                                         "PredPisc_3", "PredPisc_6"), 
                                labels=c("ABM 1", "ABM 0.5", "ABM 0", "EwE", "Model"))

#Plotting zero Different enth loss
f<-ggplot(ObservedTimeSeries, aes(x=Year, y=PiscFish)) + geom_point()+
  geom_line(data=PiscABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Forecast",
                     values=c("#452805","#BF812D", "#DFC27D", "black", "black"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = bquote('Biomass'~(g/m^2)), title="Intermediate")


###EXTREME SCENARIO##############################
###Catch abundance
##EwE forecast
ExtForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/extreme/2000_output_shortened.csv", skip=1)

names(ExtForecast)[names(ExtForecast) == "Data"] <- "Year"
names(ExtForecast)[names(ExtForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(ExtForecast)[names(ExtForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(ExtForecast)[names(ExtForecast) == "X3..Zooplankton"] <- "PredZoo"
names(ExtForecast)[names(ExtForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(ExtForecast)[names(ExtForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(ExtForecast)[names(ExtForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(ExtForecast)[names(ExtForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(ExtForecast)[names(ExtForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(ExtForecast)[names(ExtForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(ExtForecast)[names(ExtForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(ExtForecast)[names(ExtForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(ExtForecast)[names(ExtForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(ExtForecast)[names(ExtForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(ExtForecast)[names(ExtForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(ExtForecast)[names(ExtForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(ExtForecast) <- paste(colnames(ExtForecast), sep = "_", "3")
ExtForecast<-ExtForecast[-434,]


temp3 <- ExtForecast %>%
  select(Year_3, PredPlank_3, PredPisc_3, PredBenth_3, PredCB_3, PredZoo_3,
         PredLgPIP_3, PredSmPIP_3)
colnames(temp3)[1] <- "Year"

##Extreme ABM: Power curve
ExtABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/extreme/ExtremePower.2030.csv", skip=1)

names(ExtABMForecast)[names(ExtABMForecast) == "Data"] <- "Year"
names(ExtABMForecast)[names(ExtABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(ExtABMForecast)[names(ExtABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(ExtABMForecast)[names(ExtABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(ExtABMForecast)[names(ExtABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(ExtABMForecast)[names(ExtABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(ExtABMForecast)[names(ExtABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(ExtABMForecast)[names(ExtABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(ExtABMForecast)[names(ExtABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(ExtABMForecast)[names(ExtABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(ExtABMForecast)[names(ExtABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(ExtABMForecast)[names(ExtABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(ExtABMForecast)[names(ExtABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(ExtABMForecast)[names(ExtABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(ExtABMForecast)[names(ExtABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(ExtABMForecast)[names(ExtABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(ExtABMForecast) <- paste(colnames(ExtABMForecast), sep = "_", "abm")
ExtABMForecast<-ExtABMForecast[-434,]

tempABM <- ExtABMForecast %>%
  select(Year_abm, PredPlank_abm, PredPisc_abm, PredBenth_abm, 
         PredCB_abm, PredZoo_abm,
         PredLgPIP_abm, PredSmPIP_abm)
colnames(tempABM)[1] <- "Year"


temp3<-left_join(temp3, tempABM, by="Year")

##Ext: Linear

Ext2ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/extreme/ExtremeLinear.2030.csv", skip=1)

names(Ext2ABMForecast)[names(Ext2ABMForecast) == "Data"] <- "Year"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(Ext2ABMForecast) <- paste(colnames(Ext2ABMForecast), sep = "_", "abm2")
Ext2ABMForecast<-Ext2ABMForecast[-434,]

tempABM2 <- Ext2ABMForecast %>%
  select(Year_abm2, PredPlank_abm2, PredPisc_abm2, PredBenth_abm2, 
         PredCB_abm2, PredZoo_abm2,
         PredLgPIP_abm2, PredSmPIP_abm2)
colnames(tempABM2)[1] <- "Year"

temp3<-left_join(temp3, tempABM2, by="Year")

##Extreme: add in static 
Ext3ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/extreme/ExtremeStatic.2030.csv", skip=1)

names(Ext3ABMForecast)[names(Ext3ABMForecast) == "Data"] <- "Year"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(Ext3ABMForecast) <- paste(colnames(Ext3ABMForecast), sep = "_", "abm3")
Ext3ABMForecast<-Ext3ABMForecast[-434,]

tempABM3 <- Ext3ABMForecast %>%
  select(Year_abm3, PredPlank_abm3, PredPisc_abm3, PredBenth_abm3, 
         PredCB_abm3, PredZoo_abm3,
         PredLgPIP_abm3, PredSmPIP_abm3)
colnames(tempABM3)[1] <- "Year"

temp3<-left_join(temp3, tempABM3, by="Year")
temp3<-left_join(temp3, temp6)

PiscABSBio<-temp3 %>%
  select(Year, PredPisc_3, PredPisc_abm, PredPisc_abm2, PredPisc_abm3, PredPisc_6)

PiscABSBio<-gather(PiscABSBio, key=Simulation, value=Value, -Year)

PiscABSBio$Simulation <- factor(PiscABSBio$Simulation, 
                                levels=c("PredPisc_abm2", "PredPisc_abm3", "PredPisc_abm", 
                                         "PredPisc_3", "PredPisc_6"), 
                                labels=c("ABM Linear", "ABM Static", "ABM Power",  "EwE", "Model"))

#Plotting zero Different Catch Abundance relationships
g<-ggplot(ObservedTimeSeries, aes(x=Year, y=PiscFish)) + geom_point()+
  geom_line(data=PiscABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Forecast",
                     values=c("#9cd8cf", "#2b7a74", "#013f3a", "black", "black"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "", title="Extreme")


##Extreme enthusiasm scenarios
##EwE forecast
ExtForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/extreme/2000_output_shortened.csv", skip=1)

names(ExtForecast)[names(ExtForecast) == "Data"] <- "Year"
names(ExtForecast)[names(ExtForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(ExtForecast)[names(ExtForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(ExtForecast)[names(ExtForecast) == "X3..Zooplankton"] <- "PredZoo"
names(ExtForecast)[names(ExtForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(ExtForecast)[names(ExtForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(ExtForecast)[names(ExtForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(ExtForecast)[names(ExtForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(ExtForecast)[names(ExtForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(ExtForecast)[names(ExtForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(ExtForecast)[names(ExtForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(ExtForecast)[names(ExtForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(ExtForecast)[names(ExtForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(ExtForecast)[names(ExtForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(ExtForecast)[names(ExtForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(ExtForecast)[names(ExtForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(ExtForecast) <- paste(colnames(ExtForecast), sep = "_", "3")
ExtForecast<-ExtForecast[-434,]


temp3 <- ExtForecast %>%
  select(Year_3, PredPlank_3, PredPisc_3, PredBenth_3, PredCB_3, PredZoo_3,
         PredLgPIP_3, PredSmPIP_3)
colnames(temp3)[1] <- "Year"

## 0.5 scenario
ExtABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/extreme/ExtremePower.2030.csv", skip=1)

names(ExtABMForecast)[names(ExtABMForecast) == "Data"] <- "Year"
names(ExtABMForecast)[names(ExtABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(ExtABMForecast)[names(ExtABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(ExtABMForecast)[names(ExtABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(ExtABMForecast)[names(ExtABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(ExtABMForecast)[names(ExtABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(ExtABMForecast)[names(ExtABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(ExtABMForecast)[names(ExtABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(ExtABMForecast)[names(ExtABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(ExtABMForecast)[names(ExtABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(ExtABMForecast)[names(ExtABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(ExtABMForecast)[names(ExtABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(ExtABMForecast)[names(ExtABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(ExtABMForecast)[names(ExtABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(ExtABMForecast)[names(ExtABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(ExtABMForecast)[names(ExtABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(ExtABMForecast) <- paste(colnames(ExtABMForecast), sep = "_", "abm")
ExtABMForecast<-ExtABMForecast[-434,]

tempABM <- ExtABMForecast %>%
  select(Year_abm, PredPlank_abm, PredPisc_abm, PredBenth_abm, 
         PredCB_abm, PredZoo_abm,
         PredLgPIP_abm, PredSmPIP_abm)
colnames(tempABM)[1] <- "Year"


temp3<-left_join(temp3, tempABM, by="Year")


###0 enth loss scenario
Ext2ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/extreme/Extreme0.2030.csv", skip=1)

names(Ext2ABMForecast)[names(Ext2ABMForecast) == "Data"] <- "Year"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(Ext2ABMForecast)[names(Ext2ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(Ext2ABMForecast) <- paste(colnames(Ext2ABMForecast), sep = "_", "abm2")
Ext2ABMForecast<-Ext2ABMForecast[-434,]

tempABM2 <- Ext2ABMForecast %>%
  select(Year_abm2, PredPlank_abm2, PredPisc_abm2, PredBenth_abm2, 
         PredCB_abm2, PredZoo_abm2,
         PredLgPIP_abm2, PredSmPIP_abm2)
colnames(tempABM2)[1] <- "Year"

temp3<-left_join(temp3, tempABM2, by="Year")

##Extreme: add in static 
Ext3ABMForecast<-read.csv("~/Google Drive/URI/DATA/Ecosim/forecast/EwE ABM Simulations/extreme/Extreme1.2030.csv", skip=1)

names(Ext3ABMForecast)[names(Ext3ABMForecast) == "Data"] <- "Year"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X1..Phytoplankton"] <- "PredPhyto"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X2..Benthic.Algae"] <- "PredBenthAlgae"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X3..Zooplankton"] <- "PredZoo"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X4..Gelatinous.Zooplankton"] <- "PredGelZoo"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X5..Deposit.Feeding.Benthos"] <- "PredDFB"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X6..Suspension.Feeding.Benthos"] <- "PredSFB"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X7..Cultured.Shellfish"] <- "PredCultShell"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X8..Carnivorous.Benthos"] <- "PredCB"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X9..Sm.Pelagic.Invert.Predators"] <- "PredSmPIP"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X10..Lg.PIP..12cm"] <- "PredLgPIP"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X11..Planktivorous.Fish"] <- "PredPlank"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X12..Benthivorous.Fish"] <- "PredBenth"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X13..Piscivorous.Fish"] <- "PredPisc"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X14..Seabirds"] <- "PredSeabirds"
names(Ext3ABMForecast)[names(Ext3ABMForecast) == "X15..Detritus"] <- "PredDetritus"

colnames(Ext3ABMForecast) <- paste(colnames(Ext3ABMForecast), sep = "_", "abm3")
Ext3ABMForecast<-Ext3ABMForecast[-434,]

tempABM3 <- Ext3ABMForecast %>%
  select(Year_abm3, PredPlank_abm3, PredPisc_abm3, PredBenth_abm3, 
         PredCB_abm3, PredZoo_abm3,
         PredLgPIP_abm3, PredSmPIP_abm3)
colnames(tempABM3)[1] <- "Year"

temp3<-left_join(temp3, tempABM3, by="Year")
temp3<-left_join(temp3, temp6)

#make plot
PiscABSBio<-temp3 %>%
  select(Year, PredPisc_3, PredPisc_abm, PredPisc_abm2, PredPisc_abm3, PredPisc_6)

PiscABSBio<-gather(PiscABSBio, key=Simulation, value=Value, -Year)

PiscABSBio$Simulation <- factor(PiscABSBio$Simulation, 
                                levels=c("PredPisc_abm3","PredPisc_abm", "PredPisc_abm2", 
                                         "PredPisc_3", "PredPisc_6"), 
                                labels=c("ABM 1", "ABM 0.5", "ABM 0", "EwE", "Model"))

#Plotting zero Different enth loss
h<-ggplot(ObservedTimeSeries, aes(x=Year, y=PiscFish)) + geom_point()+
  geom_line(data=PiscABSBio, aes(x=Year, y=Value, color=Simulation))+
  scale_color_manual(name = "Forecast",
                     values=c("#452805","#BF812D", "#DFC27D", "black", "black"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Year", y = "", title="Extreme")



ggarrange(c, a, e, g, nrow=2, ncol=2, common.legend = TRUE, legend = "bottom")
ggarrange( d, b, f, h, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")
