#Maggie's attempt to look at species patterns by functional group

#read in biomass and catch species time series
#setwd("C:/Users/Maggie/Google Drive/EPSCoR C-AIM/2.3_HighTrophicLevel/Balancing-Model Notes")
rec=read.csv("~/Google Drive/URI/DATA/SpeciesFGanalysis/Fisheries Time Series - RecSpeciesCatch.csv")
comm=read.csv("~/Google Drive/URI/DATA/SpeciesFGanalysis/Fisheries Time Series - CommercialSpeciesCatch.csv")
comm_scaled = read.csv("~/Google Drive/URI/DATA/SpeciesFGanalysis/Fisheries Time Series - AllSpecies.csv")
bio=read.csv("~/Google Drive/URI/DATA/SpeciesFGanalysis/Biomass Time Series - TotalBiomass.csv")
fgts=read.csv("~/Google Drive/URI/DATA/SpeciesFGanalysis/Biomass Time Series - V9-NEW_FinalTimeSeries_F.csv") #functional group time series --THIS NEEDS TO BE UPDATED
fgts=fgts[fgts$Name!='Pool code',]
fgts=fgts[fgts$Name!='Type',]
fgts$Year = as.numeric(c(1994:2018))
bio=subset(bio, Year>1993)
rec=subset(rec, Year>1993)
comm=subset(comm, Year>1993)

############Code from Maggie
unique(bio$FunctionalGroup)
#SmallPIP    LargePIP    CarnBenthos PiscFish    BenthFish   PlankFish

library(ggplot2)
#Biomass
#old brute force way that I don't want to delete to have a reference but the ggplots below are way better
#plot(x=fgts$Year, y=fgts$PiscFish, type='b', main='PiscFish')
#lines(x=fgts$Year,y=bio$BiomassGM2[bio$Species=='Bluefish'], type='l')
#lines(x=fgts$Year,y=bio$BiomassGM2[bio$Species=='Striped Bass'], type='l')
#lines(x=fgts$Year,y=bio$BiomassGM2[bio$Species=='Weakfish'], type='l')
#lines(x=fgts$Year,y=bio$BiomassGM2[bio$Species=='Spiny Dogfish'], type='l')
#lines(x=fgts$Year,y=bio$BiomassGM2[bio$Species=='Summer Flounder'], type='l')
#text(x=2010, y=6,"add colors and legend")

ggplot(bio[bio$FunctionalGroup=='PiscFish',], aes(x=Year, y=BiomassGM2, color=Species)) +
geom_line() +
geom_line(data=fgts, aes(x=fgts$Year, y=fgts$PiscFish, col='FG'), size=1.7) +
  ggtitle("Piscivorous Biomass")

ggplot(bio[bio$FunctionalGroup=='BenthFish',], aes(x=Year, y=BiomassGM2, color=Species)) +
  geom_line() +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$BenthFish, col='FG'), size=1.7) +
  ggtitle("Benthivorous Biomass")

ggplot(bio[bio$FunctionalGroup=='PlankFish',], aes(x=Year, y=BiomassGM2, color=Species)) +
  geom_line() +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$PlankFish, col='FG'), size=1.7) +
  ggtitle("Planktivorous Biomass")

ggplot(bio[bio$FunctionalGroup=='CarnBenthos',], aes(x=Year, y=BiomassGM2, color=Species)) +
  geom_line() +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$CB, col='FG'), size=1.7) +
  ggtitle("CarnBenthos Biomass")


#recreational cathces
unique(rec$Group) #Benthivorous Fish Piscivorous Fish 
unique(rec$CommonName)

ggplot(rec[rec$Group=='Piscivorous Fish',], aes(x=Year, y=G.M2, color=CommonName)) +
  geom_line() +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$Pisc_F, col='FG_F'), size=1.5) +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$PiscFish/4, col='FG_Bio/4'), size=1.3) +
  ggtitle("Piscivorous Rec Catch with references")
#so note for this one is the biomoass is driving the F pattern more than catch is. 
#catch is mostly describing the varation around the main pattern which is biomass driven. 

ggplot(rec[rec$Group=='Benthivorous Fish',], aes(x=Year, y=G.M2, color=CommonName)) +
  geom_line() +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$Benth_F, col='FG_F'), size=1.5) +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$BenthFish/10, col='FG_Bio/10'), size=1.3) +
  ggtitle("Benthivorous Rec Catch with references")
#this one seems more stable because the times of high biomass generally correspond with high catch
#except at the end when there is higher catch and lower biomass so we get a spike in F



#commercial catches
unique(comm$Group)
#Benthivorous Fish          Carnivorous Benthos        Large_PIP                  Piscivorous Fish          
#Planktivorous Fish         Small_PIP                  Suspension                 Suspension Feeding Benthos
#Not sure if there is a difference between Suspension and Suspension Feeding Benthos. I left for now, but could clean

unique(comm$Species)

#for all of these commercial catch is in LBS so I just kept with that assuming that relative trends are 
#really what we're looking at

ggplot(comm[comm$Group=='Piscivorous Fish',], aes(x=Year, y=CatchLBS, color=Species)) +
  geom_line() +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$Pisc_F*10^6, col='FG_F*10^6'), size=1.5) +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$PiscFish, col='FG_Bio'), size=1.3) +
  ggtitle("Piscivorous Comm Catch with references")

ggplot(comm[comm$Group=='Benthivorous Fish',], aes(x=Year, y=CatchLBS, color=Species)) +
  geom_line() +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$Benth_F*10^7, col='FG_F*10^7'), size=1.5) +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$BenthFish, col='FG_Bio'), size=1.3) +
  ggtitle("Benthivorous Comm Catch with references")

ggplot(comm[comm$Group=='Planktivorous Fish',], aes(x=Year, y=CatchLBS, color=Species)) +
  geom_line() +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$Plank_F*10^7, col='FG_F*10^7'), size=1.5) +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$PlankFish*10^5, col='FG_Bio*10^5'), size=1.3) +
  ggtitle("Planktivorous Comm Catch with references")
#looks like F follows herring a lot with some influence on biomass. 

ggplot(comm[comm$Group=='Carnivorous Benthos',], aes(x=Year, y=CatchLBS, color=Species)) +
  geom_line() +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$CB_F*10^7, col='FG_F*10^7'), size=1.5) +
  geom_line(data=fgts, aes(x=fgts$Year, y=fgts$CB, col='FG_Bio'), size=1.3) +
  ggtitle("CarnBenthos Comm Catch with references")


#to do one for PIPs do I add them together or keep them as separate groups

#also not sure on suspension v. suspension feeding benthos. 















######ANNIE CODE

 #1. carnivorous benthos species biomass
library(tidyverse)

CBbio<-bio %>%
  filter(FunctionalGroup=="CarnBenthos")
CBtotal<-fgts
CBtotal[2:4] <- list(NULL)
CBtotal[3:13] <-list(NULL)
names(CBtotal)[names(CBtotal) == "Name"] <- "Year"
CBtotal$Species<-"Total"
CBtotal$FunctionalGroup<-"CarnBenthos"
names(CBtotal)[names(CBtotal) == "CB"] <- "BiomassGM2"
CBtotal<-CBtotal[,c("Year","Species","BiomassGM2","FunctionalGroup")]
CBtotal <- rbind(CBbio, CBtotal)

CBtotal$Species<-factor(CBtotal$Species, levels = c("Total", "Rock Crab", "Lobster", "Spider Crab", "Whelk"))

CBtotal$Year <- as.numeric(as.character(CBtotal$Year))

cb<-ggplot(CBtotal, aes(x=Year, y=BiomassGM2, col=Species)) +
  geom_line() +
  labs(y = bquote('Biomass'~(g/m^2))) +
  ggtitle("Carnivorous Benthos") +scale_color_viridis_d() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))

###Pisc fish
Piscbio<-bio %>%
  filter(FunctionalGroup=="PiscFish")
Pisctotal<-fgts
Pisctotal[2] <- list(NULL)
Pisctotal[3:13] <-list(NULL)
names(Pisctotal)[names(Pisctotal) == "Name"] <- "Year"
Pisctotal$Species<-"Total"
Pisctotal$FunctionalGroup<-"PiscFish"
names(Pisctotal)[names(Pisctotal) == "PiscFish"] <- "BiomassGM2"
Pisctotal<-Pisctotal[,c("Year","Species","BiomassGM2","FunctionalGroup")]
Pisctotal <- rbind(Piscbio, Pisctotal)

Pisctotal$Species<-factor(Pisctotal$Species, levels = c("Total", "Summer Flounder", "Spiny Dogfish", 
                                                        "Striped Bass", "Bluefish", "Weakfish"))

Pisctotal$Year <- as.numeric(as.character(Pisctotal$Year))

pisc<-ggplot(Pisctotal, aes(x=Year, y=BiomassGM2, col=Species)) +
  geom_line() +
  labs(y = bquote('Biomass'~(g/m^2))) +
  ggtitle("Piscivorous Fish") +scale_color_viridis_d() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))

###Plank fish
Plankbio<-bio %>%
  filter(FunctionalGroup=="PlankFish")
Planktotal<-fgts
Planktotal[2:3] <- list(NULL)
Planktotal[3:13] <-list(NULL)
names(Planktotal)[names(Planktotal) == "Name"] <- "Year"
Planktotal$Species<-"Total"
Planktotal$FunctionalGroup<-"PlankFish"
names(Planktotal)[names(Planktotal) == "PlankFish"] <- "BiomassGM2"
Planktotal<-Planktotal[,c("Year","Species","BiomassGM2","FunctionalGroup")]
Planktotal <- rbind(Plankbio, Planktotal)

Planktotal$Species<-factor(Planktotal$Species, levels = c("Total", "Atlantic Herring", "Silverside",
                                                          "Butterfish", "Menhaden", "Bay Anchovy", "Alewife", "Blueback Herring",
                                                          "Moonfish"))

Planktotal$Year <- as.numeric(as.character(Planktotal$Year))

##Increasing species
menhaden<-Planktotal %>%
  filter(Species=="Menhaden")
ggplot(menhaden, aes(x=Year, y=BiomassGM2))+geom_line()+stat_smooth(method="lm")

herring<-Planktotal %>%
  filter(Species=="Atlantic Herring")
ggplot(herring, aes(x=Year, y=BiomassGM2))+geom_line()+stat_smooth(method="lm")

anchovy<-Planktotal %>%
  filter(Species=="Bay Anchovy")
ggplot(anchovy, aes(x=Year, y=BiomassGM2))+geom_line()+stat_smooth(method="lm")

butterfish<-Planktotal %>%
  filter(Species=="Butterfish")
ggplot(butterfish, aes(x=Year, y=BiomassGM2))+geom_line()+stat_smooth(method="lm")

silverside<-Planktotal %>%
  filter(Species=="Silverside")
ggplot(silverside, aes(x=Year, y=BiomassGM2))+geom_line()+stat_smooth(method="lm")

Alewife<-Planktotal %>%
  filter(Species=="Alewife")
ggplot(Alewife, aes(x=Year, y=BiomassGM2))+geom_line()+stat_smooth(method="lm")

moonfish<-Planktotal %>%
  filter(Species=="Moonfish")
ggplot(moonfish, aes(x=Year, y=BiomassGM2))+geom_line()+stat_smooth(method="lm")


###DECREASE
blueback<-Planktotal %>%
  filter(Species=="Blueback Herring")
ggplot(blueback, aes(x=Year, y=BiomassGM2))+geom_line()+stat_smooth(method="lm")





plank<-ggplot(Planktotal, aes(x=Year, y=BiomassGM2, col=Species)) +
  geom_line() +
  labs(y = bquote('Biomass'~(g/m^2))) +
  ggtitle("Planktivorous Fish") +scale_color_viridis_d() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))



##Benth fish
Benthbio<-bio %>%
  filter(FunctionalGroup=="BenthFish")
Benthtotal<-fgts
Benthtotal[3:16] <-list(NULL)
names(Benthtotal)[names(Benthtotal) == "Name"] <- "Year"
Benthtotal$Species<-"Total"
Benthtotal$FunctionalGroup<-"BenthFish"
names(Benthtotal)[names(Benthtotal) == "BenthFish"] <- "BiomassGM2"
Benthtotal<-Benthtotal[,c("Year","Species","BiomassGM2","FunctionalGroup")]
Benthtotal <- rbind(Benthbio, Benthtotal)

Benthtotal$Species<-factor(Benthtotal$Species, levels = c("Total", "Little Skate", "Scup",
                                                          "Sea robin", "Black Sea Bass",
                                                          "Tautog", "Winter Flounder"))

Benthtotal$Year <- as.numeric(as.character(Benthtotal$Year))

benth<-ggplot(Benthtotal, aes(x=Year, y=BiomassGM2, col=Species)) +
  geom_line() +
  labs(y = bquote('Biomass'~(g/m^2))) +
  ggtitle("Benthivorous Fish") +scale_color_viridis_d(labels=c("Total", "Little Skate", "Scup",
                                                               "Striped Sea Robin", "Black Sea Bass",
                                                               "Tautog", "Winter Flounder")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))

library(gridExtra)


grid.arrange(cb, pisc, plank, benth, nrow=2) ##############BIOMASS PLOT

##PIP
SmPIPbio<-bio %>%
  filter(FunctionalGroup=="SmallPIP")
LgPIPbio<-bio %>%
  filter(FunctionalGroup=="LargePIP")

PIPtotal <- rbind(SmPIPbio, LgPIPbio)

PIPtotal$FunctionalGroup<-factor(PIPtotal$FunctionalGroup, levels = c("SmallPIP", "LargePIP"))
#SmPIPtotal$Year <- as.numeric(as.character(SmPIPtotal$Year))

ggplot(PIPtotal, aes(x=Year, y=BiomassGM2, col=FunctionalGroup)) +
  geom_line() +
  labs(y = bquote('Biomass'~(g/m^2)), color="Size Class") +
  ggtitle("PIP Biomass") +scale_color_viridis_d(labels=c("Small PIP", "Large PIP")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))



##REC fishing plots
ggplot(data=rec, aes(x=Year, y=G.M2, col=CommonName)) + geom_line()

pisc.rec<-rec %>%
  filter(Group=="Piscivorous Fish")

pisc.rec$CommonName<-factor(pisc.rec$CommonName, levels = c("Bluefish", "Striped Bass", "Summer Flounder"))

p1<-ggplot(data=pisc.rec, aes(x=Year, y=G.M2, col=CommonName)) + geom_line() +
  labs(y = bquote('Recreational Catch'~(g/m^2)), color="Species") +
  ggtitle("Piscivorous Fish") +scale_color_viridis_d() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))

#decreasing
bluefish.rec<-pisc.rec %>%
  filter(CommonName=="Bluefish")
ggplot(bluefish.rec, aes(x=Year, y=G.M2)) + geom_line() + stat_smooth(method="lm")

##increasing
stripedbass<-pisc.rec %>%
  filter(CommonName=="Striped Bass")
ggplot(stripedbass, aes(x=Year, y=G.M2)) + geom_line() + stat_smooth(method="lm")

#flat
summerflounder<-pisc.rec %>%
  filter(CommonName=="Summer Flounder")
ggplot(summerflounder, aes(x=Year, y=G.M2)) + geom_line() + stat_smooth(method="lm")



benth.rec<-rec %>%
  filter(Group=="Benthivorous Fish")
benth.rec$CommonName<-factor(benth.rec$CommonName, levels = c("Scup", "Tautog",  "Black Sea Bass", 
                                                              "Striped Searobin"))

p2<-ggplot(data=benth.rec, aes(x=Year, y=G.M2, col=CommonName)) + geom_line() +
  labs(y = bquote('Recreational Catch'~(g/m^2)), color="Species") +
  ggtitle("Benthivorous Fish") +scale_color_viridis_d(labels = c("Scup", "Tautog", "Black Sea Bass", 
                                                                 "Striped Sea Robin")) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))
library(gridExtra)
grid.arrange (p1, p2, nrow=1)

library(tidyverse)
##Comm
CommPlank<-comm %>%
  filter(Group=="Planktivorous Fish")

CommPlank$Species<-factor(CommPlank$Species, levels = c("Atlantic Herring", "Menhaden", "Butterfish",
                                                        "Alewife_Blueback",  "Moonfish_Silverside"))
                                                              

ggplot(data=CommPlank, aes(x=Year, y=CatchGM2, col=Species)) + geom_line() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  scale_color_viridis_d(labels=c("Atlantic Herring", "Menhaden", "Butterfish", "Alewife & Blueback Herring", 
                                 "Moonfish & Silverside"))+
  labs(y=bquote('Commercial Catch'~(g/m^2)))

CommMenhaden<-CommPlank %>%
  filter(Species=="Menhaden")
ggplot(data=CommMenhaden, aes(x=Year, y=CatchLBS)) + geom_line() + stat_smooth(method="lm")


CommHerring<-CommPlank %>%
  filter(Species=="Atlantic Herring")
ggplot(data=CommHerring, aes(x=Year, y=CatchLBS)) + geom_line() + stat_smooth(method="lm")

CommSFB<-comm %>%
  filter(Group=="Suspension Feeding Benthos")
ggplot(data=CommSFB, aes(x=Year, y=CatchLBS, col=Species)) + geom_line()
