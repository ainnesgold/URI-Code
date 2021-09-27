library(tidyverse)
library(gridExtra)
library(ggpubr)


RelEcosimfits<-read.csv("~/Desktop/URI/Data/Ecosim/MonteCarlo/V9_Final_Rel_Biomass.csv") #wherever you save this file
RelEcosimfits<-RelEcosimfits[-302,]
options(scipen=999)

MonteCarloOutput<-read.csv("~/Desktop/URI/DATA/Ecosim/MonteCarlo/V9_Final_MC.csv") #same
MonteCarloOutput<-MonteCarloOutput[-302,]

names(MonteCarloOutput)[names(MonteCarloOutput) == "Data"] <- "Year"
MonteCarloOutput<-MonteCarloOutput[-302,]
MC.long<-gather(MonteCarloOutput, key=Group, value=Value, -Year)  

MC.long<-MC.long %>%
  mutate(Group = case_when(
    str_detect(Group, "Seabirds") ~ "Seabirds",
    str_detect(Group, "Piscivorous")  ~ "PiscivorousFish",
    str_detect(Group, "Benthivorous")  ~ "BenthivorousFish",
    str_detect(Group, "Planktivorous")  ~ "PlanktivorousFish",
    str_detect(Group, "Lg")  ~ "AdultSquid",
    str_detect(Group, "Pelagic")  ~ "JuvenileSquid",
    str_detect(Group, "Carnivorous")  ~ "CarnivorousBenthos",
    str_detect(Group, "Cultured")  ~ "CulturedShellfish",
    str_detect(Group, "Suspension")  ~ "SuspensionFeedingBenthos",
    str_detect(Group, "Deposit")  ~ "DepositFeedingBenthos",
    str_detect(Group, "Gelatinous")  ~ "GelatinousZooplankton",
    str_detect(Group, "Zooplankton")  ~ "Zooplankton",
    str_detect(Group, "Benthic")  ~ "BenthicAlgae",
    str_detect(Group, "Seabirds")  ~ "Seabirds",
    str_detect(Group, "Phyto")  ~ "Phytoplankton",
    TRUE ~ Group
  )
  )

quant <- function(values, percentile){
  return(quantile(values, percentile, na.rm = TRUE))
} #create a function to calculate the percentile

quant.sum<-MC.long %>%
  group_by(Year, Group) %>%
  summarize(quantile.95 = quant(Value, 0.95), quantile.5 = quant(Value, 0.05))

quant.sum <- quant.sum[order(quant.sum$Group),]

quant.sum<-pivot_wider(data = quant.sum, 
                       id_cols = Year, 
                       names_from = Group, 
                       values_from = c("quantile.95", "quantile.5"))

RelEcosimfits<-left_join(RelEcosimfits, quant.sum, by="Year")


p1<-ggplot(RelEcosimfits, aes(x=Year, y=ObsBenth))+
  geom_point()+
  geom_line(data=RelEcosimfits, aes(x=Year, y=PredBenth))+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_BenthivorousFish,
                                        ymax=quantile.95_BenthivorousFish),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x = "", y = "Relative Biomass", title="Benthivorous Fish") +
  annotate(geom="text", x=1996, y=2.6, label="SS = 4.664",
           color="black", size=3) +
  scale_y_continuous(breaks=seq(0,3,1))


p2<-ggplot(RelEcosimfits, aes(x=Year, y=ObsPisc))+
  geom_point()+
  geom_line(data=RelEcosimfits, aes(x=Year, y=PredPisc))+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_PiscivorousFish,
                                        ymax=quantile.95_PiscivorousFish),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x = "", y = "", title="Piscivorous Fish") +
  annotate(geom="text", x=1996, y=9.35, label="SS = 2.775",
           color="black", size=3.1) + scale_y_continuous(breaks=seq(0,40,5))

p3<-ggplot(RelEcosimfits, aes(x=Year, y=ObsPlank))+
  geom_point()+
  geom_line(data=RelEcosimfits, aes(x=Year, y=PredPlank))+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_PlanktivorousFish,
                                        ymax=quantile.95_PlanktivorousFish),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x = "", y = "", title="Planktivorous Fish") +
  annotate(geom="text", x=1996, y=2.9, label="SS = 7.938",
           color="black", size=3)

p4<-ggplot(RelEcosimfits, aes(x=Year, y=ObsCB))+
  geom_point()+
  geom_line(data=RelEcosimfits, aes(x=Year, y=PredCB))+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_CarnivorousBenthos,
                                        ymax=quantile.95_CarnivorousBenthos),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x = "", y = "Relative Biomass", title="Carnivorous Benthos") +
  annotate(geom="text", x=1996, y=2.2, label="SS = 9.740",
           color="black", size=3) +
  scale_y_continuous(breaks=seq(0,3,1))




lgsquid<-ggplot(RelEcosimfits, aes(x=Year, y=ObsLgPIP))+
  geom_point()+
  geom_line(data=RelEcosimfits, aes(x=Year, y=PredLgPIP))+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_AdultSquid,
                                        ymax=quantile.95_AdultSquid),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x = "", y = "", title="Large Squid") +
  annotate(geom="text", x=1996, y=3.5, label="SS = 8.050",
           color="black", size=3)

smsquid<-ggplot(RelEcosimfits, aes(x=Year, y=ObsSmPIP))+
  geom_point()+
  geom_line(data=RelEcosimfits, aes(x=Year, y=PredSmPIP))+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_JuvenileSquid,
                                        ymax=quantile.95_JuvenileSquid),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x = "", y = "", title="Small Squid") +
  annotate(geom="text", x=1996.3, y=5.65, label="SS = 12.627",
           color="black", size=3)

gelzoo<-ggplot(RelEcosimfits, aes(x=Year, y=ObsCteno))+
  geom_point()+
  geom_line(data=RelEcosimfits, aes(x=Year, y=PredGelZoo))+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_GelatinousZooplankton,
                                        ymax=quantile.95_GelatinousZooplankton),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x = "", y = "Relative Biomass", title="Gelatinous Zooplankton") +
  annotate(geom="text", x=1996.75, y=2.9, label="SS = 100.640",
           color="black", size=3)

ggarrange(p1, p2, p3, p4, lgsquid, smsquid, gelzoo, forcing_phyto, forcing_cs, benth, pisc,
          plank, CB, squid, nrow=5, ncol=3)


forcing_phyto<-ggplot(data=RelEcosimfits, aes(x=Year, y=Phyto))+
  geom_line()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x = "", y = "", title="Phytoplankton (forcing function)") +
  scale_y_continuous(breaks=seq(0,1.75,0.5))

forcing_cs<-ggplot(data=RelEcosimfits, aes(x=Year, y=CultShell))+
  geom_line()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x = "", y = "", title="Cultured Shellfish (forcing function)") +
  scale_y_continuous(breaks=seq(0,400,200))




Fisheries<-read.csv("~/Desktop/URI/DATA/Fisheries.csv")
Benth_F<-Fisheries %>%
  filter(FG=="Benthivorous Fish")

benth<-ggplot(data=Benth_F, aes(x=Name, y=FishingPressure)) + geom_line() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x="", y="Fishing Mortality") +
  ggtitle("Benthivorous Fish F (forcing function)")

Pisc_F<-Fisheries %>%
  filter(FG=="Piscivorous Fish")

pisc<-ggplot(data=Pisc_F, aes(x=Name, y=FishingPressure)) + geom_line() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x="", y="") +
  ggtitle("Piscivorous Fish F (forcing function)")

Plank_F<-Fisheries %>%
  filter(FG=="Planktivorous Fish")

plank<-ggplot(data=Plank_F, aes(x=Name, y=FishingPressure)) + geom_line() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x="", y="") +
  ggtitle("Planktivorous Fish F (forcing function)")


CB_F<-Fisheries %>%
  filter(FG=="Carnivorous Benthos")

CB<-ggplot(data=CB_F, aes(x=Name, y=FishingPressure)) + geom_line() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x="", y="Fishing Mortality") +
  ggtitle("Carnivorous Benthos F (forcing function)") +
  scale_y_continuous(breaks=seq(0,2,1))


Squid_F<-Fisheries %>%
  filter(FG=="Large Squid")

squid<-ggplot(data=Squid_F, aes(x=Name, y=FishingPressure)) + geom_line() +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))+
  labs(x="Year", y="") +
  ggtitle("Large Squid F (forcing function)")



##Figure 4
ggarrange(p1, p2, p3, p4, lgsquid, smsquid, gelzoo, forcing_phyto, forcing_cs, benth, pisc,
          plank, CB, squid, nrow=5, ncol=3)


##other plots

benthalgae<-ggplot(data=RelEcosimfits, aes(x=Year, y=PredBenthAlgae))+
  geom_line()+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_BenthicAlgae,
                                        ymax=quantile.95_BenthicAlgae),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  labs(x = "Year", y = "Relative Biomass", title="Benthic Algae")

zoo<-ggplot(data=RelEcosimfits, aes(x=Year, y=PredZoo))+
  geom_line()+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_Zooplankton,
                                        ymax=quantile.95_Zooplankton),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  labs(x = "Year", y = "Relative Biomass", title="Zooplankton")

dfb<-ggplot(data=RelEcosimfits, aes(x=Year, y=PredDFB))+
  geom_line()+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_DepositFeedingBenthos,
                                        ymax=quantile.95_DepositFeedingBenthos),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  labs(x = "Year", y = "Relative Biomass", title="Deposit Feeding Benthos")

sfb<-ggplot(data=RelEcosimfits, aes(x=Year, y=PredSFB))+
  geom_line()+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_SuspensionFeedingBenthos,
                                        ymax=quantile.95_SuspensionFeedingBenthos),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  labs(x = "Year", y = "Relative Biomass", title="Suspension Feeding Benthos")

birds<-ggplot(data=RelEcosimfits, aes(x=Year, y=PredSeabird))+
  geom_line()+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_Seabirds,
                                        ymax=quantile.95_Seabirds),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))+
  labs(x = "Year", y = "Relative Biomass", title="Seabirds")

##Supplemental fig
ggarrange(benthalgae, zoo, dfb, sfb, birds, nrow=3, ncol=2)



















####can delete this probably
###trying this with the ALL the runs (not just 5th and 95th)

RelEcosimfits<-read.csv("~/Desktop/URI/Data/Ecosim/MonteCarlo/V9_RelEcosim_adj.csv") #wherever you save this file
options(scipen=999)

MonteCarloOutput<-read.csv("~/Desktop/URI/DATA/Ecosim/MonteCarlo/V9_MC_adj.csv") #same

names(MonteCarloOutput)[names(MonteCarloOutput) == "Data"] <- "Year"
MonteCarloOutput<-MonteCarloOutput[-302,]
MC.long<-gather(MonteCarloOutput, key=Group, value=Value, -Year)  

MC.long<-MC.long %>%
  mutate(Group = case_when(
    str_detect(Group, "Seabirds") ~ "Seabirds",
    str_detect(Group, "Piscivorous")  ~ "PiscivorousFish",
    str_detect(Group, "Benthivorous")  ~ "BenthivorousFish",
    str_detect(Group, "Planktivorous")  ~ "PlanktivorousFish",
    str_detect(Group, "Lg")  ~ "AdultSquid",
    str_detect(Group, "Pelagic")  ~ "JuvenileSquid",
    str_detect(Group, "Carnivorous")  ~ "CarnivorousBenthos",
    str_detect(Group, "Cultured")  ~ "CulturedShellfish",
    str_detect(Group, "Suspension")  ~ "SuspensionFeedingBenthos",
    str_detect(Group, "Deposit")  ~ "DepositFeedingBenthos",
    str_detect(Group, "Gelatinous")  ~ "GelatinousZooplankton",
    str_detect(Group, "Zooplankton")  ~ "Zooplankton",
    str_detect(Group, "Benthic")  ~ "BenthicAlgae",
    str_detect(Group, "Seabirds")  ~ "Seabirds",
    str_detect(Group, "Phyto")  ~ "Phytoplankton",
    TRUE ~ Group
  )
  )

quant <- function(values, percentile){
  return(quantile(values, percentile, na.rm = TRUE))
} #create a function to calculate the percentile

quant.sum<-MC.long %>%
  group_by(Year, Group) %>%
  summarize(quantile.95 = quant(Value, 1), quantile.5 = quant(Value, 0))

quant.sum <- quant.sum[order(quant.sum$Group),]

quant.sum<-pivot_wider(data = quant.sum, 
                       id_cols = Year, 
                       names_from = Group, 
                       values_from = c("quantile.95", "quantile.5"))

RelEcosimfits<-left_join(RelEcosimfits, quant.sum, by="Year")

par(mai=c(1.6,0.82,0.82,0.42))

p1<-ggplot(RelEcosimfits, aes(x=Year, y=ObsBenth))+
  geom_point()+
  geom_line(data=RelEcosimfits, aes(x=Year, y=PredBenth))+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_BenthivorousFish,
                                        ymax=quantile.95_BenthivorousFish),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5), plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))+
  labs(x = "Year", y = "Relative Biomass", title="Benthivorous Fish") +
  annotate(geom="text", x=2017, y=3, label="SS = 4.4363",
           color="black")


p2<-ggplot(RelEcosimfits, aes(x=Year, y=ObsPisc))+
  geom_point()+
  geom_line(data=RelEcosimfits, aes(x=Year, y=PredPisc))+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_PiscivorousFish,
                                        ymax=quantile.95_PiscivorousFish),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5), plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x = "Year", y = "Relative Biomass", title="Piscivorous Fish") +
  annotate(geom="text", x=2017, y=10, label="SS = 2.6749",
           color="black")

p3<-ggplot(RelEcosimfits, aes(x=Year, y=ObsPlank))+
  geom_point()+
  geom_line(data=RelEcosimfits, aes(x=Year, y=PredPlank))+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_PlanktivorousFish,
                                        ymax=quantile.95_PlanktivorousFish),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5), plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x = "Year", y = "Relative Biomass", title="Planktivorous Fish")+
  annotate(geom="text", x=2017, y=3, label="SS = 8.0407",
           color="black")

p4<-ggplot(RelEcosimfits, aes(x=Year, y=ObsCB))+
  geom_point()+
  geom_line(data=RelEcosimfits, aes(x=Year, y=PredCB))+
  geom_ribbon(data = RelEcosimfits, aes(ymin=quantile.5_CarnivorousBenthos,
                                        ymax=quantile.95_CarnivorousBenthos),alpha=0.3)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5), plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(x = "Year", y = "Relative Biomass", title="Carnivorous Benthos")+
  annotate(geom="text", x=2017, y=3, label="SS = 9.9082",
           color="black")

grid.arrange(p1,p2,p3,p4, nrow=2)


