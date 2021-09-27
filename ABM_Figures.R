library(tidyverse)
library(viridis)
library(ggpubr)
library(rstatix)
library(nlme)


ABMtrials<-read.csv("~/Google Drive/URI/ABM/Code CSVs/ABMtrials.csv")

ABMtrials$FisherEndEnthusiasm<-(ABMtrials$ShoreEndEnth + ABMtrials$BoatEndEnth) / 2





##ANOVA TRIALS

ABM_subset<- ABMtrials %>%
  filter(EnthLoss=="0") %>%
  filter(CatchCurveType=="Power") %>%
  group_by(Year) %>%
  filter(Year>2022)

#ABM_subset$EnthLoss<-as.factor(ABM_subset$EnthLoss)
res.aov<-aov(lm(ABM_subset$LgFishDead ~ ABM_subset$Year*ABM_subset$Scenario))
summary(res.aov)
TukeyHSD(res.aov)


ABM_subset<- ABMtrials %>%
  filter(EnthLoss==0.5) %>%
  filter(CatchCurveType=="Static") %>%
  filter(Year > 2022) %>%
  group_by(Year)

anova(lm(ABM_subset$PiscBiomass ~ ABM_subset$Scenario*ABM_subset$Year))






#res.aov <- aov(LgFishDead ~ CatchCurveType + Year, data = ABM_subset)
#summary(res.aov)
#TukeyHSD(res.aov)


ABM_subset<- ABMtrials %>%
  #filter(Scenario=="Extreme") %>%
  filter(EnthLoss==0.5) %>%
  filter(Year > 2022)


a<-ggplot(data=ABM_subset, aes(PiscBiomass)) + 
  geom_histogram() +
  ggtitle("") +
  labs(y="Frequency", x=bquote('Piscivorous Fish Biomass'~(g/m^2)))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)))

b<-ggplot(data=ABM_subset, aes(LgFishDead)) + 
  geom_histogram() +
  ggtitle("Abundance-Catch Trials") +
  labs(y="", x="Number of Keepers Caught")+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)), 
        plot.title = element_text(hjust = 0.5))

c<-ggplot(data=ABM_subset, aes(FisherEndEnthusiasm)) + 
  geom_histogram() +
  ggtitle("") +
  labs(y="", x="End of Season Satisfaction")+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)))


ABM_subset2<- ABMtrials %>%
  #filter(Scenario=="Extreme") %>%
  filter(CatchCurveType=="Power") %>%
  filter(Year > 2022)

d<-ggplot(data=ABM_subset2, aes(PiscBiomass)) + 
  geom_histogram() +
  ggtitle("") +
  labs(y="Frequency", x=bquote('Piscivorous Fish Biomass'~(g/m^2)))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)))

e<-ggplot(data=ABM_subset2, aes(LgFishDead)) + 
  geom_histogram() +
  ggtitle("Satisfaction Loss Trials") +
  labs(y="", x="Number of Keepers Caught")+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)), 
        plot.title = element_text(hjust = 0.5))

f<-ggplot(data=ABM_subset2, aes(FisherEndEnthusiasm)) + 
  geom_histogram() +
  ggtitle("") +
  labs(y="", x="End of Season Satisfaction")+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)))
ggarrange(a,b,c,d,e,f, nrow=2, ncol=3)



#point range plots
ABMpoints<-ABMtrials %>%
  filter(Year > 2022) %>%
  filter(EnthLoss == 0.5) %>%
  group_by(Scenario, CatchCurveType) %>%
  summarise(MinBiomass=min(PiscBiomass), MaxBiomass=max(PiscBiomass),
            MeanBiomass=mean(PiscBiomass),
            MinEnthusiasm=min(FisherEndEnthusiasm), MaxEnthusiasm=max(FisherEndEnthusiasm),
            MeanEnthusiasm=mean(FisherEndEnthusiasm),
            MinFishCaught=min(LgFishDead), MaxFishCaught=max(LgFishDead), 
            MeanFishCaught=mean(LgFishDead))

ABMpoints$Scenario <- factor(ABMpoints$Scenario, 
                               levels=c("Zero", "Status Quo", "Intermediate", 
                                        "Extreme"), 
                               labels=c("Zero", "Status Quo", "Intermediate",  "Extreme"))

ABMpoints$CatchCurveType <- factor(ABMpoints$CatchCurveType, 
                                     levels=c("Linear", "Static", "Power"), 
                                     labels=c("Linear", "Static", "Power"))


a<-ggplot() +
  geom_pointrange(data=ABMpoints, aes(x=Scenario, y=MeanBiomass, ymin=MinBiomass,
                                      ymax=MaxBiomass,
                                      color=CatchCurveType),
                  position = position_dodge(width=0.5)) +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ), 
                     name="Abundance-Catch Relationship") +
  #scale_color_viridis_d(name="Abundance-Catch Relationship") +
  labs(y=bquote('Piscivorous Fish Biomass'~(g/m^2)), x="Forage Fish Harvest Scenario")+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text( size = 12),
        axis.title = element_text( size = 12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12))+
  coord_flip()

b<-ggplot() +
  geom_pointrange(data=ABMpoints, aes(x=Scenario, y=MeanFishCaught, ymin=MinFishCaught,
                                      ymax=MaxFishCaught,
                                      color=CatchCurveType),
                  position = position_dodge(width=0.5)) +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ), 
                     name="Abundance-Catch Relationship") +
  theme_bw() +
  labs(y="Number of Keepers Caught", x="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text( size = 12),
        axis.title = element_text( size = 12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12))+
  coord_flip()

c<-ggplot() +
  geom_pointrange(data=ABMpoints, aes(x=Scenario, y=MeanEnthusiasm, ymin=MinEnthusiasm,
                                      ymax=MaxEnthusiasm,
                                      color=CatchCurveType),
                  position = position_dodge(width=0.5)) +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a"), 
                     name="Abundance-Catch Relationship") +
  #scale_color_viridis_d(name="Abundance-Catch Relationship") +
  theme_bw() +
  labs(y="Fisher End of Season Enthusiasm", x="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text( size = 12),
        axis.title = element_text( size = 12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12))+
  coord_flip()
ggarrange(a,b,c, nrow=1, ncol=3, common.legend = TRUE, legend="top")

##enthusiasm plots
ABMpoints_enth<-ABMtrials %>%
  filter(Year > 2022) %>%
  filter(CatchCurveType == "Power") %>%
  group_by(Scenario, EnthLoss) %>%
  summarise(MinBiomass=min(PiscBiomass), MaxBiomass=max(PiscBiomass),
            MeanBiomass=mean(PiscBiomass),
            MinEnthusiasm=min(FisherEndEnthusiasm), MaxEnthusiasm=max(FisherEndEnthusiasm),
            MeanEnthusiasm=mean(FisherEndEnthusiasm),
            MinFishCaught=min(LgFishDead), MaxFishCaught=max(LgFishDead), 
            MeanFishCaught=mean(LgFishDead))

ABMpoints_enth$Scenario <- factor(ABMpoints_enth$Scenario, 
                             levels=c("Zero", "Status Quo", "Intermediate", 
                                      "Extreme"), 
                             labels=c("Zero", "Status Quo", "Intermediate",  "Extreme"))


ABMpoints_enth$EnthLoss <- factor(ABMpoints_enth$EnthLoss, 
                              levels=c("1", "0.5", "0"), 
                              labels=c("1", "0.5", "0"))

d<-ggplot() +
  geom_pointrange(data=ABMpoints_enth, aes(x=Scenario, y=MeanBiomass, ymin=MinBiomass,
                                      ymax=MaxBiomass,
                                      color=as.factor(EnthLoss)),
                  position = position_dodge(width=0.5)) +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"), name="Satisfaction Loss Rate") +
  labs(y=bquote('Piscivorous Fish Biomass'~(g/m^2)), x="Forage Fish Harvest Scenario") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)))+
  coord_flip()


e<-ggplot() +
  geom_pointrange(data=ABMpoints_enth, aes(x=Scenario, y=MeanFishCaught, ymin=MinFishCaught,
                                           ymax=MaxFishCaught,
                                           color=as.factor(EnthLoss)),
                  position = position_dodge(width=0.5)) +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"), name="Satisfaction Loss Rate") +
  labs(y="Number of Keepers Caught", x="") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0))) +
  coord_flip()

f<-ggplot() +
  geom_pointrange(data=ABMpoints_enth, aes(x=Scenario, y=MeanEnthusiasm, ymin=MinEnthusiasm,
                                           ymax=MaxEnthusiasm,
                                           color=as.factor(EnthLoss)),
                  position = position_dodge(width=0.5)) +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"), name="Satisfaction Loss Rate") +
  labs(y="Fisher End of Season Satisfaction", x= "")+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0))) +
  coord_flip()


#BrBG2 = c("#452805","#BF812D", "#DFC27D", "#F6E8C3" ,
          #"#f1faf9" , "#9cd8cf", "#2b7a74", "#013f3a" )

ggarrange(d,e,f, nrow=1, ncol=3, common.legend = TRUE, legend = "top")



##point range plots grouped by model run type################
a<-ggplot() +
  geom_pointrange(data=ABMpoints, aes(x=CatchCurveType, y=MeanBiomass, ymin=MinBiomass,
                                      ymax=MaxBiomass,
                                      color=Scenario),
                  position = position_dodge(width=0.5)) +
  scale_color_brewer(type = 'div', palette = 4, direction = -1, 
                     name="Forage Fish \nHarvest Scenario")+
  labs(y="", x="Abundance-Catch Relationship")+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0))) +
  coord_flip()

b<-ggplot() +
  geom_pointrange(data=ABMpoints, aes(x=CatchCurveType, y=MeanFishCaught, ymin=MinFishCaught,
                                      ymax=MaxFishCaught,
                                      color=Scenario),
                  position = position_dodge(width=0.5)) +
  scale_color_brewer(type = 'div', palette = 4, direction = -1, 
                     name="Forage Fish \nHarvest Scenario")+
  theme_bw() +
  labs(y="", x="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0))) +
  coord_flip()

c<-ggplot() +
  geom_pointrange(data=ABMpoints, aes(x=CatchCurveType, y=MeanEnthusiasm, ymin=MinEnthusiasm,
                                      ymax=MaxEnthusiasm,
                                      color=Scenario),
                  position = position_dodge(width=0.5)) +
  scale_color_brewer(type = 'div', palette = 4, direction = -1, 
                     name="Forage Fish \nHarvest Scenario")+
  theme_bw() +
  labs(y="", x="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0))) +
  coord_flip()


d<-ggplot() +
  geom_pointrange(data=ABMpoints_enth, aes(x=as.factor(EnthLoss), y=MeanBiomass, ymin=MinBiomass,
                                           ymax=MaxBiomass,
                                           color=Scenario),
                  position = position_dodge(width=0.5)) +
  scale_color_brewer(type = 'div', palette = 4, direction = -1,name="Satisfaction Loss Rate")+
  labs(y=bquote('Piscivorous Fish Biomass'~(g/m^2)), x="Satisfaction Loss Rate") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)))+
  coord_flip()


e<-ggplot() +
  geom_pointrange(data=ABMpoints_enth, aes(x=as.factor(EnthLoss), y=MeanFishCaught, ymin=MinFishCaught,
                                           ymax=MaxFishCaught,
                                           color=Scenario),
                  position = position_dodge(width=0.5)) +
  scale_color_brewer(type = 'div', palette = 4, direction = -1,name="Satisfaction Loss Rate")+  labs(y="Number of Keepers Caught", x="Planktivorous Fish Harvest Scenario") +
  theme_bw() +
  labs(y="Number of Keepers Caught", x="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0)))+
  coord_flip()

f<-ggplot() +
  geom_pointrange(data=ABMpoints_enth, aes(x=as.factor(EnthLoss), y=MeanEnthusiasm, ymin=MinEnthusiasm,
                                           ymax=MaxEnthusiasm,
                                           color=Scenario),
                  position = position_dodge(width=0.5)) +
  scale_color_brewer(type = 'div', palette = 4, direction = -1,name="Satisfaction Loss Rate")+  labs(y="Fisher End of Season Enthusiasm", x= "")+
  theme_bw() +
  labs(y="Fisher End of Season Satisfaction", x="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0))) +
  coord_flip()



ggarrange(a,b,c,d,e,f, nrow=2, ncol = 3, common.legend = TRUE, legend = "top")











##barplots

##First Figure, grouping by Scenario

##abundance catch plots

ABMcatchsum<-ABMtrials %>%
  filter(Year > 2020) %>%
  filter(EnthLoss == 0.5) %>%
  group_by(Scenario, CatchCurveType) %>%
  summarise(AvgBiomass=mean(PiscBiomass), SDBiomass=sd(PiscBiomass), 
            AvgLgFishDead=mean(LgFishDead), SDLgFishDead=sd(LgFishDead),
            AvgShoreEndEnth=mean(ShoreEndEnth), SDShoreEndEnth=sd(ShoreEndEnth),
            AvgBoatEndEnth=mean(BoatEndEnth), SDBoatEndEnth=sd(BoatEndEnth),
            AvgFisherEndEnth=mean(FisherEndEnthusiasm), SDFisherEndEnth=sd(FisherEndEnthusiasm))

ABMcatchsum$Scenario <- factor(ABMcatchsum$Scenario, 
                                levels=c("Zero", "Status Quo", "Intermediate", 
                                         "Extreme"), 
                                labels=c("Zero", "Status Quo", "Intermediate",  "Extreme"))

ABMcatchsum$CatchCurveType <- factor(ABMcatchsum$CatchCurveType, 
                               levels=c("Linear", "Static", "Power"), 
                               labels=c("Linear", "Static", "Power"))


a<-ggplot(ABMcatchsum, aes(x=Scenario, y=AvgBiomass, fill=CatchCurveType)) +
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=AvgBiomass-SDBiomass, ymax=AvgBiomass+SDBiomass), width=.2,
                position=position_dodge(.9))+
  theme_minimal() +
  scale_fill_viridis_d(name="Abundance-Catch Relationship") +
  labs(x="", y=bquote('Average Piscivorous Fish Biomass'~(g/m^2)))



b<-ggplot(ABMcatchsum, aes(x=Scenario, y=AvgLgFishDead, fill=CatchCurveType)) +
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=AvgLgFishDead-SDLgFishDead, ymax=AvgLgFishDead+SDLgFishDead), width=.2,
                position=position_dodge(.9))+
  theme_minimal() +
  scale_fill_viridis_d(name="Abundance-Catch Relationship") +
  labs(x="", y="Number Keepers Caught")

c<-ggplot(ABMcatchsum, aes(x=Scenario, y=AvgFisherEndEnth, fill=CatchCurveType)) +
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=AvgFisherEndEnth-SDFisherEndEnth, ymax=AvgFisherEndEnth+SDFisherEndEnth), width=.2,
                position=position_dodge(.9))+
  theme_minimal() +
  scale_fill_viridis_d(name="Abundance-Catch Relationship") +
  labs(x="", y="End of Season Fisher Enthusiasm")


ggarrange(a,b,c, nrow=1, ncol=3, common.legend = TRUE, legend="top")




##enthusiasm 
ABMenthsum<-ABMtrials %>%
  filter(Year > 2020) %>%
  filter(CatchCurveType == "Power") %>%
  group_by(Scenario, EnthLoss) %>%
  summarise(AvgBiomass=mean(PiscBiomass), SDBiomass=sd(PiscBiomass), 
            AvgLgFishDead=mean(LgFishDead), SDLgFishDead=sd(LgFishDead),
            AvgShoreEndEnth=mean(ShoreEndEnth), SDShoreEndEnth=sd(ShoreEndEnth),
            AvgBoatEndEnth=mean(BoatEndEnth), SDBoatEndEnth=sd(BoatEndEnth),
            AvgFisherEndEnth=mean(FisherEndEnthusiasm), SDFisherEndEnth=sd(FisherEndEnthusiasm))

ABMenthsum$Scenario <- factor(ABMenthsum$Scenario, 
                               levels=c("Zero", "Status Quo", "Intermediate", 
                                        "Extreme"), 
                               labels=c("Zero", "Status Quo", "Intermediate",  "Extreme"))

ABMenthsum$EnthLoss <- factor(ABMenthsum$EnthLoss, 
                                     levels=c("1", "0.5", "0"), 
                                     labels=c("1", "0.5", "0"))

d<-ggplot(ABMenthsum, aes(x=Scenario, y=AvgBiomass, fill=as.factor(EnthLoss))) +
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=AvgBiomass-SDBiomass, ymax=AvgBiomass+SDBiomass), width=.2,
                position=position_dodge(.9))+
  theme_minimal() +
  scale_fill_viridis_d(name="Enthusiasm Loss Rate", option ="inferno") +
  labs(x="", y=bquote(' Average Piscivorous Fish Biomass'~(g/m^2)))

e<-ggplot(ABMenthsum, aes(x=Scenario, y=AvgLgFishDead, fill=as.factor(EnthLoss))) +
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=AvgLgFishDead-SDLgFishDead, ymax=AvgLgFishDead+SDLgFishDead), width=.2,
                position=position_dodge(.9))+
  theme_minimal() +
  scale_fill_viridis_d(name="Enthusiasm Loss Rate", option ="inferno") +
  labs(x="Planktivorous Fish Harvest Scenario", y="Number Keepers Caught")

f<-ggplot(ABMenthsum, aes(x=Scenario, y=AvgFisherEndEnth, fill=as.factor(EnthLoss))) +
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=AvgFisherEndEnth-SDFisherEndEnth, ymax=AvgFisherEndEnth+SDFisherEndEnth), width=.2,
                position=position_dodge(.9))+
  theme_minimal() +
  scale_fill_viridis_d(name="Enthusiasm Loss Rate", option ="inferno") +
  labs(x="", y="End of Season Fisher Enthusiasm")

ggarrange(d,e,f, nrow=1, ncol=3, common.legend = TRUE, legend = "top")


##################Second figure, grouping by type of sensitivity analysis#########################
##abundance catch plots

ABMcatchsum<-ABMtrials %>%
  filter(Year > 2020) %>%
  filter(EnthLoss == 0.5) %>%
  group_by(Scenario, CatchCurveType) %>%
  summarise(AvgBiomass=mean(PiscBiomass), SDBiomass=sd(PiscBiomass), 
            AvgLgFishDead=mean(LgFishDead), SDLgFishDead=sd(LgFishDead),
            AvgShoreEndEnth=mean(ShoreEndEnth), SDShoreEndEnth=sd(ShoreEndEnth),
            AvgBoatEndEnth=mean(BoatEndEnth), SDBoatEndEnth=sd(BoatEndEnth),
            AvgFisherEndEnth=mean(FisherEndEnthusiasm), SDFisherEndEnth=sd(FisherEndEnthusiasm))

ABMcatchsum$Scenario <- factor(ABMcatchsum$Scenario, 
                               levels=c("Zero", "Status Quo", "Intermediate", 
                                        "Extreme"), 
                               labels=c("Zero", "Status Quo", "Intermediate",  "Extreme"))

ABMcatchsum$CatchCurveType <- factor(ABMcatchsum$CatchCurveType, 
                                     levels=c("Linear", "Static", "Power"), 
                                     labels=c("Linear", "Static", "Power"))


a<-ggplot(ABMcatchsum, aes(x=CatchCurveType, y=AvgBiomass, fill=Scenario)) +
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=AvgBiomass-SDBiomass, ymax=AvgBiomass+SDBiomass), width=.2,
                position=position_dodge(.9))+
  theme_minimal() +
  scale_fill_viridis_d(name="Planktivorous Fish Harvest Scenario", option="plasma") +
  labs(x="", y=bquote('Average Piscivorous Fish Biomass'~(g/m^2)))



b<-ggplot(ABMcatchsum, aes(x=CatchCurveType, y=AvgLgFishDead, fill=Scenario)) +
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=AvgLgFishDead-SDLgFishDead, ymax=AvgLgFishDead+SDLgFishDead), width=.2,
                position=position_dodge(.9))+
  theme_minimal() +
  scale_fill_viridis_d(name="Planktivorous Fish Harvest Scenario", option="plasma") +
  labs(x="Abundance-Catch Relationship", y="Number Keepers Caught")

c<-ggplot(ABMcatchsum, aes(x=CatchCurveType, y=AvgFisherEndEnth, fill=Scenario)) +
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=AvgFisherEndEnth-SDFisherEndEnth, ymax=AvgFisherEndEnth+SDFisherEndEnth), width=.2,
                position=position_dodge(.9))+
  theme_minimal() +
  scale_fill_viridis_d(name="Planktivorous Fish Harvest Scenario", option="plasma") +
  labs(x="", y="End of Season Fisher Enthusiasm")


ggarrange(a,b,c, nrow=1, ncol=3, common.legend = TRUE, legend="bottom")




##enthusiasm 
ABMenthsum<-ABMtrials %>%
  filter(Year > 2020) %>%
  filter(CatchCurveType == "Power") %>%
  group_by(Scenario, EnthLoss) %>%
  summarise(AvgBiomass=mean(PiscBiomass), SDBiomass=sd(PiscBiomass), 
            AvgLgFishDead=mean(LgFishDead), SDLgFishDead=sd(LgFishDead),
            AvgShoreEndEnth=mean(ShoreEndEnth), SDShoreEndEnth=sd(ShoreEndEnth),
            AvgBoatEndEnth=mean(BoatEndEnth), SDBoatEndEnth=sd(BoatEndEnth),
            AvgFisherEndEnth=mean(FisherEndEnthusiasm), SDFisherEndEnth=sd(FisherEndEnthusiasm))

ABMenthsum$Scenario <- factor(ABMenthsum$Scenario, 
                              levels=c("Zero", "Status Quo", "Intermediate", 
                                       "Extreme"), 
                              labels=c("Zero", "Status Quo", "Intermediate",  "Extreme"))

ABMenthsum$EnthLoss <- factor(ABMenthsum$EnthLoss, 
                              levels=c("1", "0.5", "0"), 
                              labels=c("1", "0.5", "0"))

d<-ggplot(ABMenthsum, aes(x=as.factor(EnthLoss), y=AvgBiomass, fill=Scenario)) +
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=AvgBiomass-SDBiomass, ymax=AvgBiomass+SDBiomass), width=.2,
                position=position_dodge(.9))+
  theme_minimal() +
  scale_fill_viridis_d(name="Planktivorous Fish Harvest Scenario", option="plasma") +
  labs(x="", y=bquote(' Average Piscivorous Fish Biomass'~(g/m^2)))

e<-ggplot(ABMenthsum, aes(x=as.factor(EnthLoss), y=AvgLgFishDead, fill=Scenario)) +
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=AvgLgFishDead-SDLgFishDead, ymax=AvgLgFishDead+SDLgFishDead), width=.2,
                position=position_dodge(.9))+
  theme_minimal() +
  scale_fill_viridis_d(name="Planktivorous Fish Harvest Scenario", option="plasma") +
  labs(x="Enthusiasm Loss Rate", y="Number Keepers Caught")

f<-ggplot(ABMenthsum, aes(x=as.factor(EnthLoss), y=AvgFisherEndEnth, fill=Scenario)) +
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=AvgFisherEndEnth-SDFisherEndEnth, ymax=AvgFisherEndEnth+SDFisherEndEnth), width=.2,
                position=position_dodge(.9))+
  theme_minimal() +
  scale_fill_viridis_d(name="Planktivorous Fish Harvest Scenario", option="plasma") +
  labs(x="", y="End of Season Fisher Enthusiasm")

ggarrange(a,b,c,d,e,f, nrow=2, ncol=3, common.legend = TRUE, legend = "top")









##############################################################################
############scatterplots & regressions ##############################
ABMtrials<-read.csv("~/Google Drive/URI/ABM/Code CSVs/ABMtrials.csv")

ABMtrials$FisherEndEnthusiasm<-(ABMtrials$ShoreEndEnth + ABMtrials$BoatEndEnth) / 2

Catch_subset<-ABMtrials %>%
  filter(Scenario == "Status Quo") %>%
  filter(EnthLoss == 0.5) %>%
  filter(CatchCurveType=="Power") %>%
  filter(Year>2020)

lm <- lm(LgFishDead~FisherEndEnthusiasm + Year, data = Catch_subset)
summary(lm)




Zero_catch_subset<-ABMtrials %>%
  filter(Scenario == "Zero") %>%
  filter(EnthLoss == 0.5) %>%
  filter(Year > 2020)

Zero_catch_subset$CatchCurveType <- factor(Zero_catch_subset$CatchCurveType, 
                                     levels=c("Linear", "Static", "Power"), 
                                     labels=c("Linear", "Static", "Power"))




##ZERO PLOTS
a<-ggplot(Zero_catch_subset, aes(x=FisherEndEnthusiasm, y=LgFishDead, color=CatchCurveType))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ CatchCurveType, scales = "free") +
  labs(x="End of Season Enthusiasm", y="Number of Keepers Caught") +
  theme_bw() +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))
  #geom_text(aes(label=Year),hjust=0.5, vjust=-1)



b<-ggplot(Zero_catch_subset, aes(x=FisherEndEnthusiasm, y=PiscBiomass, color=CatchCurveType))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ CatchCurveType, scales = "free") +
  labs(x="", y=bquote('Biomass'~(g/m^2))) +
  theme_bw() +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))
  #geom_text(aes(label=Year),hjust=0.5, vjust=-1)

##Final Zero figure
figure<-ggarrange(b,a, nrow=2, ncol=1)
annotate_figure(figure, top=text_grob("Zero", size = 20))




##status quo
SQ_catch_subset<-ABMtrials %>%
  filter(Scenario == "Status Quo") %>%
  filter(EnthLoss == 0.5) %>%
  filter(CatchCurveType=="Power") %>%
  filter(Year > 2022)

lm <- lm(LgFishDead~PiscBiomass, data = SQ_catch_subset)
summary(lm)
lm <- lm(PiscBiomass~FisherEndEnthusiasm, data = SQ_catch_subset)
summary(lm)

SQ_catch_subset<-ABMtrials %>%
  filter(Scenario == "Status Quo") %>%
  filter(EnthLoss == 0.5) %>%
  filter(Year > 2020)


SQ_catch_subset$CatchCurveType <- factor(SQ_catch_subset$CatchCurveType, 
                                           levels=c("Linear", "Static", "Power"), 
                                           labels=c("Linear", "Static", "Power"))


a<-ggplot(SQ_catch_subset, aes(x=FisherEndEnthusiasm, y=LgFishDead, color=CatchCurveType))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ CatchCurveType, scales="free") +
  labs(x="End of Season Enthusiasm", y="Number of Keepers Caught") +
  theme_bw() +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))


b<-ggplot(SQ_catch_subset, aes(x=FisherEndEnthusiasm, y=PiscBiomass, color=CatchCurveType))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ CatchCurveType, scales="free") +
  labs(x="", y=bquote('Biomass'~(g/m^2))) +
  theme_bw() +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))

figure<-ggarrange(b,a, nrow=2, ncol=1)
annotate_figure(figure, top=text_grob("Status Quo", size = 20))




##intermediate
Int_catch_subset<-ABMtrials %>%
  filter(Scenario == "Intermediate") %>%
  filter(EnthLoss == 0.5) %>%
  filter(CatchCurveType=="Power") %>%
  filter(Year > 2020)

lm <- lm(LgFishDead~PiscBiomass, data = Int_catch_subset)
summary(lm)

lm <- lm(PiscBiomass~FisherEndEnthusiasm, data = Int_catch_subset)
summary(lm)

Int_catch_subset<-ABMtrials %>%
  filter(Scenario == "Intermediate") %>%
  filter(EnthLoss == 0.5) %>%
  filter(Year > 2020)

Int_catch_subset$CatchCurveType <- factor(Int_catch_subset$CatchCurveType, 
                                          levels=c("Linear", "Static", "Power"), 
                                          labels=c("Linear", "Static", "Power"))

a<-ggplot(Int_catch_subset, aes(x=FisherEndEnthusiasm, y=LgFishDead, color=CatchCurveType))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ CatchCurveType, scales="free") +
  labs(x="End of Season Enthusiasm", y="Number of Keepers Caught") +
  theme_bw() +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))


b<-ggplot(Int_catch_subset, aes(x=FisherEndEnthusiasm, y=PiscBiomass, color=CatchCurveType))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ CatchCurveType, scales = "free") +
  labs(x="", y=bquote('Biomass'~(g/m^2))) +
  theme_bw() +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))

figure<-ggarrange(b,a, nrow=2, ncol=1)
annotate_figure(figure, top=text_grob("Intermediate", size = 20))


##Extreme
Ext_catch_subset<-ABMtrials %>%
  filter(Scenario == "Extreme") %>%
  filter(EnthLoss == 0.5) %>%
  filter(CatchCurveType=="Power") %>%
  filter(Year > 2020)


lm <- lm(LgFishDead~PiscBiomass, data = Ext_catch_subset)
summary(lm)
lm <- lm(PiscBiomass~FisherEndEnthusiasm, data = Ext_catch_subset)
summary(lm)

Ext_catch_subset<-ABMtrials %>%
  filter(Scenario == "Extreme") %>%
  filter(EnthLoss == 0.5) %>%
  filter(Year > 2020)

Ext_catch_subset$CatchCurveType <- factor(Ext_catch_subset$CatchCurveType, 
                                          levels=c("Linear", "Static", "Power"), 
                                          labels=c("Linear", "Static", "Power"))


a<-ggplot(Ext_catch_subset, aes(x=FisherEndEnthusiasm, y=LgFishDead, color=CatchCurveType))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ CatchCurveType, scales="free") +
  labs(x="End of Season Enthusiasm", y="Number of Keepers Caught") +
  theme_bw() +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))


b<-ggplot(Ext_catch_subset, aes(x=FisherEndEnthusiasm, y=PiscBiomass, color=CatchCurveType))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ CatchCurveType, scales = "free") +
  labs(x="", y=bquote('Biomass'~(g/m^2))) +
  theme_bw() +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))

figure<-ggarrange(b,a, nrow=2, ncol=1)
annotate_figure(figure, top=text_grob("Extreme", size = 20))




##Enthusiasm Scatters


##ZERO
ABM_enth_subset<-ABMtrials %>%
  filter(Scenario=="Zero") %>%
  filter(CatchCurveType == "Power") %>%
  filter(Year > 2020) %>%
  filter(EnthLoss==0)
lm <- lm(LgFishDead~PiscBiomass, data = ABM_enth_subset)
summary(lm)




ABM_enth_subset<-ABMtrials %>%
  filter(Scenario=="Zero") %>%
  filter(CatchCurveType == "Power") %>%
  filter(Year > 2020)


ABM_enth_subset$EnthLoss <- factor(ABM_enth_subset$EnthLoss, 
                              levels=c("1", "0.5", "0"), 
                              labels=c("1", "0.5", "0"))



a<-ggplot(ABM_enth_subset, aes(x=FisherEndEnthusiasm, y=LgFishDead, 
                            color=EnthLoss))+
  facet_wrap(~EnthLoss, scales="free") +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="End of Season Enthusiasm", y="Number of Keepers Caught") +
  theme_bw() +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))


b<-ggplot(ABM_enth_subset, aes(x=PiscBiomass, y=LgFishDead, 
                            color=EnthLoss))+
  facet_wrap(~EnthLoss, scales="free") +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="", y=bquote('Biomass'~(g/m^2))) +
  theme_bw() +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))

figure<-ggarrange(b,a, nrow=2, ncol=1)
annotate_figure(figure, top=text_grob("Zero", size = 20))

##Status Quo
ABM_enth_subset<-ABMtrials %>%
  filter(Scenario=="Status Quo") %>%
  filter(CatchCurveType == "Power") %>%
  filter(Year > 2020) %>%
  filter(EnthLoss==0)

lm <- lm(LgFishDead~PiscBiomass, data = ABM_enth_subset)
summary(lm)


ABM_enth_subset<-ABMtrials %>%
  filter(Scenario=="Status Quo") %>%
  filter(CatchCurveType == "Power") %>%
  filter(Year > 2020)


ABM_enth_subset$EnthLoss <- factor(ABM_enth_subset$EnthLoss, 
                                   levels=c("1", "0.5", "0"), 
                                   labels=c("1", "0.5", "0"))



a<-ggplot(ABM_enth_subset, aes(x=FisherEndEnthusiasm, y=LgFishDead, 
                               color=EnthLoss))+
  facet_wrap(~EnthLoss, scales="free") +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="End of Season Enthusiasm", y="Number of Keepers Caught") +
  theme_bw() +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))


b<-ggplot(ABM_enth_subset, aes(x=PiscBiomass, y=LgFishDead, 
                               color=EnthLoss))+
  facet_wrap(~EnthLoss, scales="free") +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="", y=bquote('Biomass'~(g/m^2))) +
  theme_bw() +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))

figure<-ggarrange(b,a, nrow=2, ncol=1)
annotate_figure(figure, top=text_grob("Status Quo", size = 20))


##Intermediate
ABM_enth_subset<-ABMtrials %>%
  filter(Scenario=="Intermediate") %>%
  filter(CatchCurveType == "Power") %>%
  filter(Year > 2020) %>%
  filter(EnthLoss==0)

lm <- lm(LgFishDead~PiscBiomass, data = ABM_enth_subset)
summary(lm)


ABM_enth_subset<-ABMtrials %>%
  filter(Scenario=="Intermediate") %>%
  filter(CatchCurveType == "Power") %>%
  filter(Year > 2020)

ABM_enth_subset$EnthLoss <- factor(ABM_enth_subset$EnthLoss, 
                                   levels=c("1", "0.5", "0"), 
                                   labels=c("1", "0.5", "0"))



a<-ggplot(ABM_enth_subset, aes(x=FisherEndEnthusiasm, y=LgFishDead, 
                               color=EnthLoss))+
  facet_wrap(~EnthLoss, scales="free") +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="End of Season Enthusiasm", y="Number of Keepers Caught") +
  theme_bw() +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))



b<-ggplot(ABM_enth_subset, aes(x=PiscBiomass, y=LgFishDead, 
                               color=EnthLoss))+
  facet_wrap(~EnthLoss, scales="free") +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="", y=bquote('Biomass'~(g/m^2))) +
  theme_bw() +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))

figure<-ggarrange(b,a, nrow=2, ncol=1)
annotate_figure(figure, top=text_grob("Intermediate", size = 20))


##Extreme
ABM_enth_subset<-ABMtrials %>%
  filter(Scenario=="Extreme") %>%
  filter(CatchCurveType == "Power") %>%
  filter(Year > 2020) %>%
  filter(EnthLoss==0)

lm <- lm(LgFishDead~PiscBiomass, data = ABM_enth_subset)
summary(lm)


ABM_enth_subset<-ABMtrials %>%
  filter(Scenario=="Extreme") %>%
  filter(CatchCurveType == "Power") %>%
  filter(Year > 2020)


ABM_enth_subset$EnthLoss <- factor(ABM_enth_subset$EnthLoss, 
                                   levels=c("1", "0.5", "0"), 
                                   labels=c("1", "0.5", "0"))



a<-ggplot(ABM_enth_subset, aes(x=FisherEndEnthusiasm, y=LgFishDead, 
                               color=EnthLoss))+
  facet_wrap(~EnthLoss, scales="free") +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="End of Season Enthusiasm", y="Number of Keepers Caught") +
  theme_bw() +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))



b<-ggplot(ABM_enth_subset, aes(x=PiscBiomass, y=LgFishDead, 
                               color=EnthLoss))+
  facet_wrap(~EnthLoss, scales="free") +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="", y=bquote('Biomass'~(g/m^2))) +
  theme_bw() +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))

figure<-ggarrange(b,a, nrow=2, ncol=1)
annotate_figure(figure, top=text_grob("Extreme", size = 20))


###SM biomass catch scatters
##supplementary biomass plot
#zero
Zero_catch_subset<-ABMtrials %>%
  filter(Scenario == "Zero") %>%
  filter(EnthLoss == 0.5) %>%
  filter(Year > 2020)

Zero_catch_subset$CatchCurveType <- factor(Zero_catch_subset$CatchCurveType, 
                                           levels=c("Linear", "Static", "Power"), 
                                           labels=c("Linear", "Static", "Power"))
ABM_enth_subset<-ABMtrials %>%
  filter(Scenario=="Zero") %>%
  filter(CatchCurveType == "Power") %>%
  filter(Year > 2020)


ABM_enth_subset$EnthLoss <- factor(ABM_enth_subset$EnthLoss, 
                                   levels=c("1", "0.5", "0"), 
                                   labels=c("1", "0.5", "0"))

a<-ggplot(Zero_catch_subset, aes(x=PiscBiomass, y=LgFishDead, color=CatchCurveType))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ CatchCurveType, scales = "free") +
  labs(x="", y="Number of Keepers Caught") +
  theme_bw() +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))



b<-ggplot(ABM_enth_subset, aes(x=PiscBiomass, y=LgFishDead, color=EnthLoss))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ EnthLoss, scales = "free") +
  labs(x=bquote('Biomass'~(g/m^2)), y="") +
  theme_bw() +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))

figure<-ggarrange(a,b, nrow=2, ncol=1)
annotate_figure(figure, top=text_grob("Zero", size = 20))





##SQ
SQ_catch_subset<-ABMtrials %>%
  filter(Scenario == "Status Quo") %>%
  filter(EnthLoss == 0.5) %>%
  filter(Year > 2020)


SQ_catch_subset$CatchCurveType <- factor(SQ_catch_subset$CatchCurveType, 
                                         levels=c("Linear", "Static", "Power"), 
                                         labels=c("Linear", "Static", "Power"))

ABM_enth_subset<-ABMtrials %>%
  filter(Scenario=="Status Quo") %>%
  filter(CatchCurveType == "Power") %>%
  filter(Year > 2020)


ABM_enth_subset$EnthLoss <- factor(ABM_enth_subset$EnthLoss, 
                                   levels=c("1", "0.5", "0"), 
                                   labels=c("1", "0.5", "0"))
a<-ggplot(SQ_catch_subset, aes(x=PiscBiomass, y=LgFishDead, color=CatchCurveType))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ CatchCurveType, scales="free") +
  labs(x="", y="") +
  theme_bw() +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))


b<-ggplot(ABM_enth_subset, aes(x=PiscBiomass, y=LgFishDead, color=EnthLoss))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ EnthLoss, scales = "free") +
  labs(x=bquote('Biomass'~(g/m^2)), y="") +
  theme_bw() +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))

figure<-ggarrange(a,b, nrow=2, ncol=1)
annotate_figure(figure, top=text_grob("Status Quo", size = 20))



##intermediate
Int_catch_subset<-ABMtrials %>%
  filter(Scenario == "Intermediate") %>%
  filter(EnthLoss == 0.5) %>%
  filter(Year > 2020)

Int_catch_subset$CatchCurveType <- factor(Int_catch_subset$CatchCurveType, 
                                          levels=c("Linear", "Static", "Power"), 
                                          labels=c("Linear", "Static", "Power"))




ABM_enth_subset<-ABMtrials %>%
  filter(Scenario=="Intermediate") %>%
  filter(CatchCurveType == "Power") %>%
  filter(Year > 2020)

ABM_enth_subset$EnthLoss <- factor(ABM_enth_subset$EnthLoss, 
                                   levels=c("1", "0.5", "0"), 
                                   labels=c("1", "0.5", "0"))

a<-ggplot(Int_catch_subset, aes(x=PiscBiomass, y=LgFishDead, color=CatchCurveType))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ CatchCurveType, scales="free") +
  labs(x="", y="") +
  theme_bw() +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))


b<-ggplot(ABM_enth_subset, aes(x=PiscBiomass, y=LgFishDead, color=EnthLoss))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ EnthLoss, scales = "free") +
  labs(x=bquote('Biomass'~(g/m^2)), y="") +
  theme_bw() +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))

figure<-ggarrange(a,b, nrow=2, ncol=1)
annotate_figure(figure, top=text_grob("Intermediate", size = 20))


##extreme
Ext_catch_subset<-ABMtrials %>%
  filter(Scenario == "Extreme") %>%
  filter(EnthLoss == 0.5) %>%
  filter(Year > 2020)

Ext_catch_subset$CatchCurveType <- factor(Ext_catch_subset$CatchCurveType, 
                                          levels=c("Linear", "Static", "Power"), 
                                          labels=c("Linear", "Static", "Power"))
a<-ggplot(Ext_catch_subset, aes(x=PiscBiomass, y=LgFishDead, color=CatchCurveType))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ CatchCurveType, scales="free") +
  labs(x="", y="") +
  theme_bw() +
  scale_color_manual(values=c("#9cd8cf", "#2b7a74", "#013f3a" ))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))


ABM_enth_subset<-ABMtrials %>%
  filter(Scenario=="Extreme") %>%
  filter(CatchCurveType == "Power") %>%
  filter(Year > 2020)


ABM_enth_subset$EnthLoss <- factor(ABM_enth_subset$EnthLoss, 
                                   levels=c("1", "0.5", "0"), 
                                   labels=c("1", "0.5", "0"))

b<-ggplot(ABM_enth_subset, aes(x=PiscBiomass, y=LgFishDead, color=EnthLoss))+
  geom_point() + 
  geom_smooth(method="lm") +
  facet_wrap( ~ EnthLoss, scales = "free") +
  labs(x=bquote('Biomass'~(g/m^2)), y="") +
  theme_bw() +
  scale_color_manual(values=c("#452805","#BF812D", "#DFC27D"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16))

figure<-ggarrange(a,b, nrow=2, ncol=1)
annotate_figure(figure, top=text_grob("Extreme", size = 20))


