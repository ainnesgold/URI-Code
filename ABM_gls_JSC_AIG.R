library(tidyverse)
library(nlme)
library(ggpubr)

ABMtrials<-read.csv("~/Google Drive (ainnesgold@uri.edu)/Anne Innes-Gold/Data/ABMtrials.csv")
ABMtrials$FisherEndEnthusiasm<-(ABMtrials$ShoreEndEnth + ABMtrials$BoatEndEnth) / 2


##trying the gls function

ABM_subset<- ABMtrials %>%
  filter(EnthLoss=="1") %>%
  filter(CatchCurveType=="Power") %>%
  group_by(Year) %>%
  filter(Year>2022)


# Conclude that Scenario is highly significant and there is a positive trend over time
#plot(m1)
# No pattern in the residuals with the fitted values
m1<-gls(FisherEndEnthusiasm~Scenario+Year, data=ABM_subset)
anova(m1)
summary(m1)

m2<-gls(FisherEndEnthusiasm~Scenario+Year, data=ABM_subset, correlation = corAR1(form = ~Year|Scenario))
summary(m2)

# form = ~Year|Scenario ensures that the autocorrelation is calculated in chronolical order, separated by scenario.  
# This form gives a more accurate estimate of the autocorrelation coefficient
anova(m2)
summary(m2)
# Model m2 accounts for the increasing time trend and for the autocorrelation among residuals over time
# In principle this gives better estimates of the main effect Scenario, which is the one you are intersted in.

# From the standard errors one could infer that Intermediate is statistically higher than Severe
# Status Quo and Zero are higher than the others but not different from each other.


##abundance catch trial type tests
ABM_subset<- ABMtrials %>%
  filter(EnthLoss=="0.5") %>%
  filter(Scenario=="Extreme") %>%
  group_by(Year) %>%
  filter(Year>2022)

m2<-gls(FisherEndEnthusiasm~CatchCurveType+Year, data=ABM_subset, correlation = corAR1(form = ~Year|CatchCurveType))
anova(m2)
summary(m2)


##enthusiasm loss trial type tests
ABM_subset<- ABMtrials %>%
  filter(CatchCurveType=="Power") %>%
  filter(Scenario=="Intermediate") %>%
  group_by(Year) %>%
  filter(Year>2022)

m2<-gls(PiscBiomass~as.factor(EnthLoss)+Year, data=ABM_subset, correlation = corAR1(form = ~Year|as.factor(EnthLoss)))
anova(m2)
summary(m2)




############regressions####################

ABM_subset<- ABMtrials %>%
  #filter(Scenario=="Zero") %>%
  filter(EnthLoss==0.5) %>%
  group_by(Year) %>%
  filter(Year>2020)

m4<-lm(FisherEndEnthusiasm~PiscBiomass, data=ABM_subset)
summary(m4)

#check resids
plot(m4)
plot(ABM_subset$Year, resid(m4))
#color symbols by catch curve type?

m5<-lm(FisherEndEnthusiasm~PiscBiomass*CatchCurveType*Scenario, data=ABM_subset)
summary(m5)

##run m5, check for interaction terms. if it's non significant, then i can run m4
#if it is significant, then the relationship between x and y depends on the catch curve type
#focus on r2 value

ABM_subset$CatchCurveType <- factor(ABM_subset$CatchCurveType, 
                                          levels=c("Linear", "Static", "Power"), 
                                          labels=c("Linear", "Static", "Power"))


p1<-ggplot(data=ABM_subset, aes(x=LgFishDead, y=FisherEndEnthusiasm, color=CatchCurveType)) + 
  geom_point() +
  geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="black")+
  scale_color_manual(
    name = "Abundance-Catch Relationship",
    values = c("Linear" = "#9cd8cf",
               "Static" = "#2b7a74",
               "Power" = "#013f3a"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16),
        legend.title=element_text(size=16),
       legend.text=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(x="Number of Keepers Caught", y="Fisher End of Season Satisfaction")

p2<-ggplot(data=ABM_subset, aes(x=PiscBiomass, y=FisherEndEnthusiasm, color=CatchCurveType)) + 
  geom_point() +
  geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="black")+
  scale_color_manual(
    name = "Abundance-Catch Relationship",
    values = c("Linear" = "#9cd8cf",
               "Static" = "#2b7a74",
               "Power" = "#013f3a"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(x=bquote('Biomass'~(g/m^2)), y="Fisher End of Season Satisfaction")

p3<-ggplot(data=ABM_subset, aes(x=PiscBiomass, y=LgFishDead, color=CatchCurveType)) + 
  geom_point() +
  geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="black")+
  scale_color_manual(
    name = "Abundance-Catch Relationship",
    values = c("Linear" = "#9cd8cf",
               "Static" = "#2b7a74",
               "Power" = "#013f3a"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(x=bquote('Biomass'~(g/m^2)), y="Number of Keepers Caught")

ggarrange(p3,p2,p1, nrow=1, ncol=3, common.legend=TRUE)



##enthusiasm loss plots

ABM_subset<-ABMtrials %>%
  #filter(Scenario=="Zero") %>%
  filter(CatchCurveType=="Power") %>%
  group_by(Year) %>%
  filter(Year>2020)

m4<-lm(LgFishDead~PiscBiomass, data=ABM_subset)
summary(m4)

#check resids
plot(m4)
plot(ABM_subset$Year, resid(m4))
#color symbols by catch curve type?

m5<-lm(LgFishDead~PiscBiomass*as.factor(EnthLoss)*Scenario, data=ABM_subset)
summary(m5)

##run m5, check for interaction terms. if it's non significant, then i can run m4
#if it is significant, then the relationship between x and y depends on the catch curve type
#focus on r2 value

ABM_subset$EnthLoss <- factor(ABM_subset$CatchCurveType, 
                                    levels=c("1", "0.5", "0"), 
                                    labels=c("1", "0.5", "0"))

p1<-ggplot(data=ABM_subset, aes(x=LgFishDead, y=FisherEndEnthusiasm, color=as.factor(EnthLoss))) + 
  geom_point() +
  geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="black")+
  scale_color_manual(
    name = "Satisfaction Loss Rate",
    values = c("1" = "#452805",
               "0.5" = "#BF812D",
               "0" = "#DFC27D"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(x="Number of Keepers Caught", y="Fisher End of Season Satisfaction")

p2<-ggplot(data=ABM_subset, aes(x=PiscBiomass, y=FisherEndEnthusiasm, color=as.factor(EnthLoss))) + 
  geom_point() +
  geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="black")+
  scale_color_manual(
    name = "Satisfaction Loss Rate",
    values = c("1" = "#452805",
               "0.5" = "#BF812D",
               "0" = "#DFC27D"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(x=bquote('Biomass'~(g/m^2)), y="Fisher End of Season Satisfaction")

p3<-ggplot(data=ABM_subset, aes(x=PiscBiomass, y=LgFishDead, color=as.factor(EnthLoss))) + 
  geom_point() +
  geom_smooth(method=lm, na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="black")+
  scale_color_manual(
    name = "Satisfaction Loss Rate",
    values = c("1" = "#452805",
               "0.5" = "#BF812D",
               "0" = "#DFC27D"))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 20),
        axis.text = element_text( size = 14 ),
        axis.title = element_text( size = 16),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(x=bquote('Biomass'~(g/m^2)), y="Number of Keepers Caught")

ggarrange(p3,p2,p1, nrow=1, ncol=3, common.legend=TRUE)

