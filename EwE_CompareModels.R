library(tidyverse)
library(gridExtra)
library(viridis)
library(ggpubr)



EcopathInputs<-read.csv("~/Desktop/URI/DATA/Inputs_Comparison.csv")


EcopathInputs <- EcopathInputs %>%
  filter(FunctionalGroup!="Phytoplankton") %>%
  filter(FunctionalGroup!="Zooplankton") %>%
  filter(FunctionalGroup!="Benthic Algae") 

#%>%
  #filter(FunctionalGroup!="Birds") %>%
  #filter(FunctionalGroup!="Deposit Feeding Benthos") %>%
  #filter(FunctionalGroup!="Detritus") %>%
  #filter(FunctionalGroup!="Gelatinous Zooplankton") #%>%
  #filter(FunctionalGroup!="Suspension Feeding Benthos")

EcopathInputs$FunctionalGroup <- 
  factor(EcopathInputs$FunctionalGroup,
         levels = c("Detritus", "Benthic Algae", "Gelatinous Zooplankton", "Deposit Feeding Benthos",
                    "Suspension Feeding Benthos", "Cultured Shellfish", "Carnivorous Benthos", "Small Squid",
                    "Large Squid",  "Benthivorous Fish", "Planktivorous Fish", "Piscivorous Fish",  "Seabirds"))


##BETTER PLOT!!!! ###################################################
##diiferent type of plot to show magnitude and variation, suggested by Jeremy

p1<-ggplot(EcopathInputs, aes(x=Biomass, y=FunctionalGroup)) + 
  geom_point(aes(shape=as.factor(Year)), size=4)+
  scale_shape_manual(values=c(1, 8), name="Model") + 
  geom_path() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  labs(x=bquote('Biomass'~(g/m^2)), y="")


##PB
p2<-ggplot(EcopathInputs, aes(x=PB, y=FunctionalGroup)) + 
  geom_point(aes(shape=as.factor(Year)), size=4)+
  scale_shape_manual(values=c(1, 8), name="Model") + 
  geom_path() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  labs(x="P/B", y="")

####QB
p3<-ggplot(EcopathInputs, aes(x=QB, y=FunctionalGroup)) + 
  geom_point(aes(shape=as.factor(Year)), size=4)+
  scale_shape_manual(values=c(1, 8), name="Model") + 
  geom_path() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  labs(x="Q/B", y="")

##comm catch
p4<-ggplot(EcopathInputs, aes(x=Comm.Catch, y=FunctionalGroup)) + 
  geom_point(aes(shape=as.factor(Year)), size=4)+
  scale_shape_manual(values=c(1, 8), name="Model") + 
  geom_path() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  labs(x=bquote('Commercial Landings'~(g/m^2)), y="")

##Rec catch
p5<-ggplot(EcopathInputs, aes(x=Rec.Catch, y=FunctionalGroup)) + 
  geom_point(aes(shape=as.factor(Year)), size=4)+
  scale_shape_manual(values=c(1, 8), name="Model") + 
  geom_path() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  labs(x=bquote('Recreational Landings'~(g/m^2)), y="")

p6<-ggplot(EcopathInputs, aes(x=EE, y=FunctionalGroup)) + 
  geom_point(aes(shape=as.factor(Year)), size=4)+
  scale_shape_manual(values=c(1, 8), name="Model") + 
  geom_path() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  labs(x="EE", y="")


ggarrange(p1,p2,p3,p6,p4,p5, nrow=3,ncol = 2, common.legend = TRUE, legend="bottom")














##old plots

p1<-ggplot(EcopathInputs, aes(x=FunctionalGroup, y=PB, fill=factor(Year))) + 
  labs(x="", y="P/B") +
  geom_bar(stat="identity", position="dodge2") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Year", labels=c("1990s", "Modern")) +
  coord_flip()

p2<-ggplot(EcopathInputs, aes(x=FunctionalGroup, y=QB, fill=factor(Year))) + 
  labs(x="", y="Q/B") +
  geom_bar(stat="identity", position="dodge2") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Year", labels=c("1990s", "Modern")) + coord_flip()

p3<-ggplot(EcopathInputs, aes(x=FunctionalGroup, y=Biomass, fill=factor(Year))) + 
  labs(x="", y="Biomass") +
  geom_bar(stat="identity", position="dodge2") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Year", labels=c("1990s", "Modern")) +coord_flip()


grid.arrange(p3, p1, p2, nrow=3)




ProjectionComparison<-read.csv("~/Desktop/URI/DATA/Projection_Comparison.csv")

ggplot(ProjectionComparison, aes(x=FunctionalGroup, y=Biomass, fill=Year)) + 
  geom_bar(stat="identity", position="dodge2")+ 
  labs(x="")+
  scale_fill_discrete(labels=c("Modern Model", "2018 Projection"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +coord_flip()




##fishing plots
RecFishing<-read.csv("~/Desktop/URI/DATA/RecLandings.csv")
p1<-ggplot(RecFishing, aes(x=Model, y=Landings, fill=FunctionalGroup))+geom_bar(stat="identity") +
  scale_fill_viridis_d(name = "Functional Group") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Recreational Landings")


CommFishing<-read.csv("~/Desktop/URI/DATA/CommLandings.csv")
p2<-ggplot(CommFishing, aes(x=Model, y=Landings, fill=FunctionalGroup))+geom_bar(stat="identity") +
  scale_fill_viridis_d(name = "Functional Group") +
  labs(y="Commercial Landings") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(p1, p2, nrow=1)





#######THroughput. scale is too different for this plot
Throughput<-read.csv("~/Desktop/URI/DATA/Throughput.csv")


ggplot(Throughput, aes(x=Trophic.Level, y=Throughput, fill=as.factor(Model))) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_viridis_d(name = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

