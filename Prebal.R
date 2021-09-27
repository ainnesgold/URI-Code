library(tidyverse)
library(gridExtra)
library(viridis)
library(ggpubr)


EcopathInputs<-read.csv("~/Desktop/URI/DATA/EcopathInputs.csv")


#library(ggplotify)
#library(grid)

##I can plot these with log axis as regular barplots.
par(mai=c(1.6,0.82,0.82,0.42))
  
  
barplot(EcopathInputs$X1994Biomass, log="y", 
            names.arg = EcopathInputs$FG, las=2, ylab="Biomass", main="1994")

barplot(EcopathInputs$X1994PB, log="y", 
                      names.arg = EcopathInputs$FG, las=2, ylab="P/B", main="1994")

barplot(EcopathInputs$X1994QB, log="y", 
            names.arg = EcopathInputs$FG, las=2, ylab="Q/B", main="1994")

barplot(EcopathInputs$X1994RB, log="y", 
        names.arg = EcopathInputs$FG, las=2, ylab="R/B", main="1994")

barplot(EcopathInputs$X2018Biomass, log="y", names.arg = EcopathInputs$FG, las=2, ylab="Biomass", main="2018")

barplot(EcopathInputs$X2018PB, log="y", names.arg = EcopathInputs$FG, las=2, ylab="P/B", main="2018")

barplot(EcopathInputs$X2018QB, log="y", names.arg = EcopathInputs$FG, las=2, ylab="Q/B", main="2018")

barplot(EcopathInputs$X2018RB, log="y", names.arg = EcopathInputs$FG, las=2, ylab="R/B", main="2018")

#grid.arrange(p1, p2, p3, nrow=3) doesn't work




##THIS IS THE WEIRD PART Trying to do ggplot with log scale

VitalRates<-gather(EcopathInputs, key=Type, value=Rate, -FG)


VitalRates<-VitalRates %>%
  filter(Type!="X1994TrophicLevel") %>%
  filter(Type!="X2018TrophicLevel") %>%
  filter(Type!="X1994Biomass") %>%
  filter(Type!="X2018Biomass") %>%
  filter(Type!="X1994PQ") %>%
  filter(Type!="X2018PQ") %>%
  filter(Type!="X1994PR") %>%
  filter(Type!="X2018PR") %>%
  filter(Type!="X1994PBreltoPP") %>%
  filter(Type!="X2018PBreltoPP") %>%
  filter(Type!="X1994BiomassreltoPP") %>%
  filter(Type!="X2018BiomassreltoPP")

VitalRates1994<-VitalRates %>%
  filter(Type!="X2018PB") %>%
  filter(Type!="X2018QB") %>%
  filter(Type!="X2018RB")

VitalRates1994$FG <- factor(VitalRates1994$FG,levels = c("Detritus", "Benthic Algae", "Phytoplankton", 
                                                         "Zooplankton", "Deposit Feeding Benthos", 
                                                         "Suspension Feeding Benthos", "Cultured Shellfish",
                                                         "Carnivorous Benthos", "Gelatinous Zooplankton",
                                                         "Planktivorous Fish",
                                                         "Benthivorous Fish", "Small Squid", 
                                                         "Large Squid", "Seabirds",
                                                         "Piscivorous Fish"))

##bars appear starting at 1 instead of 0.01
pointrates94<-ggplot(data=VitalRates1994, aes(x=FG, y=Rate, shape=Type, color=Type)) + geom_point()+
  scale_y_log10() + labs(x="", y="Rate") +
  scale_color_discrete(labels=c("P/B", "Q/B", "R/B")) + 
  scale_shape_discrete(labels=c("P/B", "Q/B", "R/B")) +
  ggtitle("") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  coord_flip()


##adding 1
VitalRates1994$Ratesplus1<-VitalRates1994$Rate + 1

Rates94<-ggplot(data=VitalRates1994, aes(x=FG, y=Ratesplus1, fill=Type)) + geom_bar(stat="identity", position="dodge")+
  scale_y_log10() + labs(x="", y="Rate + 1") +
  scale_fill_viridis_d(labels=c("P/B", "Q/B", "R/B")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) + coord_flip()


EcopathInputs$FG <- factor(EcopathInputs$FG,levels = c("Detritus", "Benthic Algae", "Phytoplankton", 
                                                       "Zooplankton", "Deposit Feeding Benthos", 
                                                       "Suspension Feeding Benthos", "Cultured Shellfish",
                                                       "Carnivorous Benthos", "Gelatinous Zooplankton",
                                                       "Planktivorous Fish",
                                                       "Benthivorous Fish", "Small Squid", 
                                                       "Large Squid", "Seabirds",
                                                       "Piscivorous Fish"))



pointsbio94<-ggplot(data=EcopathInputs, aes(x=FG, y=X1994Biomass)) + geom_point()+
  scale_y_log10(labels = scales::number_format(accuracy = 0.01))+
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5), plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  ggtitle("1994 Model") + labs(x="", y="Biomass") + coord_flip()





EcopathInputs$X1994Biomassplus1<-EcopathInputs$X1994Biomass + 1

Bio94<-ggplot(data=EcopathInputs, aes(x=FG, y=X1994Biomassplus1)) + geom_bar(stat="identity", fill="midnightblue")+scale_y_log10()+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("1994 Model") + labs(x="", y="Biomass") + coord_flip()



VitalRates2018<-VitalRates %>%
  filter(Type!="X1994PB") %>%
  filter(Type!="X1994QB") %>%
  filter(Type!="X1994RB")

VitalRates2018$FG <- factor(VitalRates2018$FG,levels =  c("Detritus", "Benthic Algae", "Phytoplankton", 
                                                          "Zooplankton", "Deposit Feeding Benthos", 
                                                          "Suspension Feeding Benthos", "Cultured Shellfish",
                                                          "Carnivorous Benthos", "Gelatinous Zooplankton",
                                                          "Planktivorous Fish",
                                                          "Benthivorous Fish", "Small Squid", 
                                                          "Large Squid", "Seabirds",
                                                          "Piscivorous Fish"))

pointrates18<-ggplot(data=VitalRates2018, aes(x=FG, y=Rate, shape=Type, color=Type)) + 
  geom_point()+
  scale_y_log10() + labs(x="", y="Rate") +
  scale_color_discrete(labels=c("P/B", "Q/B", "R/B")) + 
  scale_shape_discrete(labels=c("P/B", "Q/B", "R/B")) +
  ggtitle("") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  coord_flip()




VitalRates2018$Ratesplus1<-VitalRates2018$Rate + 1

Rates18<-ggplot(data=VitalRates2018, aes(x=FG, y=Ratesplus1, fill=Type)) + geom_bar(stat="identity", position="dodge")+
  scale_y_log10() + labs(x="", y="Rate + 1") +
  scale_fill_viridis_d(labels=c("P/B", "Q/B", "R/B")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) + coord_flip()


pointsbio18<-ggplot(data=EcopathInputs, aes(x=FG, y=X2018Biomass)) + geom_point()+
  scale_y_log10(labels = scales::number_format(accuracy = 0.01))+
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5), plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  ggtitle("2018 Model") + labs(x="", y="Biomass") + coord_flip()


EcopathInputs$X2018Biomassplus1<-EcopathInputs$X2018Biomass + 1

Bio18<-ggplot(data=EcopathInputs, aes(x=FG, y=X2018Biomassplus1)) + geom_bar(stat="identity", fill="midnightblue")+scale_y_log10()+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("2018 Model") + labs(x="", y="Biomass") + coord_flip()


ggarrange(pointsbio94, pointsbio18, pointrates94, pointrates18, nrow=2, ncol = 2,
          common.legend = TRUE, legend = "bottom")

ggarrange(Bio94, Bio18, Rates94, Rates18, nrow=2,ncol = 2, common.legend = TRUE, legend="bottom")











#other stuff
##this stuff works
##PQ and PR plot
EcopathInputs<-read.csv("~/Desktop/URI/DATA/EcopathInputs.csv")

VitalRates<-gather(EcopathInputs, key=Type, value=Rate, -FG)

PQPR1994<-VitalRates %>%
  filter(Type!="X1994PB") %>%
  filter(Type!="X1994QB") %>%
  filter(Type!="X1994RB") %>%
  filter(Type!="X1994PB") %>%
  filter(Type!="X2018PB") %>%
  filter(Type!="X2018QB") %>%
  filter(Type!="X2018RB") %>%
  filter(Type!="X2018PB") %>%
  filter(Type!="X1994TrophicLevel") %>%
  filter(Type!="X2018TrophicLevel") %>%
  filter(Type!="X1994Biomass") %>%
  filter(Type!="X2018Biomass") %>%
  filter(Type!="X2018PQ") %>%
  filter(Type!="X2018PR") %>%
  filter(Type!="X1994PBreltoPP") %>%
  filter(Type!="X2018PBreltoPP") %>%
  filter(Type!="X1994BiomassreltoPP") %>%
  filter(Type!="X2018BiomassreltoPP")

PQPR1994$FG <- factor(PQPR1994$FG,levels =  c("Detritus", "Benthic Algae", "Phytoplankton", 
                                              "Zooplankton", "Deposit Feeding Benthos", 
                                              "Suspension Feeding Benthos", "Cultured Shellfish",
                                              "Carnivorous Benthos", "Gelatinous Zooplankton",
                                              "Planktivorous Fish",
                                              "Benthivorous Fish", "Small Squid", 
                                              "Large Squid", "Seabirds",
                                              "Piscivorous Fish"))

p1<-ggplot(PQPR1994, aes(x=FG, y=Rate, fill=Type)) + geom_bar(stat="identity", position = "dodge") +
  geom_hline(yintercept=1, linetype="dashed", color = "red") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name = "", labels=c("P/Q", "P/R")) +
  labs(y="", x="")+
  ggtitle("1994") + coord_flip()

PQPR2018<-VitalRates %>%
  filter(Type!="X1994PB") %>%
  filter(Type!="X1994QB") %>%
  filter(Type!="X1994RB") %>%
  filter(Type!="X1994PB") %>%
  filter(Type!="X2018PB") %>%
  filter(Type!="X2018QB") %>%
  filter(Type!="X2018RB") %>%
  filter(Type!="X2018PB") %>%
  filter(Type!="X1994TrophicLevel") %>%
  filter(Type!="X2018TrophicLevel") %>%
  filter(Type!="X1994Biomass") %>%
  filter(Type!="X2018Biomass") %>%
  filter(Type!="X1994PQ") %>%
  filter(Type!="X1994PR") %>%
  filter(Type!="X1994PBreltoPP") %>%
  filter(Type!="X2018PBreltoPP") %>%
  filter(Type!="X1994BiomassreltoPP") %>%
  filter(Type!="X2018BiomassreltoPP")

PQPR2018$FG <- factor(PQPR2018$FG,levels = c("Detritus", "Benthic Algae", "Phytoplankton", 
                                             "Zooplankton", "Deposit Feeding Benthos", 
                                             "Suspension Feeding Benthos", "Cultured Shellfish",
                                             "Carnivorous Benthos", "Gelatinous Zooplankton",
                                             "Planktivorous Fish",
                                             "Benthivorous Fish", "Small Squid", 
                                             "Large Squid", "Seabirds",
                                             "Piscivorous Fish"))

p2<-ggplot(PQPR2018, aes(x=FG, y=Rate, fill=Type)) + geom_bar(stat="identity", position = "dodge") +
  geom_hline(yintercept=1, linetype="dashed", color = "red") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name = "", labels=c("P/Q", "P/R")) +
  labs(y="", x="")+
  ggtitle("2018") +
  coord_flip()

ggarrange(p1,p2, nrow=1,ncol = 2, common.legend = TRUE, legend="right")


  
  
  
