library(tidyverse)
library(gridExtra)
library(viridis)
library(ggpubr)

keeperboat <- function(x){0.04 * (x ^ 0.379488225)}

a<-ggplot() +
  scale_x_continuous(name="Keeper Abundance", limits=c(0,5000)) +
  labs(y="Proportion of Trips Successful") +
  stat_function(fun = keeperboat, aes(linetype="Power")) +
  geom_abline(aes(intercept=0,slope=0.000202678, linetype="Linear"), lwd=1) +
  geom_abline(aes(intercept=0.39638425, slope = 0, linetype="Static")) +
  scale_linetype_manual(name="Relationship",values=c(Power=2,Linear=3, Static=1)) +
  ggtitle("Keeper / Boat") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

keepershore <- function(x){0.02 * (x ^ 0.229636797)}

b<-ggplot() +
  scale_x_continuous(name="Keeper Abundance", limits=c(0,5000)) +
  labs(y="") +
  stat_function(fun = keepershore, aes(linetype="Power")) +
  geom_abline(aes(intercept=0,slope=2.828E-05, linetype="Linear"), lwd=1) +
  geom_abline(aes(intercept=0.07171281, slope = 0, linetype="Static")) +
  scale_linetype_manual(name="Relationship",values=c(Power=2,Linear=3, Static=1)) +
  ggtitle("Keeper / Shore") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

smallboat <- function(x){0.08 * (x ^ 0.258588565)}

c<-ggplot() +
  scale_x_continuous(name="Sublegal Fish Abundance", limits=c(0,10000)) +
  labs(y="Proportion of Trips Successful") +
  stat_function(fun = smallboat, aes(linetype="Power")) +
  geom_abline(aes(intercept=0,slope=8.65853E-05, linetype="Linear"), lwd=1) +
  geom_abline(aes(intercept=0.66510712, slope = 0, linetype="Static")) +
  scale_linetype_manual(name="Relationship",values=c(Power=2,Linear=3, Static=1)) +
  ggtitle("Sublegal / Boat") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5)) +
  ylim(0,1)

smallshore <- function(x){0.04 * (x ^ 0.251086013)}

d<-ggplot() +
  scale_x_continuous(name="Sublegal Fish Abundance", limits=c(0,10000)) +
  labs(y="") +
  stat_function(fun = smallshore, aes(linetype="Power")) +
  geom_abline(aes(intercept=0,slope=4.04021E-05, linetype="Linear"), lwd=1) +
  geom_abline(aes(intercept=0.33776653, slope = 0, linetype="Static")) +
  scale_linetype_manual(name="Relationship",values=c(Power=2,Linear=3, Static=1)) +
  ggtitle("Sublegal / Shore") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(hjust = 0.5))+
  ylim(0,1)

ggarrange(a,b,c,d, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

