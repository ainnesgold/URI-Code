##creating logistic growth curves for probability of capturing fish for my ABM

##shore keepers
ps<-read.csv("~/Desktop/URI/DATA/logistic/Keeper_Shore.csv")

L_est1 = 30 #taken from a visual estimate of where the max value would be
x0_est1 = 1000 #taken from the visual estimate of where all the points come together
k_est1 = 0.003 #a completely arbitrary guess starting point. 
#I started at k=1 but the S gets more pronouced and curvier at smaller numbers
#this seemed like a good place to start
fish1 = seq(from=0, to = 4000, by = 20)
PerCatch_est1 = L_est1 / (1 + exp(-k_est1*(fish1-x0_est1)))

plot(PerCatch_est1~fish1, xlab="Fish (keeper) Population", ylab="Probability of a successful trip")

plot(ps$Keeper_PercentSuccessful ~ ps$NumberKeepers , main='Shore/Keeper % Success', ylim=c(0, 70),
     xlab="Number of Keepers", ylab="% Successful") +
  points(PerCatch_est1~fish1, col=3, pch=16, type='b')

##boat keepers

pb<-read.csv("~/Desktop/URI/DATA/logistic/Keeper_Boat.csv")

L_est2 = 35 #taken from a visual estimate of where the max value would be
x0_est2 = 1000 #taken from the visual estimate of where all the points come together
k_est2 = 0.003 #a completely arbitrary guess starting point. 
#I started at k=1 but the S gets more pronouced and curvier at smaller numbers
#this seemed like a good place to start
fish2 = seq(from=0, to = 4000, by = 20)
PerCatch_est2 = L_est2 / (1 + exp(-k_est2*(fish2-x0_est2)))

plot(pb$Keeper_PercentSuccessful ~ pb$NumberKeepers , main='Boat/Keeper % Success', xlab="Number of Keepers",
     ylab="% Successful", ylim=c(0, 70)) +
  points(PerCatch_est2~fish2, col=3, pch=16, type='b')



##shore small

small_ps<-read.csv("~/Desktop/URI/DATA/logistic/Small_Shore.csv")

L_est3 = 50 #taken from a visual estimate of where the max value would be
x0_est3 = 4000 #taken from the visual estimate of where all the points come together
k_est3 = 0.001 #a completely arbitrary guess starting point. 
#I started at k=1 but the S gets more pronouced and curvier at smaller numbers
#this seemed like a good place to start
fish3 = seq(from=0, to = 15000, by = 20)
PerCatch_est3 = L_est3 / (1 + exp(-k_est3*(fish3-x0_est3)))

plot(small_ps$Small_PercentSuccessful ~ small_ps$NumberSmall , main='Shore/Small % Success', 
         xlab="Number of Small Fish", ylab="% Successful", ylim=c(0, 80), xlim=c(0, 17000)) +
  points(PerCatch_est3~fish3, col=3, pch=16, type='b')

##boat small

small_pb<-read.csv("~/Desktop/URI/DATA/logistic/Small_Boat.csv")

L_est4 = 60 #taken from a visual estimate of where the max value would be
x0_est4 = 4000 #taken from the visual estimate of where all the points come together
k_est4 = 0.001 #a completely arbitrary guess starting point. 
#I started at k=1 but the S gets more pronouced and curvier at smaller numbers
#this seemed like a good place to start
fish4 = seq(from=0, to = 15000, by = 20)
PerCatch_est4 = L_est4 / (1 + exp(-k_est4*(fish4-x0_est4)))

plot(small_pb$Small_PercentSuccessful ~ small_pb$NumberSmall , main='Boat/Small % Success',
         ylab="% Successful", xlab="Number of Small Fish", ylim=c(0, 100)) +
  points(PerCatch_est4~fish4, col=3, pch=16, type='b')





#plots together
par(mfrow=c(2,2))
plot(ps$Keeper_PercentSuccessful ~ ps$NumberKeepers , main='Shore/Keeper % Success', ylim=c(0, 70),
     xlab="Number of Keepers", ylab="% Successful") +
  points(PerCatch_est1~fish1, col=3, pch=16, type='b')

plot(pb$Keeper_PercentSuccessful ~ pb$NumberKeepers , main='Boat/Keeper % Success', xlab="Number of Keepers",
     ylab="% Successful", ylim=c(0, 70)) +
  points(PerCatch_est2~fish2, col=3, pch=16, type='b')

plot(small_ps$Small_PercentSuccessful ~ small_ps$NumberSmall , main='Shore/Small % Success', 
     xlab="Number of Small Fish", ylab="% Successful", ylim=c(0, 80), xlim=c(0, 20000)) +
  points(PerCatch_est3~fish3, col=3, pch=16, type='b')

plot(small_pb$Small_PercentSuccessful ~ small_pb$NumberSmall , main='Boat/Small % Success',
     ylab="% Successful", xlab="Number of Small Fish", ylim=c(0, 100)) +
  points(PerCatch_est4~fish4, col=3, pch=16, type='b')












