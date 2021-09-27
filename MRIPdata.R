library(tidyverse)
library(readxl)
options(scipen=999)
catch_19943<-read_xls("~/Desktop/MRIP/1994-Catch/catch_19943.xls")
catch_19943<-select(catch_19943, "common", "AREA_X", "MODE_FX", "ID_CODE", "ST", "SUB_REG", "RELEASE", "HARVEST")
catch_19944<-read_xls("~/Desktop/MRIP/1994-Catch/catch_19944.xls")
catch_19944<-select(catch_19944, "common", "AREA_X", "MODE_FX", "ID_CODE", "ST", "SUB_REG", "RELEASE", "HARVEST")


catchtotal<-rbind(catch_19943, catch_19944)



catchtotal$common[is.na(catchtotal$common)] <- "NONE"

catchtotal$StripedBass <- ifelse(catchtotal$common =="STRIPED BASS", "1", "0")
catchtotal$Bluefish <- ifelse(catchtotal$common =="BLUEFISH", "1", "0")
catchtotal$SummerFlounder <- ifelse(catchtotal$common =="SUMMER FLOUNDER", "1", "0")
catchtotal$PiscFish<-ifelse(catchtotal$StripedBass==1 | catchtotal$Bluefish==1 | catchtotal$SummerFlounder==1, "1", "0")




catch_19943<-catch_19943 %>%
  filter(common == "STRIPED BASS" | common == "SUMMER FLOUNDER" | common == "BLUEFISH" | common == "NONE")

catch_19943<-catch_19943 %>%
  group_by(ID_CODE, ) %>%
  summarise(summedcatch = sum(HARVEST))
