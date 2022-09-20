### Nils jore

### Utfordrning 2.3

library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(plyr)



Sys.setlocale(locale="no_NO")
setwd("/Users/nils/Downloads")
#Leser inn dataen
union <- read_csv("union_unempl.csv")
# Rydder i datasettet
union$country <- gsub("United Kingdom", "UK", union$country)
# Rydder i datasettet

names(union)[names(union) == "country"] <- "region"

#Henter kartdata
mapdata <- map_data("world")
#Slår sammen datasettene
mapdata <- left_join(mapdata, union, by = "region")

# filtrerer for Europa 
europa_df <- mapdata %>% 
  filter(!is.na(mapdata$unempl))

# Kart over arbeidsledighetsrate:
Unempl <- ggplot(europa_df, aes(x =long, y = lat, group = group)) +
  geom_polygon(aes(fill = unempl), color = "black") +
  scale_fill_gradient(name = " % unemployed", low = "grey", high = "pink", na.value = "white") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Arbeidsledighetsrate i Europa")
Unempl

#2. Lag kart over Europa som viser 1) fagforeningsdensitet, 2) “Excess coverage”, 
#og 3) Koordinering av lønnsfastsettelse.

# 1. Fagforeningsdensitet
# Nytt kart med variablen "Density" 
density <- ggplot(europa_df, aes(x =long, y = lat, group = group)) +
  geom_polygon(aes(fill = density), color = "black") +
  scale_fill_gradient(name = "Union density in %", low = "grey", high = "pink", na.value = "white") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Fagforeningsdensitet i Europa")
density


# 2. Excess coverage
# Lager ny variabel "excov" 

europa_df$excov <- europa_df$coverage - europa_df$density

Excess <- ggplot(europa_df, aes(x =long, y = lat, group = group)) +
  geom_polygon(aes(fill = excov), color = "black") +
  scale_fill_gradient(name = "Excess_coverage in %", low = "grey", high = "pink", na.value = "white") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Excess Coverage i Europa")
Excess

# 3. Koordinering av lønnsfastsettelse

Coordination <- ggplot(europa_df, aes(x =long, y = lat, group = group)) +
  geom_polygon(aes(fill = coord), color = "black") +
  scale_fill_brewer(name="Coordination level", palette = "Set3") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Koordinering av lønnsfastsettelse i Europa")
Coordination




