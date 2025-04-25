library(ggplot2)
dir <- setwd("C:/Users/Emily/Documents/EDI data")
height <- read.csv("T7_warmx_soca_height_harvest_L1.csv")
gall <- read.csv("T7_warmx_Soca_galls_L1.csv")
biomass <- read.csv("T7_warmx_soca_biomass_L1.csv")

clean <- merge(height, gall, by = c("Unique_ID", "Treatment", "Rep", "Footprint", "Subplot", "Climate_Treatment", "Year"))
clean <- merge(clean, biomass, by = c("Unique_ID", "Treatment", "Rep", "Footprint", "Subplot", "Climate_Treatment", "Year"))

ggplot(clean, aes(x=Height_cm, y= Dried_Weight)) +
  geom_point(size=I(4)) + 
  geom_smooth(method = lm, se = FALSE)

ggplot(clean, aes(x=Biomass, y= Dried_Weight)) +
  geom_point(size=I(4)) + 
  geom_smooth(method = lm, se = FALSE)

height_lm <- lm(Dried_Weight ~ Height_cm, data = clean)
summary(height_lm)

biomass_lm <- lm(Dried_Weight ~ Biomass, data = clean)
summary(biomass_lm)
