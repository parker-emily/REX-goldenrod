# Clear all existing data
rm(list=ls())

library(ggplot2)
library(ggpubr)
library(tidyverse)


dir <- setwd("C:/Users/parkere5/Documents/Goldenrod Data")
height <- read.csv("T7_warmx_soca_height_harvest_L1.csv")
gall <- read.csv("T7_warmx_Soca_galls_L1.csv")
biomass <- read.csv("T7_warmx_soca_biomass_L1.csv")

clean <- merge(height, gall, by = c("Unique_ID", "Treatment", "Rep", "Footprint", "Subplot", "Climate_Treatment", "Year"))
clean <- clean %>%
  merge(biomass, by = c("Unique_ID", "Treatment", "Rep", "Footprint", "Subplot", "Climate_Treatment", "Year", "Galling_Status")) %>%
  select(-Harvest_Date.y) %>% #remove duplicate harvest date column 
  filter(Climate_Treatment != "Irrigated Control")

height_plot <- 
  ggplot(clean, aes(x=Height_cm, y= Dried_Weight)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem height (cm)", y = "Dried gall biomass (g)") +
  annotate("text", x = 35, y=2, label = "A", size=5) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))

height_treat <-
  ggplot(clean, aes(x=Height_cm, y= Dried_Weight)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem height (cm)", y = "Dried gall biomass (g)") +
  facet_wrap(~Climate_Treatment) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))

height_year <-
  ggplot(clean, aes(x=Height_cm, y= Dried_Weight)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem height (cm)", y = "Dried gall biomass (g)") +
  facet_wrap(~Year) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))


bm_plot <- 
  ggplot(clean, aes(x=Biomass, y= Dried_Weight)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color="black") + 
  labs(x = "Stem biomass (g)", y = "Dried gall biomass (g)") +
  annotate("text", x = 1.5, y=2, label = "B", size=5) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))

bm_treat <-
ggplot(clean, aes(x=Height_cm, y= Dried_Weight)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem biomass (g)", y = "Dried gall biomass (g)") +
  facet_wrap(~Climate_Treatment) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))

bm_year <-
  ggplot(clean, aes(x=Height_cm, y= Dried_Weight)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem biomass (g)", y = "Dried gall biomass (g)") +
  facet_wrap(~Year) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))


#export pngs
png("gall_regression.png", units="in", width=11, height=5, res=300)
ggarrange(height_plot, bm_plot,
          nrow = 1, common.legend = T, legend="right",widths = c(1, 1))
dev.off()

png("gall_regression_treatment.png", units="in", width=11, height=5, res=300)
ggarrange(height_treat, bm_treat,
          nrow = 1, common.legend = T, legend="right",widths = c(1, 1))
dev.off()

png("gall_regression_year.png", units="in", width=11, height=5, res=300)
ggarrange(height_year, bm_year,
          nrow = 1, common.legend = T, legend="right",widths = c(1, 1))
dev.off()



##stats
height_lm <- lm(Dried_Weight ~ Height_cm, data = clean)
summary(height_lm)

biomass_lm <- lm(Dried_Weight ~ Biomass, data = clean)
summary(biomass_lm)
