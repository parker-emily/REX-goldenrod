# Clear all existing data
rm(list=ls())

library(ggplot2)
library(ggpubr)
library(tidyverse)

# Set working directory from .Renviron
dir <- Sys.getenv("DATA_DIR")
list.files(dir)

height <- read.csv(file.path(dir, "/T7_warmx_plant_traits/L1/T7_warmx_soca_height_harvest_L1.csv"))
gall <- read.csv(file.path(dir, "/T7_warmx_insect/L1/T7_warmx_Soca_galls_L1.csv"))
biomass <- read.csv(file.path(dir, "/T7_warmx_plant_traits/L1/T7_warmx_soca_biomass_L1.csv"))
chmb_vol <- read.csv(file.path(dir, "/T7_warmx_insect/L1/T7_warmx_Soca_gall_chmb_vol_L1.csv"))
chmb_count <- read.csv(file.path(dir, "/T7_warmx_insect/L1/T7_warmx_Soca_gall_chmb_count_L1.csv"))

# Emily's
dir <- setwd("C:/Users/parkere5/Documents/Goldenrod Data")
height <- read.csv("T7_warmx_soca_height_harvest_L1.csv")
gall <- read.csv("T7_warmx_Soca_galls_L1.csv")
biomass <- read.csv("T7_warmx_soca_biomass_L1.csv")
chmb_vol <- read.csv("T7_warmx_Soca_gall_chmb_vol_L1.csv")
chmb_count <- read.csv("T7_warmx_Soca_gall_chmb_count_L1.csv")

#rename chamber columns
chmb_vol <- chmb_vol %>% 
  rename("Unique_ID" = "unique_plant_number",
         "Rep" = "rep",
         "Footprint" = "footprint",
         "Climate_Treatment" = "treatment")

chmb_count <- chmb_count %>% 
  rename("Unique_ID" = "unique_plant_number",
         "Rep" = "rep",
         "Footprint" = "footprint",
         "Climate_Treatment" = "treatment")


#merge gall characters
gall <- merge(gall, chmb_count, by = c("Unique_ID", "Rep", "Footprint", "Climate_Treatment"))
gall <- merge(gall, chmb_vol, by = c("Unique_ID", "Rep", "Footprint", "Climate_Treatment"))


clean <- merge(height, gall, by = c("Unique_ID", "Treatment", "Rep", "Footprint", "Subplot", "Climate_Treatment", "Year"))
clean <- clean %>%
  merge(biomass, by = c("Unique_ID", "Treatment", "Rep", "Footprint", "Subplot", "Climate_Treatment", "Year", "Galling_Status")) %>%
  select(-Harvest_Date.y) %>% #remove duplicate harvest date column 
  filter(Climate_Treatment != "Irrigated Control")

height_plot1 <- 
  ggplot(clean, aes(x=Height_cm, y= Dried_Weight)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem height (cm)", y = "Dried gall biomass (g)") +
  annotate("text", x = 35, y=2, label = "A", size=5) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))
height_plot1

# same graph as above but color by treatment
height_plot2 <- 
        ggplot(clean, aes(x=Height_cm, y= Dried_Weight, fill = Climate_Treatment)) +
        geom_point(shape = 21, size = 3) + 
        geom_smooth(method = lm, se = FALSE, aes(color = Climate_Treatment)) +
        labs(x="Stem height (cm)", y = "Dried gall biomass (g)") +
        annotate("text", x = 35, y=2, label = "A", size=5) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 11),
              axis.text.y = element_text(size = 14),
              axis.title = element_text(size=14,face="bold"))
height_plot2

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
height_treat

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

height_vol <-
  ggplot(clean, aes(x=Height_cm, y= chamber_volume_mm3)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem height (cm)", y = "Chamber volume (mm³)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))

height_count <-
  ggplot(clean, aes(x=Height_cm, y= num_of_chambers)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem height (cm)", y = "Number of chambers") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))

bm_plot1 <- 
  ggplot(clean, aes(x=Biomass, y= Dried_Weight)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color="black") + 
  labs(x = "Stem biomass (g)", y = "Dried gall biomass (g)") +
  annotate("text", x = 1.5, y=2, label = "B", size=5) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))
bm_plot1

# same plot as above but points are colored by climate treatment
bm_plot2 <- 
        ggplot(clean, aes(x=Biomass, y= Dried_Weight, fill = Climate_Treatment)) +
        geom_point(shape = 21, size = 3) + 
        geom_smooth(method = lm, se = FALSE, aes(color = Climate_Treatment)) + 
        labs(x = "Stem biomass (g)", y = "Dried gall biomass (g)") +
        annotate("text", x = 1.5, y=2, label = "B", size=5) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 11),
              axis.text.y = element_text(size = 14),
              axis.title = element_text(size=14,face="bold"))
bm_plot2

bm_treat <-
ggplot(clean, aes(x=Biomass, y= Dried_Weight)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem biomass (g)", y = "Dried gall biomass (g)") +
  facet_wrap(~Climate_Treatment) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))

bm_year <-
  ggplot(clean, aes(x=Biomass, y= Dried_Weight)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem biomass (g)", y = "Dried gall biomass (g)") +
  facet_wrap(~Year) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))

bm_vol <-
  ggplot(clean, aes(x=Biomass, y= chamber_volume_mm3)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem biomass (g)", y = "Chamber volume (mm³)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))

bm_count <-
  ggplot(clean, aes(x=Biomass, y= num_of_chambers)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem biomass (g)", y = "Number of chambers") +
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

png("gall_regression_vol.png", units="in", width=11, height=5, res=300)
ggarrange(height_vol, bm_vol,
          nrow = 1, common.legend = T, legend="right",widths = c(1, 1))
dev.off()

png("gall_regression_count.png", units="in", width=11, height=5, res=300)
ggarrange(height_count, bm_count,
          nrow = 1, common.legend = T, legend="right",widths = c(1, 1))
dev.off()

##stats
# height
height_lm_1 <- lm(Dried_Weight ~ Height_cm, data = clean)
summary(height_lm_1)

height_lm_2 <- lm(Dried_Weight ~ Height_cm * Climate_Treatment, data = clean)
summary(height_lm_2)

# biomass
biomass_lm_1 <- lm(Dried_Weight ~ Biomass, data = clean)
summary(biomass_lm_1)

biomass_lm_2 <- lm(Dried_Weight ~ Biomass * Climate_Treatment, data = clean)
summary(biomass_lm_2)
