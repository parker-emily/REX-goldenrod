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

#merge gall characters - don't need
#gall <- left_join(gall, chmb_count, by = c("Unique_ID", "Rep", "Footprint", "Climate_Treatment"))
#gall <- left_join(gall, chmb_vol, by = c("Unique_ID", "Rep", "Footprint", "Climate_Treatment")) ## merging this adds duplicates - probably don't need to do this


clean <- merge(height, gall, by = c("Unique_ID", "Treatment", "Rep", "Footprint", "Subplot", "Climate_Treatment", "Year"))
clean <- clean %>%
  merge(biomass, by = c("Unique_ID", "Treatment", "Rep", "Footprint", "Subplot", "Climate_Treatment", "Year", "Galling_Status")) %>%
  #select(-Harvest_Date.y) %>% #remove duplicate harvest date column 
  filter(Climate_Treatment != "Irrigated Control") %>%
  filter(Galling_Status != "Non-galled") #removes non-galled plants

#factor year for graphing
clean$Year <- levels(as.factor(clean$Year))

#plots
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

height_plot3 <- 
  ggplot(clean, aes(x=Height_cm, y= Dried_Weight, fill = Year)) +
  geom_point(shape = 21, size = 3) + 
  geom_smooth(method = lm, se = FALSE, aes(color = Year)) +
  labs(x="Stem height (cm)", y = "Dried gall biomass (g)") +
  annotate("text", x = 35, y=2, label = "A", size=5) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))
height_plot3


# height_vol <-
#   ggplot(clean, aes(x=Height_cm, y= chamber_volume_mm3)) +
#   geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
#   geom_smooth(method = lm, se = FALSE, color = "black") +
#   labs(x="Stem height (cm)", y = "Chamber volume (mm³)") +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 11),
#         axis.text.y = element_text(size = 14),
#         axis.title = element_text(size=14,face="bold"))

# height_count <-
#   ggplot(clean, aes(x=Height_cm, y= num_of_chambers)) +
#   geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
#   geom_smooth(method = lm, se = FALSE, color = "black") +
#   labs(x="Stem height (cm)", y = "Number of chambers") +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 11),
#         axis.text.y = element_text(size = 14),
#         axis.title = element_text(size=14,face="bold"))

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

bm_plot3 <- 
  ggplot(clean, aes(x=Biomass, y= Dried_Weight, fill = Year)) +
  geom_point(shape = 21, size = 3) + 
  geom_smooth(method = lm, se = FALSE, aes(color = Year)) + 
  labs(x = "Stem biomass (g)", y = "Dried gall biomass (g)") +
  annotate("text", x = 1.5, y=2, label = "B", size=5) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))
bm_plot3


# bm_vol <-
#   ggplot(clean, aes(x=Biomass, y= chamber_volume_mm3)) +
#   geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
#   geom_smooth(method = lm, se = FALSE, color = "black") +
#   labs(x="Stem biomass (g)", y = "Chamber volume (mm³)") +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 11),
#         axis.text.y = element_text(size = 14),
#         axis.title = element_text(size=14,face="bold"))

# bm_count <-
#   ggplot(clean, aes(x=Biomass, y= num_of_chambers)) +
#   geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
#   geom_smooth(method = lm, se = FALSE, color = "black") +
#   labs(x="Stem biomass (g)", y = "Number of chambers") +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 11),
#         axis.text.y = element_text(size = 14),
#         axis.title = element_text(size=14,face="bold"))


#export pngs
png("gall_regression1.png", units="in", width=11, height=5, res=300)
ggarrange(height_plot1, bm_plot1,
          nrow = 1, common.legend = T, legend="right",widths = c(1, 1))
dev.off()

png("gall_regression2.png", units="in", width=11, height=5, res=300)
ggarrange(height_plot2, bm_plot2,
          nrow = 1, common.legend = T, legend="right",widths = c(1, 1))
dev.off()

png("gall_regression3.png", units="in", width=11, height=5, res=300)
ggarrange(height_plot3, bm_plot3,
          nrow = 1, common.legend = T, legend="right",widths = c(1, 1))
dev.off()

# png("gall_regression_vol.png", units="in", width=11, height=5, res=300)
# ggarrange(height_vol, bm_vol,
#           nrow = 1, common.legend = T, legend="right",widths = c(1, 1))
# dev.off()

# png("gall_regression_count.png", units="in", width=11, height=5, res=300)
# ggarrange(height_count, bm_count,
#           nrow = 1, common.legend = T, legend="right",widths = c(1, 1))
# dev.off()

##stats
# height
height_lm_1 <- lm(Dried_Weight ~ Height_cm, data = clean)
anova(height_lm_1)
summary(height_lm_1)

height_lm_2 <- lm(Dried_Weight ~ Height_cm * Climate_Treatment, data = clean)
anova(height_lm_2)
summary(height_lm_2)


# biomass
biomass_lm_1 <- lm(Dried_Weight ~ Biomass, data = clean)
anova(biomass_lm_1)
summary(biomass_lm_1)

biomass_lm_2 <- lm(Dried_Weight ~ Biomass * Climate_Treatment, data = clean)
anova(biomass_lm_2)
summary(biomass_lm_2)

