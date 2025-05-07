library(ggplot2)
dir <- setwd("C:/Users/Emily/Documents/EDI data")
height <- read.csv("T7_warmx_soca_height_harvest_L1.csv")
gall <- read.csv("T7_warmx_Soca_galls_L1.csv")
biomass <- read.csv("T7_warmx_soca_biomass_L1.csv")

clean <- merge(height, gall, by = c("Unique_ID", "Treatment", "Rep", "Footprint", "Subplot", "Climate_Treatment", "Year"))
clean <- clean %>%
  merge(biomass, by = c("Unique_ID", "Treatment", "Rep", "Footprint", "Subplot", "Climate_Treatment", "Year", "Galling_Status")) %>%
  select(-Harvest_Date.y)

height_plot <- ggplot(clean, aes(x=Height_cm, y= Dried_Weight)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  labs(x="Stem height (cm)", y = "Dried gall biomass (g)")

bm_plot <- ggplot(clean, aes(x=Biomass, y= Dried_Weight)) +
  geom_point(shape = 21, size = 3, color = "black",fill = "purple4") + 
  geom_smooth(method = lm, se = FALSE, color="black") + 
  labs(x = "Stem biomass (g)", y = "Dried gall biomass (g)")

#export png
png("gall_regression.png", units="in", width=11, height=5, res=300)
ggarrange(height_plot, bm_plot,
          nrow = 1, common.legend = T, legend="right",widths = c(1, 1))
dev.off()

height_lm <- lm(Dried_Weight ~ Height_cm, data = clean)
summary(height_lm)

biomass_lm <- lm(Dried_Weight ~ Biomass, data = clean)
summary(biomass_lm)
