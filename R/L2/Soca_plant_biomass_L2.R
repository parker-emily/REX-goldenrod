# TITLE:          REX: Soca biomass analyses and plot
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Kara Dobson, Kristin Wolford, Emily Parker, Mark Hammond
# DATA INPUT:     Data imported as csv files from shared REX Google drive T7_warmx_plant_traits L1 folder
# DATA OUTPUT:    analyses and plots
# PROJECT:        REX
# DATE:           February 2025

# Clear all existing data
rm(list=ls())

# Load packages
library(lmerTest)
library(fitdistrplus)
library(sjPlot)
library(tidyverse)
library(car)
library(emmeans)
library(bbmle)
library(multcomp)
library(knitr)
library(ggpubr)

# Set working directory from .Renviron
dir <- Sys.getenv("DATA_DIR")
list.files(dir)

## Read in data
biomass <- read.csv(file.path(dir, "/T7_warmx_plant_traits/L1/T7_warmx_soca_biomass_L1.csv"))
biomass$Year <- as.factor(biomass$Year)

# remove irrigated control treatement for analysis
biomass <- biomass %>% filter(Climate_Treatment != "Irrigated Control")

#remove plants with incorrect galling status label
biomass <- biomass %>% filter(!(Unique_ID == "217" & Year == "2022" | Unique_ID == "232" & Year == "2022" | Unique_ID == "285" & Year == "2021"))

###### Data exploration #######
# The first thing we do is check the distribution of the raw data
# We're checking for a normal distribution; the distribution of the raw data
# doesn't matter as much as the distribution of a model's residuals (which we'll also check),
# but its a good way to get a glimpse of how the data look
descdist(biomass$Biomass, discrete = FALSE) # checks what type of distribution your data matches
hist(biomass$Biomass) # should be a bell curve if normal
qqnorm(biomass$Biomass) # should be a straight diagonal line if normal
shapiro.test(biomass$Biomass) # p > 0.05 if normal
# making a model to test distribution of model residuals
raw.data.test <- lm(Biomass ~ Climate_Treatment * Galling_Status, data=biomass) # testing model
hist(resid(raw.data.test)) # checking model residuals
shapiro.test(resid(raw.data.test)) # not normal residuals
# looks pretty good, going to try some transformations but might not need them

# log normal transformation
biomass$log_biomass <- log(biomass$Biomass)
descdist(biomass$log_biomass, discrete = FALSE)
hist(biomass$log_biomass)
qqnorm(biomass$log_biomass)
shapiro.test(biomass$log_biomass)
# making a model to test distribution of model residuals w/ log transformation
raw.data.test.log <- lm(log_biomass ~ Climate_Treatment * Galling_Status, data=biomass) # testing model
hist(resid(raw.data.test.log)) # checking model residuals
shapiro.test(resid(raw.data.test.log))

# square root transformation
biomass$sqrt_biomass <- sqrt(biomass$Biomass)
descdist(biomass$sqrt_biomass, discrete = FALSE)
hist(biomass$sqrt_biomass)
qqnorm(biomass$sqrt_biomass)
shapiro.test(biomass$sqrt_biomass) # looks better
# making a model to test distribution of model residuals w/ sqrt transformation
raw.data.test.sqrt <- lm(sqrt_biomass ~ Climate_Treatment * Galling_Status, data=biomass) # testing model
hist(resid(raw.data.test.sqrt)) # checking model residuals
shapiro.test(resid(raw.data.test.sqrt))
# going with sqrt root transformation

######### Assumption checking ##########
# in the assumption checking, we're making sure that our full model meets the assumptions of the model.
# the model below is the model structure we can use for all response variables; its testing to see
# if there is 1. an effect of climate treatment on height, 2. an effect of galling status on height, and 3. does the effect
# of climate on height depend on galling status. Subplot nested within footprint nested within rep is used as our random effect
# to account for variation between plots. Year is also included as a random effect to account for variation between years.
m1 <- lmer(sqrt(Biomass) ~ Climate_Treatment * Galling_Status + (1|Rep/Footprint/Subplot) + (1|Year), data = biomass, REML=F)

# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(m1, main = "Soca Biomass")
# Homogeneity of variance looks a bit off (increasing variance in resids does increase with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(m1) ~ biomass$Climate_Treatment) # not met
leveneTest(residuals(m1) ~ biomass$Galling_Status) # met
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals
qqPlot(resid(m1), main = "Soca biomass")
hist(residuals(m1), main = "Soca biomass")
shapiro.test(resid(m1))
outlierTest(m1) # checking for outliers - none

###### Checking model results ########
summary(m1)
anova(m1)

emmip(m1, Climate_Treatment~Galling_Status)

# Pairwise comparisons
contrast(emmeans(m1, ~Climate_Treatment*Galling_Status), "pairwise", simple = "each", combine = F, adjust = "mvt")

# re-set the reference level for diff. comparisons
biomass <- within(biomass, Climate_Treatment <- relevel(factor(Climate_Treatment), ref = "Ambient"))
m1 <- lmer(sqrt(Biomass) ~ Climate_Treatment * Galling_Status + (1|Rep/Footprint/Subplot) + (1|Year), data = biomass, REML=F)
summary(m1)

# make model table for supp
kable(anova(m1), digits = 3) %>% kableExtra::kable_styling()

# Extract back-transformed EMMs
emm <- emmeans(m1, ~ Climate_Treatment*Galling_Status, type = "response")
emm_df <- as.data.frame(emm)

######## calculating effect sizes ########
# Helper function to calculate percent increase
percent_increase <- function(mean1, mean2) {
        ((mean1 - mean2) / mean2) * 100
}

# Reshape data to make comparisons easier
emm_wide <- emm_df %>%
        select(Climate_Treatment, Galling_Status, response) %>%
        tidyr::pivot_wider(names_from = Galling_Status, values_from = response)

# Calculate percent increase in biomass for galled vs non-galled within each treatment
emm_wide <- emm_wide %>%
        mutate(Galled_vs_NonGalled = percent_increase(Galled, `Non-Galled`))

# For comparisons across climate treatments within galled plants
emm_galled <- emm_df %>% filter(Galling_Status == "Galled") %>%
        select(Climate_Treatment, response) %>%
        tidyr::pivot_wider(names_from = Climate_Treatment, values_from = response)

# Add percent increases across treatments for galled plants
emm_galled <- emm_galled %>%
        mutate(
                Warm_vs_Ambient = percent_increase(Warm, Ambient),
                WarmDrought_vs_Ambient = percent_increase(`Warm Drought`, Ambient),
                Warm_vs_Drought = percent_increase(Warm, `Ambient Drought`),
                WarmDrought_vs_Drought = percent_increase(`Warm Drought`, `Ambient Drought`)
        )

# View results
print(emm_wide)
print(emm_galled)

######## Biomass plotting ########
plant_biomass_plot <- ggplot(biomass, aes(x=Climate_Treatment, y = Biomass, color = Galling_Status, fill = Galling_Status)) +
        geom_point(size=1, position=position_jitterdodge(), alpha=0.4) +
        geom_errorbar(data = emm_df, 
                      aes(x = Climate_Treatment, y = response, ymin = response-SE, ymax = response+SE), 
                      width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
        geom_point(data = emm_df, 
                   aes(x = Climate_Treatment, y = response), 
                   shape = 21, size = 3, color = "black", position = position_dodge(width = 0.9)) +
        scale_fill_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
        scale_color_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
        labs(x = NULL, y = "Stem biomass (g)", title=NULL) +
        scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                         labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                                  "Ambient Drought" = "Drought",
                                  "Warm Drought" = "Warmed &\nDrought")) +
        annotate("text", x = 0.6, y = 15.4, label = "A", size = 5) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 11),
              axis.text.y = element_text(size = 14),
              axis.title = element_text(size=14,face="bold"),
              legend.position = "none")

png("plant_biomass.png", units="in", width=6, height=4, res=300)
plant_biomass_plot
dev.off()


######## Year plot for supp ########
m2 <- lmer(sqrt(Biomass) ~ Climate_Treatment * Galling_Status + Year + (1|Rep/Footprint/Subplot), data = biomass, REML=F)
anova(m2)
# Extract back-transformed EMMs
emm2 <- emmeans(m2, ~ Climate_Treatment*Galling_Status+Year, type = "response")
emm_df2 <- as.data.frame(emm2)

png("plant_biomass_year.png", units="in", width=10, height=6, res=300)
ggplot(biomass, aes(x=Climate_Treatment, y = Biomass, color = Galling_Status, fill = Galling_Status)) +
        facet_wrap(.~Year) +
        geom_point(size=1, position=position_jitterdodge(), alpha=0.4) +
        geom_errorbar(data = emm_df2, 
                      aes(x = Climate_Treatment, y = response, ymin = response-SE, ymax = response+SE), 
                      width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
        geom_point(data = emm_df2, 
                   aes(x = Climate_Treatment, y = response), 
                   shape = 21, size = 3, color = "black", position = position_dodge(width = 0.9)) +
        scale_fill_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
        scale_color_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
        labs(x = NULL, y = "Stem biomass (g)", title=NULL) +
        scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                         labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                                  "Ambient Drought" = "Drought",
                                  "Warm Drought" = "Warmed &\nDrought")) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 11),
              axis.text.y = element_text(size = 14),
              axis.title = element_text(size=14,face="bold"),
              legend.title = element_text(size=11,face="bold"),
              legend.text = element_text(size=11))
dev.off()


######## combine plant biomass and plant height plots together  ########
# note, you have to have ran both scripts (biomass and height) so that the two plots are in your global environment
png("plant_biomass_height.png", units="in", width=11, height=5, res=300)
ggarrange(plant_biomass_plot, plant_height_plot,
          ncol = 2, common.legend = T, legend="right", widths = c(1, 1))
dev.off()
 


