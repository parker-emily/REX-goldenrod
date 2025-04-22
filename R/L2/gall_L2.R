# TITLE:          REX: Gall mass analyses
# AUTHORS:        Kara Dobson, Emily Parker
# COLLABORATORS:  Phoebe Zarnetske, Moriah Young, Kristin Wolford, Mark Hammond
# DATA INPUT:     Data imported as csv files from shared REX Google drive T7_warmx_insect L1 folder
# DATA OUTPUT:    analyses
# PROJECT:        REX
# DATE:           Jan 2022; updated June 2024

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

# Set working directory
dir<-setwd("DATA_DIR")

# Read in data
weight <- read.csv(file.path(dir, "T7_warmx_insect/L1/T7_warmx_Soca_galls_weight_L1.csv"))
weight <- weight %>%
  filter(!(Climate_Treatment == "Irrigated Control"))

count <- read.csv(file.path(dir, "T7_warmx_insect/L1/T7_warmx_Soca_gall_chmb_count_L1.csv"))
count <- count %>%
  filter(!(treatment == "Irrigated Control"))

vol <- read.csv(file.path(dir, "T7_warmx_insect/L1/T7_warmx_Soca_gall_chmb_vol_L1.csv"))
vol <- vol %>%
  filter(!(treatment == "Irrigated Control"))



######## Gall mass analysis ########
# Data exploration
descdist(weight$Dried_Weight, discrete = FALSE)
hist(weight$Dried_Weight)
qqnorm(weight$Dried_Weight)
shapiro.test(weight$Dried_Weight)
# pretty right skewed, going to try a few transformations & poisson distribution

# square root transformation
weight$sqrt_weight <- sqrt(weight$Dried_Weight)
descdist(weight$sqrt_weight, discrete = FALSE)
hist(weight$sqrt_weight)
qqnorm(weight$sqrt_weight)
shapiro.test(weight$sqrt_weight)

# log transformation
weight$log_weight <- log(weight$Dried_Weight)
descdist(weight$log_weight, discrete = FALSE)
hist(weight$log_weight)
qqnorm(weight$log_weight)
shapiro.test(weight$log_weight) # sqrt was better

# Assumption checking - log transformation
m1 <- lmer(sqrt(Dried_Weight) ~ Climate_Treatment + (1|Rep/Footprint/Subplot) + (1|Year), data = weight, REML=FALSE)
# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(m1, main = "Gall weight")
# Homogeneity of variance is ok here (increasing variance in resids is not increasing with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(m1) ~ weight$Climate_Treatment)
# Assumption met
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals
qqPlot(resid(m1), main = "Gall weight")
hist(residuals(m1), main = "Gall weight")
shapiro.test(resid(m1))
outlierTest(m1)

# Model outcomes
anova(m1)
kable(anova(m1)) %>% kableExtra::kable_styling()
summary(m1)



######## Gall mass plotting ########
# Extract back-transformed EMMs
emm_weight <- emmeans(m1, ~ Climate_Treatment, type = "response")
emm_weight_df <- as.data.frame(emm_weight)

#png("gall_mass.png", units="in", width=6, height=4, res=300)
weight_plot <- ggplot(weight, aes(x=Climate_Treatment, y = Dried_Weight)) +
  geom_jitter(alpha = 0.3, color = "purple4") +
  labs(x = NULL, y = "Dried gall biomass (g)", title=NULL) +
  geom_errorbar(data = emm_weight_df, 
                aes(x = Climate_Treatment, y = response, ymin = response-SE, ymax = response+SE), 
                width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
  geom_point(data = emm_weight_df, 
             aes(x = Climate_Treatment, y = response), 
             shape = 21, size = 3, color = "black",fill = "purple4", position = position_dodge(width = 0.9)) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed &\nDrought")) +
  annotate("text", x = 0.6, y=1.5, label = "A", size=5) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
               axis.text.y = element_text(size = 14),
               axis.title = element_text(size=14,face="bold"))
weight_plot



######## Gall chamber counts analysis ########
# Data exploration
descdist(count$num_of_chambers, discrete = FALSE)
hist(count$num_of_chambers)
qqnorm(count$num_of_chambers)
shapiro.test(count$num_of_chambers)
# pretty right skewed, going to try a few transformations & poisson distribution

# square root transformation
count$sqrt_count <- sqrt(count$num_of_chambers)
descdist(count$sqrt_count, discrete = FALSE)
hist(count$sqrt_count)
qqnorm(count$sqrt_count)
shapiro.test(count$sqrt_count)

# log transformation
count$log_count <- log((count$num_of_chambers)+1)
descdist(count$log_count, discrete = FALSE)
hist(count$log_count)
qqnorm(count$log_count)
shapiro.test(count$log_count)

# poisson distribution?
fit.pois <- fitdist(count$num_of_chambers, "pois")
plot(fit.pois)

# negative binomial distribution?
fit.neg <- fitdist(count$num_of_chambers, "nbinom")
plot(fit.neg)

# comparing the two
gofstat(list(fit.pois, fit.neg))
par(mfrow = c(1,2))
denscomp(list(fit.pois, fit.neg))
cdfcomp(list(fit.pois, fit.neg))
# going with poisson, also makes sense bc count data

# Assumption checking - poisson 
count %>%
  dplyr::filter(num_of_chambers != "0") %>%
  dplyr::summarize(mean_num = mean(num_of_chambers, na.rm=T), var_num = var(num_of_chambers, na.rm=T))
# mean = variance (approximately)

# main model
m2 <- glmer(num_of_chambers ~ treatment + (1|rep/footprint), data = count, family = poisson)
summary(m2)
car::Anova(m2)
anova(m2)



######## Gall chamber count plotting ########
# Extract back-transformed EMMs
emm_count <- emmeans(m2, ~ treatment, type = "response")
emm_count_df <- as.data.frame(emm_count)

#png("gall_mass.png", units="in", width=6, height=4, res=300)
count_plot <- ggplot(count, aes(x=treatment, y = num_of_chambers)) +
  geom_jitter(alpha = 0.3, color = "purple4") +
  labs(x = NULL, y = "Larval chamber count", title=NULL) +
  geom_errorbar(data = emm_count_df, 
                aes(x = treatment, y = rate, ymin = rate-SE, ymax = rate+SE), 
                width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
  geom_point(data = emm_count_df, 
             aes(x = treatment, y = rate), 
             shape = 21, size = 3, color = "black",fill = "purple4", position = position_dodge(width = 0.9)) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed &\nDrought")) +
  annotate("text", x = 0.6, y=6, label = "B", size=5) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))
count_plot



######## Gall chamber volume analysis ########
# Data exploration
descdist(vol$chamber_volume_mm3, discrete = FALSE)
hist(vol$chamber_volume_mm3)
qqnorm(vol$chamber_volume_mm3)
shapiro.test(vol$chamber_volume_mm3)
# pretty right skewed, going to try a few transformations & poisson distribution

# square root transformation
vol$sqrt_vol <- sqrt(vol$chamber_volume_mm3)
descdist(vol$sqrt_vol, discrete = FALSE)
hist(vol$sqrt_vol)
qqnorm(vol$sqrt_vol)
shapiro.test(vol$sqrt_vol)

# log transformation
vol$log_vol <- log(vol$chamber_volume_mm3)
descdist(vol$log_vol, discrete = FALSE)
hist(vol$log_vol)
qqnorm(vol$log_vol)
shapiro.test(vol$log_vol) # really good

# Assumption checking - log transformation
m3 <- lmer(log(chamber_volume_mm3) ~ treatment + (1|unique_plant_number) + (1|rep/footprint), data = vol, REML=FALSE)
# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(m3, main = "Gall chamber volume")
# Homogeneity of variance is ok here (increasing variance in resids is not increasing with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(m3) ~ vol$treatment)
# Assumption met
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals
qqPlot(resid(m3), main = "Gall chamber volume")
hist(residuals(m3), main = "Gall chamber volume")
shapiro.test(resid(m3))
outlierTest(m3)

# model results
summary(m3)
anova(m3)



######## Gall chamber volume plotting ########
# Extract back-transformed EMMs
emm_vol <- emmeans(m3, ~ treatment, type = "response")
emm_vol_df <- as.data.frame(emm_vol)

#png("gall_mass.png", units="in", width=6, height=4, res=300)
vol_plot <- ggplot(vol, aes(x=treatment, y = chamber_volume_mm3)) +
  geom_jitter(alpha = 0.3, color = "purple4") +
  labs(x = NULL, y = "Chamber volume (mm³)", title=NULL) +
  geom_errorbar(data = emm_vol_df, 
                aes(x = treatment, y = response, ymin = response-SE, ymax = response+SE), 
                width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
  geom_point(data = emm_vol_df, 
             aes(x = treatment, y = response), 
             shape = 21, size = 3, color = "black",fill = "purple4", position = position_dodge(width = 0.9)) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed &\nDrought")) +
  annotate("text", x = 0.6, y=20, label = "C", size=5) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))
vol_plot

#stitch plots together
png("gall_weight_count_vol.png", units="in", width=11, height=5, res=300)
ggarrange(weight_plot, count_plot, vol_plot,
          nrow = 1, common.legend = T, legend="right",widths = c(1, 1))
dev.off()



### For supp: gall biomass plots split by year ###
m4 <- lmer(sqrt(Dried_Weight) ~ Climate_Treatment + Year + (1|Rep/Footprint/Subplot), data = weight, REML=FALSE)
anova(m4)

emm_weight_year <- emmeans(m4, ~ Climate_Treatment*Year, type = "response")
emm_weight_year_df <- as.data.frame(emm_weight_year)

png("gall_weight_year.png", units="in", width=10, height=6, res=300)
ggplot(weight, aes(x=Climate_Treatment, y = Dried_Weight)) +
  facet_wrap(.~Year) +
  geom_jitter(alpha = 0.3, color = "purple4") +
  labs(x = NULL, y = "Dried gall biomass (g)", title=NULL) +
  geom_errorbar(data = emm_weight_year_df, 
                aes(x = Climate_Treatment, y = response, ymin = response-SE, ymax = response+SE), 
                width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
  geom_point(data = emm_weight_year_df, 
             aes(x = Climate_Treatment, y = response), 
             shape = 21, size = 3, color = "black",fill = "purple4", position = position_dodge(width = 0.9)) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed &\nDrought")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"))
dev.off()
