# TITLE:          REX: Gall chamber volume & count analyses
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Moriah Young, Kristin Wolford, Emily Parker, Mark Hammond
# DATA INPUT:     Data imported as csv files from shared REX Google drive T7_warmx_insect L1 folder
# DATA OUTPUT:    analyses
# PROJECT:        REX
# DATE:           Jan 2022

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

# Set working directory
dir<-Sys.getenv("DATA_DIR")

# Read in data
count <- read.csv(file.path(dir, "T7_warmx_insect/L1/T7_warmx_Soca_gall_chmb_count_L1.csv"))
count <- count %>%
  filter(!(treatment == "Irrigated Control"))
vol <- read.csv(file.path(dir, "T7_warmx_insect/L1/T7_warmx_Soca_gall_chmb_vol_L1.csv"))
vol <- vol %>%
  filter(!(treatment == "Irrigated Control"))

######## Gall chamber counts ########
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




######## Gall chamber volume ########
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
m1 <- lmer(log_vol ~ treatment + (1|unique_plant_number) + (1|rep/footprint), data = vol, REML=FALSE)
# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(m1, main = "Gall chamber volume")
# Homogeneity of variance is ok here (increasing variance in resids is not increasing with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(m1) ~ vol$treatment)
# Assumption met
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals
qqPlot(resid(m1), main = "Gall chamber volume")
hist(residuals(m1), main = "Gall chamber volume")
shapiro.test(resid(m1))
outlierTest(m1)

# model results
summary(m1)
anova(m1)



######## Plotting ########
# Taking averages
count_avg <- count %>%
  group_by(treatment) %>%
  summarize(avg_count = mean(num_of_chambers, na.rm = TRUE),
            se = std.error(num_of_chambers, na.rm = TRUE))
vol_avg <- vol %>%
  group_by(treatment) %>%
  summarize(avg_vol = mean(chamber_volume_mm3, na.rm = TRUE),
            se = std.error(chamber_volume_mm3, na.rm = TRUE))

#pointrange - vol
png("chmbr_vol_overall_point.png", units="in", width=9, height=6, res=300)
ggplot(vol_avg, aes(x = treatment, y = avg_vol)) +
  geom_pointrange(aes(ymin = avg_vol - se, ymax = avg_vol + se),pch=21,size=1,position=position_dodge(0.2), fill = "purple4") +
  labs(x = NULL, y = "Chamber Volume (mm^3)", title=NULL) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed\nDrought"))

dev.off()

#pointrange - count
png("chmbr_count_overall_point.png", units="in", width=9, height=6, res=300)
ggplot(count_avg, aes(x = treatment, y = avg_count)) +
  geom_pointrange(aes(ymin = avg_count - se, ymax = avg_count + se),pch=21,size=1,position=position_dodge(0.2), fill = "purple4") +
  labs(x = NULL, y = "Number of Larval Chambers", title=NULL) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed\nDrought"))

dev.off()

