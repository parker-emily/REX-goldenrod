# TITLE:          REX: Abiotic plots for goldenrod paper
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young, Emily Parker
# DATA INPUT:     Data imported as csv files from shared REX Google drive sensor data L1 folder
# DATA OUTPUT:    Plots of data
# PROJECT:        REX
# DATE:           May 2024



# Clear all existing data
rm(list=ls())

# Set working directory
dir <- Sys.getenv("DATA_DIR")
list.files(dir)

# Load packages
library(tidyverse)
library(plotrix)
library(ggpubr)
library(lmerTest)

# Read in data
hobo_data <- read.csv(file.path(dir, "sensors/OTC Footprints/L1/T7_warmx_HOBO_L1.csv"))
soil_data <- read.csv(file.path(dir, "sensors/OTC Footprints/L1/T7_warmx_soil_sensors_L1.csv"))
str(hobo_data)
str(soil_data)
# re-making the data column a date
hobo_data$Date_Time <- as.POSIXct(hobo_data$Date_Time, format = "%Y-%m-%d %H:%M:%S")
soil_data$Date_Time <- as.POSIXct(soil_data$sample_datetime, format = "%Y-%m-%d %H:%M:%S")



### subsetting data ###
# create new dataframe with only temp data from July - October
hobo_season <- hobo_data
hobo_season$month <- format(hobo_season$Date_Time,format="%m")
hobo_season$year <- format(hobo_season$Date_Time,format="%Y")
hobo_season$hour <- format(hobo_season$Date_Time, format="%H")
hobo_season$day <- format(hobo_season$Date_Time, format="%d")
hobo_season$year_month_day <- format(hobo_season$Date_Time, format="%Y-%m-%d")
hobo_season$date <- paste0(hobo_season$month,"",hobo_season$day)
#hobo_season$date <- as.numeric(hobo_season$date)
hobo_season_sum <- hobo_season %>%
  filter(!(year == "2023")) %>%
  filter(month >= "07") %>%
  filter(month <= "10")
# create new dataframe with only soil data from July - October
soil_season <- soil_data
soil_season$month <- format(soil_season$Date_Time,format="%m")
soil_season$year <- format(soil_season$Date_Time,format="%Y")
soil_season$hour <- format(soil_season$Date_Time, format="%H")
soil_season$day <- format(soil_season$Date_Time, format="%d")
soil_season$monthday <- format(soil_season$Date_Time, format="%m%d")
soil_season$month_day <- format(soil_season$Date_Time, format="%m-%d")
soil_season$year_month_day <- format(soil_season$Date_Time, format="%Y-%m-%d")
soil_season_sum <- soil_season %>%
  filter(!(Subplot_Descriptions == "irrigated_control")) %>% # remove irrigated control
  filter(!(year == "2023" | year == "2024")) %>%
  filter(month >= "07") %>%
  filter(month <= "10")
# Separate dataframe for drought check
soil_drought_check <- soil_season %>%
  filter(!(year == "2023" | year == "2024")) %>%
  filter(monthday >= "0620") %>%
  filter(monthday <= "0831") %>%
  filter(Subplot_Descriptions == "drought")



### temperature average from July - Oct ###
# take average temp per rep, per treatment
hobo_temp_rep_avg <- hobo_season_sum %>%
  filter(!(year == 2021 & Subplot_Descriptions == "drought")) %>%
  filter(!(year == 2021 & Subplot_Descriptions == "warmed_drought")) %>%
  group_by(Rep, Subplot_Descriptions) %>%
  summarize(average_temp = mean(Temperature_C, na.rm = TRUE))
# averaging over each rep so n=6 for each treatment
hobo_temp_avg <- hobo_temp_rep_avg %>%
  group_by(Subplot_Descriptions) %>%
  summarize(avg_temp = mean(average_temp, na.rm = TRUE),
            se = std.error(average_temp, na.rm = TRUE),
            count=n())

# take average temp per rep, per treatment, per year
hobo_temp_rep_avg_year <- hobo_season_sum %>%
  group_by(Rep,Subplot_Descriptions, year) %>%
  summarize(average_temp = mean(Temperature_C, na.rm = TRUE))
# averaging over each rep so n=6 for each treatment
hobo_temp_avg_year <- hobo_temp_rep_avg_year %>%
  group_by(Subplot_Descriptions, year) %>%
  summarize(avg_temp = mean(average_temp, na.rm = TRUE),
            se = std.error(average_temp, na.rm = TRUE),
            count=n())



### soil temp and moisture average from July - Oct ###
# take average per day, per rep, per treatment
soil_sampling_avg_rep <- soil_season_sum %>%
  group_by(Rep, Subplot_Descriptions) %>%
  summarize(average_temp = mean(temperature, na.rm = TRUE),
            average_moist = mean(vwc, na.rm=T))
# averaging over each rep, n varies between treatments
soil_sampling_avg <- soil_sampling_avg_rep %>%
  group_by(Subplot_Descriptions) %>%
  summarize(avg_temp = mean(average_temp, na.rm = TRUE),
            se_temp = std.error(average_temp, na.rm = TRUE),
            avg_moist = mean(average_moist, na.rm=T),
            se_moist = std.error(average_moist, na.rm=T),
            count=n())

# take average per rep, per treatment, per year
soil_sampling_avg_rep_year <- soil_season_sum %>%
  group_by(Rep, Subplot_Descriptions, year) %>%
  summarize(average_temp = mean(temperature, na.rm = TRUE),
            average_moist = mean(vwc, na.rm=T))
# averaging over each rep, n varies between treatments
soil_sampling_avg_year <- soil_sampling_avg_rep_year %>%
  group_by(Subplot_Descriptions, year) %>%
  summarize(avg_temp = mean(average_temp, na.rm = TRUE),
            se_temp = std.error(average_temp, na.rm = TRUE),
            avg_moist = mean(average_moist, na.rm=T),
            se_moist = std.error(average_moist, na.rm=T))



### daily soil moisture from july-aug in drought treatment ###
dr_check <- soil_drought_check %>%
  group_by(month_day,year,Rep) %>%
  summarize(average_moist = mean(vwc, na.rm = TRUE),
            se_moist = std.error(vwc, na.rm=T))



### main figure ###
### plot - air temp, soil temp, and soil moisture averaged over both years
level_order1 <- c("Ambient", 'Warmed', 'Drought',"Warmed_Drought") 
level_order2 <- c("ambient", 'warmed', 'drought',"warmed_drought") 

air_temp <- ggplot(data = hobo_temp_rep_avg, aes(x = factor(Subplot_Descriptions, level = level_order2), y = average_temp)) +
  geom_jitter(size=1, alpha=0.4,color="purple4") +
  geom_pointrange(data=hobo_temp_avg, aes(x = factor(Subplot_Descriptions, level = level_order2), y = avg_temp,
                                          ymin=avg_temp-se, ymax=avg_temp+se),
                  pch=21,size=1,fill="purple4") +
  labs(y="Air temperature (°C)", x=NULL) +
  scale_x_discrete(labels=c("ambient" = "Ambient", "drought" = "Drought",
                            "warmed" = "Warmed",
                            "warmed_drought" = "Warmed &\nDrought")) +
  annotate("text", x = 0.6, y=23.7, label = "A", size=5) +
  theme_bw() +
  theme(axis.title = element_text(size=17, face="bold"),
        axis.text = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

soil_temp <- ggplot(soil_sampling_avg_rep, aes(x = factor(Subplot_Descriptions, level=level_order2), y = average_temp)) +
  geom_jitter(size=1, alpha=0.4,color="purple4") +
  geom_pointrange(data=soil_sampling_avg,
                  aes(x = factor(Subplot_Descriptions, level=level_order2), y = avg_temp,
                          ymin=avg_temp-se_temp, ymax=avg_temp+se_temp),
                  pch=21,size=1,fill="purple4") +
  labs(y="Soil temperature (°C)", x=NULL) +
  scale_x_discrete(labels=c("ambient" = "Ambient", "drought" = "Drought",
                            "warmed" = "Warmed",
                            "warmed_drought" = "Warmed &\nDrought")) +
  annotate("text", x = 0.6, y=18.9, label = "B", size=5) +
  theme_bw() +
  theme(axis.title = element_text(size=17, face="bold"),
        axis.text = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

soil_moist <- ggplot(soil_sampling_avg_rep, aes(x = factor(Subplot_Descriptions, level=level_order2), y = average_moist)) +
  geom_jitter(size=1, alpha=0.4,color="purple4") +
  geom_pointrange(data=soil_sampling_avg,
                  aes(x = factor(Subplot_Descriptions, level=level_order2), y = avg_moist,
                      ymin=avg_moist-se_moist, ymax=avg_moist+se_moist),
                  pch=21,size=1,fill="purple4") +
  labs(y=bquote(bold("Soil moisture " (m^3/m^3))), x=NULL) +
  scale_x_discrete(labels=c("ambient" = "Ambient", "drought" = "Drought",
                            "warmed" = "Warmed",
                            "warmed_drought" = "Warmed &\nDrought")) +
  annotate("text", x = 0.6, y=0.25, label = "C", size=5) +
  theme_bw() +
  theme(axis.title = element_text(size=17, face="bold"),
        axis.text = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# combine plots
png("rex_abiotic.png", units="in", width=12, height=5, res=300)
ggpubr::ggarrange(air_temp,soil_temp,soil_moist,
                  ncol = 3,widths = c(0.9,0.9,1))
dev.off()



### stats ###
# daily average
daily_soil_moist <- soil_season_sum %>%
  group_by(year_month_day,year,Subplot_Descriptions,Replicate) %>%
  summarize(average_moist = mean(vwc, na.rm = TRUE))
daily_soil_temp <- soil_season_sum %>%
  group_by(year_month_day,year,Subplot_Descriptions,Replicate) %>%
  summarize(average_temp = mean(temperature, na.rm = TRUE))
daily_air_temp <- hobo_season_sum %>%
  filter(!(year == 2021 & Subplot_Descriptions == "drought")) %>%
  filter(!(year == 2021 & Subplot_Descriptions == "warmed_drought")) %>%
  group_by(year_month_day,year,Subplot_Descriptions,Rep) %>%
  summarize(average_temp = mean(Temperature_C, na.rm = TRUE))
# in the assumption checking, we're making sure that our full model meets the assumptions of the model.
# the model below is the model structure we can use for all response variables; its testing to see
# if there is 1. an effect of climate treatment on height, 2. an effect of galling status on height, and 3. does the effect
# of climate on height depend on galling status. Subplot nested within footprint nested within rep is used as our random effect
# to account for variation between plots. Year is also included as a random effect to account for variation between years.
m1 <- lmer(average_moist ~ Subplot_Descriptions + (1|Replicate) + (1|year), data = daily_soil_moist, REML=F)
m2 <- lmer(average_temp ~ Subplot_Descriptions + (1|Replicate) + (1|year), data = daily_soil_temp, REML=F)
m3 <- lmer(average_temp ~ Subplot_Descriptions + (1|Rep) + (1|year), data = daily_air_temp, REML=F)
# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(m1)
plot(m2)
plot(m3)
# Homogeneity of variance looks a bit off (increasing variance in resids does increase with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(m1) ~ daily_soil_moist$Subplot_Descriptions) # not met
leveneTest(residuals(m2) ~ daily_soil_temp$Subplot_Descriptions) # met
leveneTest(residuals(m3) ~ daily_air_temp$Subplot_Descriptions) # met
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals
qqPlot(resid(m1))
hist(residuals(m1))
shapiro.test(resid(m1))
outlierTest(m1) # checking for outliers - none
qqPlot(resid(m2))
hist(residuals(m2))
shapiro.test(resid(m2))
outlierTest(m2) # checking for outliers - none
qqPlot(resid(m3))
hist(residuals(m3))
shapiro.test(resid(m3))
outlierTest(m3) # checking for outliers - none

# checking model results
anova(m1)
anova(m2)
anova(m3)

# Pairwise comparisons
contrast(emmeans(m1, ~Subplot_Descriptions), "pairwise", simple = "each", combine = F, adjust = "mvt")
contrast(emmeans(m2, ~Subplot_Descriptions), "pairwise", simple = "each", combine = F, adjust = "mvt")
contrast(emmeans(m3, ~Subplot_Descriptions), "pairwise", simple = "each", combine = F, adjust = "mvt",
         pbkrtest.limit = 7000,lmerTest.limit = 7000)




### Figs for supp ###
# Remove irr.
soil_sampling_avg_year <- soil_sampling_avg_year %>%
  filter(!(Subplot_Descriptions == "irrigated_control"))
### plot - air temp, soil temp, and soil moisture separate for both years
level_order1 <- c("Ambient", 'Warmed', 'Drought',"Warmed_Drought") 
level_order2 <- c("irrigated_control","ambient", 'warmed', 'drought',"warmed_drought") 
air_temp_yearly <- ggplot(hobo_temp_avg_year, aes(x = factor(Subplot_Descriptions, level = level_order2), y = avg_temp, fill=year)) +
  geom_pointrange(aes(ymin=avg_temp-se, ymax=avg_temp+se),pch=21,size=1,position=position_jitter(w=0.1)) +
  labs(y="Air temperature (°C)", x=NULL) +
  scale_x_discrete(labels=c("ambient" = "Ambient", "drought" = "Drought",
                            "warmed" = "Warmed",
                            "warmed_drought" = "Warmed &\nDrought")) +
  theme_bw() +
  #annotate("text", x = 0.6, y=23, label = "A", size=6) +
  scale_fill_manual(name="Year",
                    values = c("#FFB451", "#0b0055")) +
  theme(axis.title = element_text(size=17),
        axis.text = element_text(size=15),
        legend.title = element_text(size=17),
        legend.text = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
soil_temp_yearly <- ggplot(soil_sampling_avg_year, aes(x = factor(Subplot_Descriptions, level=level_order2), y = avg_temp, fill=year)) +
  geom_pointrange(aes(ymin=avg_temp-se_temp, ymax=avg_temp+se_temp), pch=21,size=1,position=position_jitter(w=0.1)) +
  #geom_errorbar(aes(ymin=avg_temp-se_temp, ymax=avg_temp+se_temp),width=0.1,color="black",linetype="solid") +
  #geom_point(size = 2) +
  labs(y="Soil temperature (°C)", x=NULL) +
  scale_x_discrete(labels=c("ambient" = "Ambient", "drought" = "Drought",
                            "warmed" = "Warmed",
                            "warmed_drought" = "Warmed &\nDrought")) +
  theme_bw() +
  # annotate("text", x = 0.7, y=21.3, label = "B", size=6) +
  scale_fill_manual(name="Year",
                    values = c("#FFB451", "#0b0055")) +
  theme(axis.title = element_text(size=17),
        axis.text = element_text(size=15),
        legend.title = element_text(size=17),
        legend.text = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
soil_moist_yearly <- ggplot(soil_sampling_avg_year, aes(x = factor(Subplot_Descriptions, level=level_order2), y = avg_moist, fill=year)) +
  geom_pointrange(aes(ymin=avg_moist-se_moist, ymax=avg_moist+se_moist),pch=21,size=1,position=position_jitter(w=0.1)) +
  #geom_errorbar(aes(ymin=avg_moist-se_moist, ymax=avg_moist+se_moist),width=0.1,color="black",linetype="solid") +
  #geom_point(size = 2) +
  labs(y=bquote("Soil moisture " (m^3/m^3)), x=NULL) +
  scale_x_discrete(labels=c("ambient" = "Ambient", "drought" = "Drought",
                            "warmed" = "Warmed",
                            "warmed_drought" = "Warmed &\nDrought")) +
  theme_bw() +
  #annotate("text", x = 0.7, y=0.28, label = "C", size=6) +
  scale_fill_manual(name="Year",
                    values = c("#FFB451", "#0b0055")) +
  theme(axis.title = element_text(size=17),
        axis.text = element_text(size=15),
        legend.title = element_text(size=17),
        legend.text = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# combine plots
abiotic_comb_yearly <- ggpubr::ggarrange(air_temp_yearly,soil_temp_yearly,soil_moist_yearly,
                                         ncol = 3,widths = c(1,1,1), common.legend=T, legend="right")
png("rex_abiotic_yearly.png", units="in", width=12, height=5, res=300)
annotate_figure(abiotic_comb_yearly,
                bottom = text_grob("Treatment", color = "black", size=17))
dev.off()





### Checking to see drought treatment effects over time across reps
# plot - daily avg. soil moisture from july-august 2021
dr_check$Rep <- as.character(dr_check$Rep)
dr_check_2021 <- dr_check %>%
  filter(year == "2021")
everyother <- function(x) x[seq_along(x) %% 4 == 0]
png("rex_drought_2021.png", units="in", width=12, height=5, res=300)
ggplot(dr_check_2021, aes(x = month_day, y = average_moist, group=Rep, color=Rep)) +
  geom_errorbar(aes(ymin=average_moist-se_moist, ymax=average_moist+se_moist),width=0.1,color="black",linetype="solid") +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(y="VWC", x="Date", title="2021") +
  scale_x_discrete(breaks=everyother) +
  theme_bw() +
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=18),
        title=element_text(size=20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=18))
dev.off()



# plot - daily avg. soil moisture from july-august 2022
dr_check$Rep <- as.character(dr_check$Rep)
dr_check_2022 <- dr_check %>%
  filter(year == "2022")
everyother <- function(x) x[seq_along(x) %% 6 == 0]
png("rex_drought_2022.png", units="in", width=12, height=5, res=300)
ggplot(dr_check_2022, aes(x = month_day, y = average_moist, group=Rep, color=Rep)) +
  geom_errorbar(aes(ymin=average_moist-se_moist, ymax=average_moist+se_moist),width=0.1,color="black",linetype="solid") +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(y="VWC", x="Date", title="2022") +
  scale_x_discrete(breaks=everyother) +
  theme_bw() +
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=18),
        title = element_text(size=20),
        legend.title = element_text(size=20),
        legend.text = element_text(size=18))
dev.off()





