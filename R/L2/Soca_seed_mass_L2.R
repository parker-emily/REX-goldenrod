# TITLE:          REX: Soca seed mass analyses
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Emily Parker, Phoebe Zarnetske, Moriah Young, Mark Hammond
# DATA INPUT:     Data imported as csv files from shared REX Google drive T7_warmx_plant_traits L1 folder
# DATA OUTPUT:    Plots
# PROJECT:        REX
# DATE:           Feb 2024


# clear all existing data
rm(list=ls())

# load packages
library(tidyverse)
library(pscl)
library(performance)
library(fitdistrplus)
library(MASS)
library(lmtest)
library(glmmTMB)
library(plotrix)
library(emmeans)
library(ggpubr)

# Import data
dir<-Sys.getenv("DATA_DIR")
list.files(dir)
seed <- read.csv(file.path(dir, "T7_warmx_plant_traits/L1/T7_warmx_soca_seeds_mass_L1.csv"))
biomass <- read.csv(file.path(dir, "/T7_warmx_plant_traits/L1/T7_warmx_soca_biomass_L1.csv"))
str(seed)
# Storing year as a factor
seed$Year <- as.factor(seed$Year)
biomass$Year <- as.factor(biomass$Year)
# Adding unique plant ID for each subplot
seed <- seed %>%
  group_by(Treatment, Rep, Footprint, Subplot) %>%
  mutate(plant_num = 1:n())

# Merge seed and biomass dataframes
seed <- left_join(seed,biomass,by=c("Treatment","Rep","Footprint","Subplot","Climate_Treatment","Galling_Status","Year","Unique_ID"))
# Removing rep 4
#seed <- seed %>%
#  filter(!(Rep == 4 & Climate_Treatment == "Warm Drought")) %>%
#  filter(!(Rep == 4 & Climate_Treatment == "Ambient Drought"))
# Removing irrigated control
seed <- seed %>%
  filter(!(Climate_Treatment == "Irrigated Control"))


##################################### Distribution check #####################################
# First checking basic histogram
hist(seed$Seeds_Mass) # very right-skewed

# How much of the data is zeros?
100*sum(seed$Seeds_Mass == 0)/nrow(seed) # 32.4%

### Determining distribution ###
descdist(seed$Seeds_Mass, discrete = FALSE)
# log transform?
seed$seed_log <- log(seed$Seeds_Mass+1)
hist(seed$seed_log)
fit <- lm(seed_log~1, data = seed)
hist(resid(fit))
# mean centering?
seed$seed_scaled <- seed$seed_log - mean(seed$seed_log)
hist(seed$seed_scaled)
fit_scaled <- lm(seed_scaled~1, data = seed)
hist(resid(fit_scaled))
# square root?
seed$seed_sqrt <- sqrt(seed$Seeds_Mass)
hist(seed$seed_sqrt)
fit_sqrt <- lm(seed_sqrt~1, data = seed)
hist(resid(fit_sqrt))
shapiro.test(seed$seed_sqrt)

# Transformations are a no-go
# gamma distribution?
fit.gamma <- fitdist(seed$Seeds_Mass, distr = "gamma", method = "mme")
plot(fit.gamma)
# gamma seems to fit well - we can do a zero-inflated gamma model using glmmTMB



##### Full model #####
seed <- within(seed, Climate_Treatment <- relevel(factor(Climate_Treatment), ref = "Ambient"))
seed <- within(seed, Galling_Status <- relevel(factor(Galling_Status), ref = "Galled"))
full.model <- glmmTMB(Seeds_Mass ~ Climate_Treatment * Galling_Status + (1|Year:Rep:Footprint) + (1|Biomass),
                      data=seed,
                      family=ziGamma(link="log"),
                      zi=~.)
summary(full.model)
car::Anova(full.model) # Interaction term is significant, going to check all pairwise comparisons
### pairwise comparisons ###
# Note: in zero-inflated model, we're testing the probability of being zero
# Negative estimate (<0) means fewer 0's, positive estimate (>0) means more 0's
# Positive estimate means it is more likely to be zero (or, there is a reduced probability of of having a seed)
# Negative estimates means it is less likely to be zero (or, there is an increased probability of having a seed)
emmeans(full.model, ~Climate_Treatment*Galling_Status, type = "response") # seed weight
emmeans(full.model, ~Climate_Treatment*Galling_Status, component = "zi", type = "response") # probability

contrast(emmeans(full.model, ~Climate_Treatment*Galling_Status), "pairwise", simple="each", combine = F, adjust="mvt")
contrast(emmeans(full.model, ~Climate_Treatment*Galling_Status, component = "zi",), "pairwise", simple="each", combine = F, adjust="mvt")





######## Seed plotting ########
# Extract back-transformed EMMs
# Seed weight
emm_weight <- emmeans(full.model, ~ Climate_Treatment*Galling_Status, type = "response")
emm_weight_df <- as.data.frame(emm_weight)

# Probability of producing a seed
emm_prob <- emmeans(full.model, ~ Climate_Treatment*Galling_Status, component = "zi", type = "response")
emm_prob_df <- as.data.frame(emm_prob)
# Because emmeans shows the probability of being zero, we need to subtract from 1 to get the probability of producing a seed
emm_prob_df <- emm_prob_df %>%
  mutate(response_prob = 1-response) %>%
  mutate(asymp.LCL_prob = 1-asymp.LCL) %>%
  mutate(asymp.UCL_prob = 1-asymp.UCL)

# Probability of having a seed per plot using the raw data (for the geom_jitter)
mass_binom <- seed %>%
  mutate_at(vars(contains('Seeds_Mass')), ~1 * (. != 0))
mass_binom$Seeds_Mass[mass_binom$Seeds_Mass == 1] <- "Seed"
mass_binom$Seeds_Mass[mass_binom$Seeds_Mass == 0] <- "No Seed"
mass_binom_sum <- mass_binom %>%
  group_by(Treatment,Rep,Footprint,Subplot,Climate_Treatment,Galling_Status,Seeds_Mass) %>%
  count(Treatment,Rep,Footprint,Subplot,Climate_Treatment,Galling_Status,Seeds_Mass) %>%
  group_by(Treatment,Rep,Footprint,Subplot,Climate_Treatment,Galling_Status) %>%
  mutate(n = n/sum(n)) %>%
  group_by(Treatment,Rep,Footprint,Subplot,Climate_Treatment,Galling_Status,Seeds_Mass) %>%
  summarize(mean_n = mean(n),
            se = std.error(n))
mass_binom_seed <- mass_binom_sum %>%
  filter(Seeds_Mass == "Seed")

# set levels of the galling status and make sure they are factors
emm_weight_df$Galling_Status <- factor(emm_weight_df$Galling_Status, levels = c("Galled", "Non-Galled"))
seed$Galling_Status <- factor(seed$Galling_Status, levels = c("Galled", "Non-Galled"))

# Weight plot
weight_plot <- ggplot(seed, aes(x=Climate_Treatment, y = Seeds_Mass, color = Galling_Status, fill = Galling_Status)) +
  geom_point(size=1, position=position_jitterdodge(), alpha=0.4) +
  geom_errorbar(data = emm_weight_df, 
                aes(x = Climate_Treatment, y = response, ymin = response-SE, ymax = response+SE), 
                width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
  geom_point(data = emm_weight_df, 
             aes(x = Climate_Treatment, y = response), 
             shape = 21, size = 3, color = "black", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
  scale_color_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
  labs(x = NULL, y = "Seed weight (g)", title=NULL) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed &\nDrought")) +
  annotate("text", x = 0.6, y=3.5, label = "B", size=5) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size=16,face="bold"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=12))

mass_binom_seed$Galling_Status <- factor(mass_binom_seed$Galling_Status, levels = c("Galled", "Non-Galled"))
emm_prob_df$Galling_Status <- factor(emm_prob_df$Galling_Status, levels = c("Galled", "Non-Galled"))

# Probability plot
prob_plot <- ggplot(mass_binom_seed, aes(x=Climate_Treatment, y = mean_n, color = Galling_Status, fill = Galling_Status)) +
  geom_point(size=1, position=position_jitterdodge(), alpha=0.4) +
  geom_errorbar(data = emm_prob_df, 
                aes(x = Climate_Treatment, y = response_prob, ymin = response_prob-SE, ymax = response_prob+SE), 
                width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
  geom_point(data = emm_prob_df, 
             aes(x = Climate_Treatment, y = response_prob), 
             shape = 21, size = 3, color="black",position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
  scale_color_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
  labs(x = NULL, y = "Probability of having a seed", title=NULL) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed &\nDrought")) +
  annotate("text", x = 0.6, y=1, label = "A", size=5) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size=16,face="bold"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=12))

# Merging figures
png("seed_weight.png", units="in", width=11, height=5, res=300)
ggarrange(prob_plot,weight_plot,
          ncol = 2, common.legend = T, legend="right",widths = c(1, 1))
dev.off()





### For supp: plots split by year ###
full.model2 <- glmmTMB(Seeds_Mass ~ Climate_Treatment * Galling_Status + Year + (1|Rep:Footprint) + (1|Biomass),
                      data=seed,
                      family=ziGamma(link="log"),
                      zi=~.)
# Extract back-transformed EMMs
# Seed weight
emm_weight_year <- emmeans(full.model2, ~ Climate_Treatment*Galling_Status+Year, type = "response")
emm_weight_year_df <- as.data.frame(emm_weight_year)

# Probability of producing a seed
emm_prob_year <- emmeans(full.model2, ~ Climate_Treatment*Galling_Status+Year, component = "zi", type = "response")
emm_prob_year_df <- as.data.frame(emm_prob_year)
# Because emmeans shows the probability of being zero, we need to subtract from 1 to get the probability of producing a seed
emm_prob_year_df <- emm_prob_year_df %>%
  mutate(response_prob = 1-response)

# Probability of having a seed per plot using the raw data (for the geom_jitter)
mass_binom <- seed %>%
  mutate_at(vars(contains('Seeds_Mass')), ~1 * (. != 0))
mass_binom$Seeds_Mass[mass_binom$Seeds_Mass == 1] <- "Seed"
mass_binom$Seeds_Mass[mass_binom$Seeds_Mass == 0] <- "No Seed"
mass_binom_sum2 <- mass_binom %>%
  group_by(Treatment,Rep,Footprint,Subplot,Climate_Treatment,Galling_Status,Year,Seeds_Mass) %>%
  count(Treatment,Rep,Footprint,Subplot,Climate_Treatment,Galling_Status,Year,Seeds_Mass) %>%
  group_by(Treatment,Rep,Footprint,Subplot,Climate_Treatment,Galling_Status,Year) %>%
  mutate(n = n/sum(n)) %>%
  group_by(Treatment,Rep,Footprint,Subplot,Climate_Treatment,Galling_Status,Year,Seeds_Mass) %>%
  summarize(mean_n = mean(n),
            se = std.error(n))
mass_binom_seed2 <- mass_binom_sum2 %>%
  filter(Seeds_Mass == "Seed")

# Weight plot
weight_year_plot <- ggplot(seed, aes(x=Climate_Treatment, y = Seeds_Mass, color = Galling_Status, fill = Galling_Status)) +
  facet_wrap(.~Year) +
  geom_point(size=1, position=position_jitterdodge(), alpha=0.4) +
  geom_errorbar(data = emm_weight_year_df, 
                aes(x = Climate_Treatment, y = response, ymin = response-SE, ymax = response+SE), 
                width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
  geom_point(data = emm_weight_year_df, 
             aes(x = Climate_Treatment, y = response), 
             shape = 21, size = 3, color = "black", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
  scale_color_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
  labs(x = NULL, y = "Seed weight (g)", title=NULL) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed &\nDrought")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size=16,face="bold"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=12))

# Probability plot
prob_year_plot <- ggplot(mass_binom_seed2, aes(x=Climate_Treatment, y = mean_n, color = Galling_Status, fill = Galling_Status)) +
  facet_wrap(.~Year) +
  geom_point(size=1, position=position_jitterdodge(), alpha=0.4) +
  geom_errorbar(data = emm_prob_year_df, 
                aes(x = Climate_Treatment, y = response_prob, ymin = response_prob-SE, ymax = response_prob+SE), 
                width = 0.2, color = "black", position = position_dodge(width = 0.9)) +
  geom_point(data = emm_prob_year_df, 
             aes(x = Climate_Treatment, y = response_prob), 
             shape = 21, size = 3, color="black",position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
  scale_color_manual(values = c("purple4", "plum1"), name="Galling status",labels=c("Galled","Non-Galled")) +
  labs(x = NULL, y = "Probability of having a seed", title=NULL) +
  scale_x_discrete(limits = c("Ambient", "Ambient Drought", "Warm", "Warm Drought"),
                   labels=c("Ambient" = "Ambient", "Warm" = "Warmed",
                            "Ambient Drought" = "Drought",
                            "Warm Drought" = "Warmed &\nDrought")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size=16,face="bold"),
        legend.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=12))

# Merging figures
png("seed_year_weight.png", units="in", width=10, height=8, res=300)
ggarrange(prob_year_plot,weight_year_plot,
          nrow = 2, common.legend = T, legend="right",widths = c(1, 1))
dev.off()


