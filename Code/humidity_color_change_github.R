#Humidity induces cuticular color change in iridescent sweat bees

##### Set-up ####

#Load necessary packages
library(ggplot2)
library(dplyr)
library(car)
library(raster)
library(sf)
library(glmmTMB)
library(ggeffects)
library(MuMIn)
library(mgcv)
library(maps)
library(igraph)

df <- read.csv("Agapostemon_color_data_github.csv") 

# Calculate relative color values
df$RB_mean <- df$R_mean/df$B_mean
df$RG_mean <- df$R_mean/df$G_mean
df$GB_mean <- df$G_mean/df$B_mean
df$RB_median <- df$R_median/df$B_median
df$RG_median <- df$R_median/df$G_median
df$GB_median <- df$G_median/df$B_median

humid_df <- subset(df, treatment=="humid")
dry_df <- subset(df, treatment=="dry")

# calculate mean and standard error for R/B mean, humid
summary_humid_df_RB <- humid_df %>%
  group_by(hours, age_category) %>%
  summarise(mean_RB_mean = mean(RB_mean),
            SE = sd(RB_mean) / sqrt(n()))  
# calculate mean and standard error for R/G mean, humid
summary_humid_df_RG <- humid_df %>%
  group_by(hours, age_category) %>%
  summarise(mean_RG_mean = mean(RG_mean),
            SE = sd(RG_mean) / sqrt(n()))  
# calculate mean and standard error for R/B mean, dry
summary_dry_df_RB <- dry_df %>%
  group_by(hours, age_category) %>%
  summarise(mean_RB_mean = mean(RB_mean),
            SE = sd(RB_mean) / sqrt(n()))  
# calculate mean and standard error for R/G mean, dry
summary_dry_df_RG <- dry_df %>%
  group_by(hours, age_category) %>%
  summarise(mean_RG_mean = mean(RG_mean),
            SE = sd(RG_mean) / sqrt(n()))  


# Does color changewith humidity?
# # Organize data to compare R/B mean values at 0 hr vs 55 hr in humid treatment (preserved bees)
before_humid_collections <-subset(humid_df, age_category=="collections" & hours==0)
after_humid_collections <-subset(humid_df, age_category=="collections" & hours==55)
paired_humid_collections <- merge(before_humid_collections, after_humid_collections, by = "barcode", suffixes = c("_before", "_after"))
# assess normality with QQPlot
paired_humid_collections$diff <- paired_humid_collections$RB_mean_before - paired_humid_collections$RB_mean_after
qqPlot(paired_humid_collections$diff) #normal
# run paired t-test
t.test(paired_humid_collections$RB_mean_before, paired_humid_collections$RB_mean_after, paired = TRUE)

# # Organize data to compare R/B mean values at 0 hr vs 55 hr in dry treatment (preserved bees)
before_dry_collections <-subset(dry_df, age_category=="collections" & hours==0)
after_dry_collections <-subset(dry_df, age_category=="collections" & hours==55)
paired_dry_collections <- merge(before_dry_collections, after_dry_collections, by = "barcode", suffixes = c("_before", "_after"))
# assess normality with QQPlot
paired_dry_collections$diff <- paired_dry_collections$RB_mean_before - paired_dry_collections$RB_mean_after
qqPlot(paired_dry_collections$diff) #normal
# run paired t-test
t.test(paired_dry_collections$RB_mean_before, paired_dry_collections$RB_mean_after, paired = TRUE)
# 
# Organize data to compare R/B mean values at 0 hr vs 55 hr in humid treatment (fresh bees)
before_humid_freezer <-subset(humid_df, age_category=="freezer" & hours==0)
after_humid_freezer <-subset(humid_df, age_category=="freezer" & hours==55)
paired_humid_freezer <- merge(before_humid_freezer, after_humid_freezer, by = "barcode", suffixes = c("_before", "_after"))
# assess normality with QQPlot
paired_humid_freezer$diff <- paired_humid_freezer$RB_mean_before - paired_humid_freezer$RB_mean_after
qqPlot(paired_humid_freezer$diff) #normal
# run paired t-test
t.test(paired_humid_freezer$RB_mean_before, paired_humid_freezer$RB_mean_after, paired = TRUE)

# Organize data to compare R/B mean values at 0 hr vs 55 hr in dry treatment (fresh bees)
before_dry_freezer <-subset(dry_df, age_category=="freezer" & hours==0)
after_dry_freezer <-subset(dry_df, age_category=="freezer" & hours==55)
paired_dry_freezer <- merge(before_dry_freezer, after_dry_freezer, by = "barcode", suffixes = c("_before", "_after"))
# assess normality with QQPlot
paired_dry_freezer$diff <- paired_dry_freezer$RB_mean_before - paired_dry_freezer$RB_mean_after
qqPlot(paired_dry_freezer$diff) #normal
# run paired t-test
t.test(paired_dry_freezer$RB_mean_before, paired_dry_freezer$RB_mean_after, paired = TRUE)
# 
paired_humid_data <- rbind(paired_humid_freezer, paired_humid_collections)
qqPlot(paired_humid_data$diff) #normal
# run paired t-test
t.test(paired_humid_data$RB_mean_before, paired_humid_data$RB_mean_after, paired = TRUE)

paired_dry_data <- rbind(paired_dry_freezer, paired_dry_collections)
qqPlot(paired_dry_data$diff) #normal
# run paired t-test
t.test(paired_dry_data$RB_mean_before, paired_dry_data$RB_mean_after, paired = TRUE)

#summary statistics
mean(paired_dry_data$RB_mean_after)
sd(paired_dry_data$RB_mean_after)/sqrt(24)
mean(paired_humid_data$RB_mean_after)
sd(paired_humid_data$RB_mean_after)/sqrt(24)

zerohrs_df <-subset(df, hours==0)
mean(zerohrs_df$RB_mean)
sd(zerohrs_df$RB_mean)/sqrt(24)

# Is the magnitude of color change affected by the age of specimens?
#Run independent t tests on the differences.


leveneTest(diff ~ age_category_before, data = paired_humid_data) # data are heteroscedastic, proceed with Welch's t test (default in t.test function)
t.test(diff ~ age_category_before, data = paired_humid_data, var.equal=FALSE) # P < 0.001

leveneTest(diff ~ age_category_before, data = paired_dry_data) # data are marginally homoscedastic (P = 0.096)
t.test(diff ~ age_category_before, data = paired_dry_data, var.equal=FALSE) # P =0.286
t.test(diff ~ age_category_before, data = paired_dry_data, var.equal=TRUE) # P =0.281
# Result is the same regardless of whether we use student's t or Welch's t





#### iNaturalist Analysis ####
inat_df <- read.csv("inat_bee_color_data_github.csv")

# mirrored log transform
inat_df$relative_humidity[inat_df$relative_humidity > 100] <- 100 # fix values above 100
inat_df$rh_transformed <- -log(101-inat_df$relative_humidity) #because many values are close to 100. common with humidity data. The negative reverses the mirroring again so that that values are more interpretable. Higher transformed values correspond to higher humidity.
inat_df$rb_mean_transformed <- log(inat_df$rb_mean)

#  GLMM TMB WITH LOG LINK. BEST FIT.
model_fixed <- glmmTMB(rb_mean ~ rh_transformed, family = Gamma(link="log"), data = inat_df)
sim_res <- simulateResiduals(fittedModel = model_fixed, n = 1000)
plot(sim_res) 
r.squaredGLMM(model_fixed)
summary(model_fixed)


