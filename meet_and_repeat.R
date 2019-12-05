

# load the data
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')

#basic checks of the data, BPRS
names(BPRS)
str(BPRS)
summary(BPRS)

#basic checks of the data, rats
names(RATS)
str(RATS)
summary(RATS)


#factoring
library(dplyr)
library(tidyr)
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
# Convert to long form
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
# Extract the week number
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,5)))

# glimpse at the BPRSL data
glimpse(BPRSL)
dim(BPRSL)
str(BPRSL)

#RATS factoring
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)
# Convert data to long form
RATSL <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD,3,4))) 

# Glimpse the data
glimpse(RATSL)
dim(RATSL)
str(RATSL)

#BRSL analysis
library(ggplot2)
ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))

#what if linear regression is used
simple_linear_model <- lm(bprs ~ treatment + week, data = BPRSL)
summary(simple_linear_model)

#tracking phenomenan plots
# Standardise the variable bprs
BPRSL <- BPRSL %>%
  group_by(week) %>%
  mutate(stdbprs = (bprs - mean(bprs))/sd(bprs) ) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSL)

# Plot again with the standardised bprs
ggplot(BPRSL, aes(x = week, y = stdbprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  scale_y_continuous(name = "standardized bprs")

#chapter 9 exercises with BPRS data
#PLOT BPRSL
ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))

#RIM
#install.package("lme4")
library(lme4)
BPRSL_ref <- lmer(bprs ~ week + treatment + (1 | subject), data = BPRSL, REML = FALSE)
summary(BPRSL_ref)

#Slippery slopes: Random Intercept and Random Slope Model
BPRSL_ref1 <- lmer(bprs ~ week + treatment + (week | subject), data = BPRSL, REML = FALSE)
summary(BPRSL_ref1)
# perform an ANOVA test on the two models
anova(BPRSL_ref1, BPRSL_ref)

#Time to interact: Random Intercept and Random Slope Model with interaction
BPRSL_ref2 <- lmer(bprs ~ week * treatment + (week | subject), data = BPRSL, REML = FALSE)
summary(BPRSL_ref2)
anova(BPRSL_ref2, BPRSL_ref1)


# Create a vector of the fitted values
Fitted <- fitted(BPRSL_ref2)

# Create a new column fitted to BPRSL
BPRSL <- BPRSL %>%
  mutate(Fitted)

# draw the plot of BPRSL with fitted values
ggplot(BPRSL, aes(x = week, y = Fitted, group= subject, linetype = treatment)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "right") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))

# draw the plot of BPRSL with observed values
ggplot(BPRSL, aes(x = week, y = bprs, group= subject, linetype = treatment)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "right") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))

#RATSL data analysis
#tracking without standardization
library(ggplot2)
ggplot(RATSL, aes(x = Time, y = Weight, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ Group, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(RATSL$Weight), max(RATSL$Weight)))


#tracking phenomenan plots
RATSL <- RATSL %>%
  group_by(Time) %>%
  mutate(stdweight = (Weight - mean(Weight))/sd(Weight) ) %>%
  ungroup()
glimpse(RATSL)
ggplot(RATSL, aes(x = Time, y = stdweight, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ Group, labeller = label_both) +
  scale_y_continuous(name = "standardized weights")

#standard error calculation
# Number of weeks, baseline (week 0) included
n <- RATSL$Time %>% unique() %>% length()

# Summary data with mean and standard error of bprs by group and time 
RATSSS <- RATSL %>%
  group_by(Group, Time) %>%
  summarise( mean = mean(Weight), se = sd(Weight)/sqrt(n) ) %>%
  ungroup()

# Glimpse the data
glimpse(RATSSS)

# Plot the mean profiles
ggplot(RATSSS, aes(x = Time, y = mean, linetype = Group, shape = Group)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2,3)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2,3)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.8,0.8)) +
  scale_y_continuous(name = "mean(Weight) +/- se(Weight)")
RATSL$Time

#means and outlier checking
# Create a summary data by treatment and subject with mean as the summary variable (ignoring baseline week 0).
RATSSL8S <- RATSL %>%
  filter(Time > 1) %>%
  group_by(Group, ID) %>%
  summarise( mean=mean(Weight))
# Glimpse the data
glimpse(RATSSL8S)
# Draw a boxplot of the mean versus treatment
ggplot(RATSSL8S, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(Weight), weeks 8-64")

# Create a new data by filtering the outlier and adjust the ggplot code the draw the plot again with the new data
RATSSL8S1 <- RATSSL8S %>%
  filter(mean < 550 & mean>250)
ggplot(RATSSL8S1, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(Weight), weeks 8-64")

#anova test
# Perform a two-sample t-test cannot be run, because there are more than two groups. Anova is used instead
#t.test(mean ~ Group, data = RATSSL8S1, var.equal = TRUE)
# Add the baseline from the original data as a new variable to the summary data
RATSSL8S2 <- RATSSL8S %>%
  mutate(baseline = RATS$Time1)
# Fit the linear model with the mean as the response 
fit <- lm(mean ~ Group, data = RATSSL8S2)

# Compute the analysis of variance table for the fitted model with anova()
anova(fit)
confint(fit)

#Liner model, just for fun
simple_linear_model_rats <- lm(Weight ~ Group + Time, data = RATSL)
summary(simple_linear_model_rats)

