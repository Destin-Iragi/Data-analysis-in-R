### DATA ANALYSIS FOR DIFFRENT EXPERIMENTAL DESIGNS ######

# Load package needed #
library(emmeans)
library(nlme)
library(agricolae)
library(dplyr)
library(broom)
library(openxlsx)
library(multcomp)
library(multcompView)
library(augmentedRCBD)


####Field layout for designs and Randomization###
library(FielDHub)
run_app()

library(FielDHub)
run_app()

###CRD analysis###
###Load Data#####
CRD<-read.csv("CRD.csv", header = T)

# View data and structure
View(CRD)
str(CRD)

##Factors###
CRD$Variety<-as.factor(CRD$Variety)
CRD$Variety<-as.factor(CRD$Variety)

# Perform ANOVA
model <- aov(CRD$Yield ~ Variety, data = CRD)
model <- aov(CRD$Yield ~ Variety, data = CRD)
summary(model)


# Means and standard errors
cv.model(model)
compare1 <-LSD.test (model, c ("Variety"),console=TRUE)
crd_result <- LSD.test(model, "Variety", p.adj="bonferron")
print(crd_result)


# Plot means
bar.group(crd_result$groups,
          ylim = c(0, 3),
          ylab = "Mean Yield (kg/ha)",
          main = "Cowpea Yield (CRD)",
          col = "lightgreen")


###RCBD Analysis###
##Load data##
RCBD<-read.csv("RCBD.csv", header = T)

# View data
View(RCBD)

###Factors####
RCBD$Block<-as.factor(RCBD$Block)
RCBD$Variety<-as.factor(RCBD$Variety)

str(RCBD)

# Perform ANOVA
model <- aov(RCBD$Yield ~ Block + Variety, data = RCBD)
summary(model)

sink("Yield.txt")
model_RCBD <- aov(RCBD$Yield ~ Block + Variety, data = RCBD)
summary(model_RCBD)
cv.model(model_RCBD)

# Means and LSD test
compare1 <-LSD.test (model_RCBD, c ("Variety"),console=TRUE)
sink()

# Others to try #
rcbd_result <- LSD.test(model, "Variety", p.adj = "none")
rcbd_result <- LSD.test(model, "Variety", p.adj = "bonferroni")
rcbd_result <- LSD.test(model, "Variety", p.adj = "fdr")
print(rcbd_result)

# Bar plot of means
bar.group(rcbd_result$groups,
          ylim = c(0, 6),
          ylab = "Mean Yield (t/ha)",
          main = "Maize Yield (RCBD)",
          col = "lightblue")



# LSD #
# Loead data #
LSD<-read.csv("LSD.csv", header = T)

# Quick view
View(LSD)
LSD$Row<-as.factor(LSD$Row)
LSD$Col<-as.factor(LSD$Col)
LSD$Treatment<-as.factor(LSD$Treatment)

# --- ANOVA for Latin square ----------------------------------
# Note: include Row and Col as blocking factors
model_lsd <- aov(Yield ~ Row + Col + Treatment, data = LSD)
summary(model_lsd)

# --- Treatment means and comparison --------------------------
# treatment means
tapply(LSD$Yield, LSD$Treatment, mean)

# If you want pairwise comparisons use agricolae or emmeans
lsd_test <- LSD.test(model_lsd, "Treatment", p.adj="none")
print(lsd_test$groups)   # groups with letters

# alternatively use emmeans
em <- emmeans(model_lsd, ~ Treatment)
pairs(em, adjust = "tukey")

# Means and LSD test
compare1 <-LSD.test (model_lsd, c ("Treatment"),console=TRUE)



#####SPlit plot########
###Load data sample####
split<-read.csv("Split.csv", header = T)

# Quick check
print(split)

str(split)

# Factors #
split$Rep<-as.factor(split$Rep)
split$MainPlot<as.factor(split$MainPlot)
split$Irrigation<-as.factor(split$Irrigation)
split$Variety<-as.factor(split$Variety)


# Table layout for visualization
with(split, table(Rep, Irrigation, Variety))

# --- 1) Classical aov with Error strata ---------------------
# Use nested error: main plot nested in replication
# Note: the Error term handles the two error strata implicitly
model_aov <- aov(Yield ~ Irrigation * Variety + Error(Rep/MainPlot), data = split)
summary(model_aov)

# --- 2) Mixed-model approach (recommended) ------------------
# Model as mixed-effects: random intercepts for Rep and Rep:MainPlot
model_lme <- lme(Yield ~ Irrigation * Variety,
                 random = ~1 | Rep/MainPlot,
                 data = split,
                 method = "REML")
summary(model_lme)
anova(model_lme)   # Type I tests; for Type III use car::Anova on lm/list


# --- Estimated marginal means and pairwise comparisons ------
# Varieties within each irrigation level
emm_int <- emmeans(model_lme, ~ Variety | Irrigation)
emm_int
pairs(emm_int, adjust = "tukey")  # pairwise comparisons within irrigation


# Main effects (marginal)
emmeans(model_lme, ~ Irrigation)
emmeans(model_lme, ~ Variety)


# Optional: simple means table #
split %>% group_by(Irrigation, Variety) %>%
  summarise(Mean = mean(Yield), SD = sd(Yield), n = n()) %>%
  print()



####SPlit-Plit Plot######

Split_split<-read.csv("Split split.csv", header=T)

head(Split_split)
####Classical ANOVA using aov() with nested error###
# Specify nested error terms for each level
model_ssp <- aov(Yield ~ Irrigation * Fertilizer * Variety +
                   Error(Rep/Irrigation/Fertilizer),
                 data = Split_split)
summary(model_ssp)

###Mixed-model using nlme::lme() (modern approach)####
model_ssp_lme <- lme(
  Yield ~ Irrigation * Fertilizer * Variety,
  random = ~1 | Rep/Irrigation/Fertilizer,
  data = Split_split
)

anova(model_ssp_lme)
summary(model_ssp_lme)


# Interaction means
emm_ip <- emmeans(model_ssp_lme, ~ Variety | Irrigation * Fertilizer)
pairs(emm_ip, adjust = "tukey")

# Plot of interaction means
plot(emm_ip, comparisons = TRUE)


####Mean summary######
Split_split %>%
  group_by(Irrigation, Fertilizer, Variety) %>%
  summarise(Mean = mean(Yield), SD = sd(Yield), .groups = "drop")


######Facrorial Designs####
# Load data #
factorial<-read.csv("Factorial.csv", header = T)

head(factorial)
str(factorial)

factorial$Rep<-as.factor(factorial$Rep)
factorial$Fertilizer<-as.factor(factorial$Fertilizer)
factorial$Density<-as.factor(factorial$Density)

# --- Two-way factorial ANOVA with blocking ---
model_fac <- aov(Yield ~ Rep + Fertilizer * Density, data = factorial)
summary(model_fac)
cv.model(model_fac)
compare1 <-LSD.test (model_fac, c ("Fertilizer"),console=TRUE)
compare1 <-LSD.test (model_fac, c ("Density"),console=TRUE)
compare1 <-LSD.test (model_fac, c ("Fertilizer","Density"),console=TRUE)


factorial %>%
  group_by(Fertilizer, Density) %>%
  summarise(Mean = mean(Yield), SD = sd(Yield), .groups = "drop")


interaction.plot(
  x.factor = factorial$Fertilizer,
  trace.factor = factorial$Density,
  response = factorial$Yield,
  type = "b",
  col = c("blue", "red"),
  pch = 19,
  xlab = "Fertilizer",
  ylab = "Mean Yield",
  trace.label = "Density"
)


#####ALpha Lattice Design####
df<-read.csv("Alpha.csv", header = T)


# Quick look
df %>% group_by(Treatment) %>% summarise(n = n(), Mean = mean(Yield), SD = sd(Yield)) %>% head()

# Treat Block nested in Rep as fixed factor here just to see sums of squares.
# Note: In practice, blocks are random and you should prefer mixed models.
aov_model <- aov(Yield ~ Rep + Rep:Block + Treatment, data = df)
summary(aov_model)


# If you want treatment LS means from the aov:
tapply(df$Yield, df$Treatment, mean)

# --- Recommended: Mixed model (nlme) -------------------------
# Fit Treatment fixed; Rep and Block (nested in Rep) random
# In nlme, use random = ~1 | Rep/BlockID
lme_model <- lme(Yield ~ Treatment,
                 random = ~1 | Rep/BlockID,
                 data = df,
                 method = "REML",
                 control = lmeControl(opt = "optim"))

summary(lme_model)
anova(lme_model)   # tests for fixed effect Treatment (Type I by nlme)


# Variance components
VarCorr(lme_model)


# --- Estimated marginal means (adjusted means) ---------------
emm_treat <- emmeans(lme_model, ~ Treatment)
emm_treat
# pairwise comparisons with Tukey adjustment
pairs(emm_treat, adjust = "tukey")

# Compact letter display for grouping
cld(emm_treat, Letters = letters)

#####Augmented Design#####
###Import data 
emma <-read.csv("Augmented.csv", header = TRUE)
str(emma)
View(emma)

###Convert to factors and numeric
emma$Genotype<-as.factor(emma$Genotype)
emma$Block<-as.factor(emma$Block)
emma$Yield<-as.numeric(emma$Yield)

str(emma)
summary(emma)

names(emma)

###ANOVA for augmented RCBD#####
kofi <- augmentedRCBD.bulk(data = emma, block = "Block", treatment = "Genotype", 
                           traits = c("Yield"),
                           checks = NULL, alpha = 0.05, describe = TRUE, freqdist = TRUE, gva = TRUE,
                           check.col = c("brown", "forestgreen","purple"),console = FALSE)

sink("Soy_Manga_24")
kofi
sink()
report.augmentedRCBD.bulk(kofi, file.path(('/Users/peniel/Desktop/Kabanyolo class'),"Serere.docx"))

'/Users/peniel/Desktop/Kabanyolo class'

'\Users\peniel\Desktop\Kabanyolo class'