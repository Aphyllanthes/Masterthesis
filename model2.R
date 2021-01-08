

source("datenschau.R")
Artenzahl_Par %>% ggplot(aes(Parasitoid)) + geom_histogram()
A2 %>% ggplot(aes(imp1000, Parasitoid)) + geom_point()

sub_IH <- A2 %>% 
  rename(Insektenhaus = Isnsektenhaus) %>% 
  filter(!is.na(Insektenhaus)) %>% 
  mutate(Insektenhaus = as.factor(Insektenhaus))

sub_IH %>% ggplot(aes(Insektenhaus, Parasitoid)) +
  geom_boxplot()

fit.exp <- fitdistrplus::fitdist(sub_IH$Parasitoid, "exp")
fit.cauchy <- fitdistrplus::fitdist(sub_IH$Parasitoid, "cauchy")
fit.geom <- fitdistrplus::fitdist(sub_IH$Parasitoid, "geom")
fit.logis <- fitdistrplus::fitdist(sub_IH$Parasitoid, "logis")
fit.nbinom <- fitdistrplus::fitdist(sub_IH$Parasitoid, "nbinom")
fit.norm <- fitdistrplus::fitdist(sub_IH$Parasitoid, "norm")
fit.pois <- fitdistrplus::fitdist(sub_IH$Parasitoid, "pois")

aics <- c(fit.exp$aic,
          fit.cauchy$aic,
          fit.geom$aic,
          fit.logis$aic,
          fit.nbinom$aic,
          fit.norm$aic,
          fit.pois$aic) 
which(aics == min(aics)) ## exp


## 0.1 einfluss von Insektenhaus auf artenzahl: ####
model_IH0 <- MASS::glm.nb(Parasitoid ~ imp1000 + I(imp1000^2), data = sub_IH) 
model_IH1 <- MASS::glm.nb(Parasitoid ~ imp1000 + I(imp1000^2) + Insektenhaus + Insektenhaus*imp1000, data = sub_IH)  ## nicht signifikant
model_IH2 <- MASS::glm.nb(Parasitoid ~ imp1000 + I(imp1000^2) + Insektenhaus, data = sub_IH)
model_IH2_100 <- MASS::glm.nb(Parasitoid ~ imp100 + I(imp100^2) + Insektenhaus, data = sub_IH)
model_IH2_250 <- MASS::glm.nb(Parasitoid ~ imp250 + I(imp250^2) + Insektenhaus, data = sub_IH)
model_IH2_500 <- MASS::glm.nb(Parasitoid ~ imp500 + I(imp500^2) + Insektenhaus, data = sub_IH)
model_IH2_2000 <- MASS::glm.nb(Parasitoid ~ imp2000 + I(imp2000^2) + Insektenhaus, data = sub_IH)
model_IH2a <- MASS::glm.nb(Parasitoid ~ imp1000 + I(imp1000^2) + Insektenhaus + len + class_nr, data = sub_IH) 
model_IH2b <- MASS::glm.nb(Parasitoid ~ imp1000 + I(imp1000^2) + Insektenhaus + class_nr + mean_annual_temp_celsius + annual_precipitation_mm + altitude_town_m + Arable + Pastures + Forest, data = sub_IH) #
model_IH2c <- step(model_IH2b)
model_IH3 <- glm(Parasitoid ~ imp1000 + I(imp1000^2) + Insektenhaus, data = sub_IH, family = "poisson")
model_IH4 <- glm(Parasitoid ~ Insektenhaus, data = sub_IH, family = "poisson")
model_IH5 <- MASS::glm.nb(Parasitoid ~ Insektenhaus, data = sub_IH) # schwach signifikant

model_IH2_gauss <- glm(Parasitoid ~ imp1000 + I(imp1000^2) + Insektenhaus, data = sub_IH)
model_IH5_gauss <- glm(Parasitoid ~ Insektenhaus, data = sub_IH)
model_IH2_250pois <- glm(Parasitoid ~ imp250 + I(imp250^2) + Insektenhaus, family = "poisson", data = sub_IH)

AIC(model_IH0, model_IH1, model_IH2, model_IH2a, model_IH2b, model_IH3, model_IH4, model_IH5, model_IH2_gauss, model_IH5_gauss)
AIC(model_IH2_250, model_IH2_250pois)

anova(model_IH0, model_IH2) # Insektenhaus verbessert das Modell
anova(model_IH2, model_IH2c) # step-model ist besser: model_IH2c
anova(model_IH5, model_IH2) # andererseits verbessert der Versiegelungsgrad das Modell nicht...
anova(model_IH5, model_IH2_100)  # aber  im 100m-Radius 
anova(model_IH5, model_IH2_250) # aber im 250m-Radius verbessert der Versiegelungsgrad das Modell
anova(model_IH5, model_IH2_2000) # aber nicht im 500- und 2000-m-Radius.

# model_IH2_250, model_IH5

# R^2: 
with(summary(model_IH0), 1 - deviance/null.deviance)
with(summary(model_IH1), 1 - deviance/null.deviance)
with(summary(model_IH2), 1 - deviance/null.deviance)
with(summary(model_IH2a), 1 - deviance/null.deviance)
with(summary(model_IH2b), 1 - deviance/null.deviance) # immerhin 11% erklärte Varianz
with(summary(model_IH2c), 1 - deviance/null.deviance) # immerhin 8%
with(summary(model_IH3), 1 - deviance/null.deviance) # knapp 6 % 
with(summary(model_IH4), 1 - deviance/null.deviance)
with(summary(model_IH4), 1 - deviance/null.deviance)

## der einzige Versiegelungsradius, der das Modell mit Insektenhaus verbessert ist 100m und 250m

# folgende Modelle werden untersucht:
# - (modelIH_2) model_IH5, model_IH2_250

library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = model_IH5_gauss, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model_IH4, plot = T)

simulationOutput <- simulateResiduals(fittedModel = model_IH2_250, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model_IH5, plot = T)


### 0.1 DHARMa #######
#simulationOutput <- simulateResiduals(fittedModel = model_IH0, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model_IH2_250, plot = T)
testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput) ## 
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)
par(mfrow = c(1,1))
plotResiduals(simulationOutput, sub_IH$imp250)
plotResiduals(simulationOutput, sub_IH$Insektenhaus)
testSpatialAutocorrelation(simulationOutput, 
                           x = left_join(sub_IH, Artenzahl_autocor, by = "location")$lon,
                           y = left_join(sub_IH, Artenzahl_autocor, by = "location")$lat)

# gutes Modell! model_IH2_250

#simulationOutput <- simulateResiduals(fittedModel = model_IH0, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model_IH2_250pois, plot = T)
testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput) ## 
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)
par(mfrow = c(1,1))
plotResiduals(simulationOutput, sub_IH$imp250)
plotResiduals(simulationOutput, sub_IH$Insektenhaus)
testSpatialAutocorrelation(simulationOutput, 
                           x = left_join(sub_IH, Artenzahl_autocor, by = "location")$lon,
                           y = left_join(sub_IH, Artenzahl_autocor, by = "location")$lat)

#  model_IH2_250pois disperion problem!!!

simulationOutput <- simulateResiduals(fittedModel = model_IH5, plot = T)
testDispersion(simulationOutput) # p= 0.86
testOutliers(simulationOutput) ## 0.7
testUniformity(simulationOutput) # 0.74
testZeroInflation(simulationOutput) # p =0.32
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)
plotResiduals(simulationOutput, sub_IH$Insektenhaus)
testSpatialAutocorrelation(simulationOutput, 
                           x = left_join(sub_IH, Artenzahl_autocor, by = "location")$lon,
                           y = left_join(sub_IH, Artenzahl_autocor, by = "location")$lat)
# model_IH5 passt auch

# testing poisson-model
simulationOutput <- simulateResiduals(fittedModel = model_IH4, plot = T)
# poisson fliegt raus

# testing gaussian-model
simulationOutput <- simulateResiduals(fittedModel = model_IH5_gauss, plot = T)
# gaussian fliegt raus

## 0.1.1 summary ########
sjPlot::plot_model(model_IH2_250, type = "pred", terms = c("Insektenhaus", "imp250"), 
                   show.p = T, show.data = T, jitter = T) 
sjPlot::tab_model(model_IH2_250, model_IH5, 
                  dv.labels  = c("nb-model mit Versiegelungsgrad", "nb-model ohne Versiegelunsgrad")) 

#jtools::plot_summs(model_IH2_250, plot.distributions = T, scale = T)


# 1. Artenzahl Parasiten #############

### 1.1 select predictors, check for correlating predictors ######
## imp100:
A2_100 <- A2 %>% 
  select(-imp250, -imp500, -imp1000, -imp2000) %>% 
  #select(- Parasitoid, - Abundanz) %>% 
  select(-DistInsektenhaus, -Isnsektenhaus)

A2_100 <- A2_100 %>% 
  select(-Urban)

## imp250:
A2_250 <- A2 %>% 
  select(-imp100, -imp500, -imp1000, -imp2000) %>% 
  #select(- Parasitoid, - Abundanz) %>% 
  select(-DistInsektenhaus, -Isnsektenhaus)

A2_250 <- A2_250 %>% 
  select(-Urban)

## imp500:
A2_500 <- A2 %>% 
  select(-imp100, -imp250, -imp1000, -imp2000) %>% 
  #select(- Parasitoid, - Abundanz) %>% 
  select(-DistInsektenhaus, -Isnsektenhaus)

A2_500 <- A2_500 %>% 
  select(-Urban, -len)

## imp1000:
A2_1000 <- A2 %>% 
  select(-imp100, -imp250, -imp500, -imp2000) %>% 
  #select(- Parasitoid, - Abundanz) %>% 
  select(-DistInsektenhaus, -Isnsektenhaus)

A2_1000 <- A2_1000 %>% 
  select(-Urban, -len)

## imp2000:
A2_2000 <- A2 %>% 
  select(-imp100, -imp250, -imp500, -imp1000) %>% 
  #select(- Parasitoid, - Abundanz) %>% 
  select(-DistInsektenhaus, -Isnsektenhaus)

A2_2000 <- A2_2000 %>% 
  select(-Urban)

##1.2 Verteilung testen#######

fit.exp <- fitdistrplus::fitdist(A2$Parasitoid, "exp")
fit.cauchy <- fitdistrplus::fitdist(A2$Parasitoid, "cauchy")
fit.geom <- fitdistrplus::fitdist(A2$Parasitoid, "geom")
fit.logis <- fitdistrplus::fitdist(A2$Parasitoid, "logis")
fit.nbinom <- fitdistrplus::fitdist(A2$Parasitoid, "nbinom")
fit.norm <- fitdistrplus::fitdist(A2$Parasitoid, "norm")
fit.pois <- fitdistrplus::fitdist(A2$Parasitoid, "pois")

aics <- c(fit.exp$aic,
    fit.cauchy$aic,
    fit.geom$aic,
    fit.logis$aic,
    fit.nbinom$aic,
    fit.norm$aic,
    fit.pois$aic)

which(aics == min(aics)) ## exp
## AIC soll möglichst niedrig sein.
## exponential passt am besten


## Modell Artenzahl gegen versiegelungsgrad:

## 1.3 Modell Artenzahl gegen versiegelungsgrad: ##########
### 1.3.1 imp100 #######
model0 <- MASS::glm.nb(Parasitoid ~ . + I(imp100^2), data = A2_100[,-c(1,2)]) ##
model0_step <- step(model0)
model1 <- MASS::glm.nb(Parasitoid ~ imp100 + I(imp100^2), data = A2) ## nicht signifikant
model1a <- MASS::glm.nb(Parasitoid ~ I(imp100^2), data = A2) ## signifikant !!
AIC(model0, model0_step, model1, model1a) # alle gleich, model0 etwas schlechter

anova(model1, model1a, test = "Chisq") # model1a ist ausreichend im Vergleich zu model1
anova(model0_step, model1a, test = "Chisq") # model1a ausreichend

summary(model0_step)

## DHARMa test
library(DHARMa)
testDispersion(model1a) #ok
testDispersion(model0_step) #ok
simulationOutput_a <- simulateResiduals(fittedModel = model0_step, plot = T)
simulationOutput_b <- simulateResiduals(fittedModel = model1a, plot = T)

testDispersion(simulationOutput_a)
testDispersion(simulationOutput_b)
testOutliers(simulationOutput_a)
testOutliers(simulationOutput_b)
testUniformity(simulationOutput_a)
testUniformity(simulationOutput_b)
testZeroInflation(simulationOutput_a)
testZeroInflation(simulationOutput_b)
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput_a, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_a, summary = spread, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput_a)
testQuantiles(simulationOutput_b)
plotResiduals(simulationOutput_a, A2_100$imp100)
plotResiduals(simulationOutput_a, A2_100$annual_precipitation_mm)
plotResiduals(simulationOutput_b, I(A2_100$imp100^2))
testSpatialAutocorrelation(simulationOutput_a, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)
testSpatialAutocorrelation(simulationOutput_b, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)

summary(model1a) # model1a wird ausgewählt

ggplot(A2_100, aes(imp100, Parasitoid)) + geom_point() +
  geom_smooth(method = MASS::glm.nb, se = T, formula = y ~  I(x^2))

### 1.3.2 imp250 #####
model2a <- MASS::glm.nb(Parasitoid ~ . + I(imp250^2), data = A2_250[,-c(1,2)]) ##
model2a_step <- step(model2a)

model2b <- MASS::glm.nb(Parasitoid ~ imp250 + I(imp250^2), data = A2_250) ## nicht signifikant
model2c <- MASS::glm.nb(Parasitoid ~ I(imp250^2), data = A2_250) ## signifikant!!

AIC(model2a, model2a_step, model2b, model2c) # model2a ist am schlechtesten, alle anderen ok
anova(model2b, model2c) # nicht signifikant, also einfacherers Modell
anova(model2a_step, model2c) # knapp signifikant, also komplexeres Model = model2a_step

## DHARMa test
library(DHARMa)
testDispersion(model2c) #ok
testDispersion(model2a_step) #ok
simulationOutput_a <- simulateResiduals(fittedModel = model2c, plot = T)
simulationOutput_b <- simulateResiduals(fittedModel = model2a_step, plot = T)

testDispersion(simulationOutput_a)
testDispersion(simulationOutput_b)
testOutliers(simulationOutput_a)
testOutliers(simulationOutput_b)
testUniformity(simulationOutput_a)
testUniformity(simulationOutput_b) # b viel besser
testZeroInflation(simulationOutput_a)
testZeroInflation(simulationOutput_b)
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput_a, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_a, summary = spread, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput_a) # a besser
testQuantiles(simulationOutput_b)
plotResiduals(simulationOutput_a, I(A2_250$imp250^2))
plotResiduals(simulationOutput_b, A2_250$imp250)
plotResiduals(simulationOutput_b, A2_250$annual_precipitation_mm)
plotResiduals(simulationOutput_b, A2_250$Arable)
plotResiduals(simulationOutput_b, A2_250$Pastures)
plotResiduals(simulationOutput_b, A2_250$Forest)

testSpatialAutocorrelation(simulationOutput_a, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)
testSpatialAutocorrelation(simulationOutput_b, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)

## beide Modelle model2c und model2a_step schneiden gut ab
summary(model2a_step)
summary(model2c)

### 1.3.3 imp500 #####
model3a <- MASS::glm.nb(Parasitoid ~ . + I(imp500^2), data = A2_500[,-c(1,2)]) ##
model3a_step <- step(model3a)
model3b <- MASS::glm.nb(Parasitoid ~ imp500 + I(imp500^2), data = A2_500) ## nicht signifikant
model3c <- MASS::glm.nb(Parasitoid ~ I(imp500^2), data = A2_500) ## signifikant

AIC(model3a, model3a_step, model3b, model3c)
anova(model3b, model3c) # nicht signifikant, also einfacherers Modell
anova(model3a_step, model3c) # knapp signifikant, also beide Modelle evaluieren

## DHARMa test
testDispersion(model3a_step) #ok
testDispersion(model3c) #ok
simulationOutput_a <- simulateResiduals(fittedModel = model3a_step, plot = T)
simulationOutput_b <- simulateResiduals(fittedModel = model3c, plot = T)

testDispersion(simulationOutput_a)
testDispersion(simulationOutput_b)
testOutliers(simulationOutput_a)
testOutliers(simulationOutput_b)
testUniformity(simulationOutput_a)
testUniformity(simulationOutput_b) 
testZeroInflation(simulationOutput_a)
testZeroInflation(simulationOutput_b)
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput_a, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_a, summary = spread, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput_a) # a besser
testQuantiles(simulationOutput_b)
plotResiduals(simulationOutput_b, I(A2_500$imp500^2))
plotResiduals(simulationOutput_a, A2_500$imp500)
plotResiduals(simulationOutput_a, A2_500$annual_precipitation_mm)
plotResiduals(simulationOutput_a, A2_500$Arable)
plotResiduals(simulationOutput_a, A2_500$Pastures)
plotResiduals(simulationOutput_a, A2_500$Forest)

testSpatialAutocorrelation(simulationOutput_a, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)
testSpatialAutocorrelation(simulationOutput_b, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)


ggplot(A2_500, aes(imp500, Parasitoid)) + geom_point()

## beide Modelle model3c und model3a_step schneiden gut ab
summary(model3a_step)
summary(model3c)


### 1.3.4 imp1000 #####
model4a <- MASS::glm.nb(Parasitoid ~ . + I(imp1000^2), data = A2_1000[,-c(1,2)]) ##
model4a_step <- step(model4a)

model4b <- MASS::glm.nb(Parasitoid ~ imp1000 + I(imp1000^2), data = A2_1000) ## knapp nicht signifikant
model4c <- MASS::glm.nb(Parasitoid ~ I(imp1000^2), data = A2_1000) ## knapp nicht signifikant

AIC(model4a, model4a_step, model4b, model4c) # keine großen unterschiede
anova(model4b, model4c) # nicht signifikant also einfacheres Modell: model4c
# model4c = model4a_step, daher nur 4c in der evaluation

model4_pois <- glm(Parasitoid ~ imp1000 + I(imp1000^2), data = A2, family = "poisson") ## knapp  signifikant
model4a_pois <- glm(Parasitoid ~  I(imp1000^2), data = A2, family = "poisson") ## knapp nicht signifikant
AIC(model4_pois, model4a_pois)


## DHARMa test
testDispersion(model4c) #ok
simulationOutput <- simulateResiduals(fittedModel = model4_pois, plot = T) # fliegt raus
simulationOutput_b <- simulateResiduals(fittedModel = model4c, plot = T)

testDispersion(simulationOutput_b) # gleich
testOutliers(simulationOutput_b) 
testUniformity(simulationOutput_b) #
testZeroInflation(simulationOutput_b) # 
testGeneric(simulationOutput_b, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = spread, alternative = "two.sided") #
testQuantiles(simulationOutput_b) #

plotResiduals(simulationOutput_b, I(A2_1000$imp1000^2))
testSpatialAutocorrelation(simulationOutput_b, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat) #

## insgesamt in Ordnung aber kein gutes Modell
summary(model4c)
ggplot(A2_1000, aes(imp1000, Parasitoid)) + geom_point()+
  geom_smooth(method = MASS::glm.nb, se = T, formula = y ~  I(x^2))


### 1.3.5 imp2000 #####

model5a <- MASS::glm.nb(Parasitoid ~ . + I(imp2000^2), data = A2_2000[,-c(1,2)]) ##
model5a_step <- step(model5a)

model5b <- MASS::glm.nb(Parasitoid ~ imp2000 + I(imp2000^2), data = A2_2000) ## knapp nicht signifikant
model5c <- MASS::glm.nb(Parasitoid ~ I(imp2000^2), data = A2_2000) ## nicht signifikant

AIC(model5a, model5a_step, model5b, model5c) # keine großen Unterschiede
anova(model5b, model5c) # nicht signifikant also einfacheres Modell: model5c
# model5a_step ist das selbe wie model5b, daher wird model5c verwendet

## DHARMa test
testDispersion(model5c) #ok
simulationOutput_a <- simulateResiduals(fittedModel = model5c, plot = T)
# da model5c schlecht abschneidet im Residuentest wird auch model5a_step geprüft:
simulationOutput_b <- simulateResiduals(fittedModel = model5a_step, plot = T)

testDispersion(simulationOutput_a)
testDispersion(simulationOutput_b) # gleich
testOutliers(simulationOutput_a)
testOutliers(simulationOutput_b) # b ist besser
testUniformity(simulationOutput_a)
testUniformity(simulationOutput_b) # b besser
testZeroInflation(simulationOutput_a)
testZeroInflation(simulationOutput_b) # b etwas besser
testGeneric(simulationOutput_a, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_a, summary = spread, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = spread, alternative = "two.sided") #
testQuantiles(simulationOutput_a)
testQuantiles(simulationOutput_b)

plotResiduals(simulationOutput_a, I(A2_2000$imp2000^2))
plotResiduals(simulationOutput_b, I(A2_2000$imp2000^2)) # a sieht schöner aus
testSpatialAutocorrelation(simulationOutput_a, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)
testSpatialAutocorrelation(simulationOutput_b, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat) # a besser


# model5a_step shcneidet besser ab bei der Residuendiagnostik.
ggplot(A2_2000, aes(imp2000, Parasitoid)) + geom_point()

summary(model5c)
summary(model5a_step) # DHARMa identifiziert model5a_step als besseres Modell

### 1.3.6 summary ###########

sjPlot::tab_model(model1a,model2c, model2a_step, model3c, model3a_step, model4c,model5a_step,  
                  dv.labels = c("imp100", "imp250", "imp250", "imp500","imp500", "imp1000", "imp2000")) 

broom::tidy(model1a)
broom::tidy(model2c)
broom::tidy(model2a_step)
broom::tidy(model3c)
broom::tidy(model3a_step)
broom::tidy(model4c)
broom::tidy(model5a_step)

### ################

## plots
ggplot(A2_new, aes(imp100, Parasitoid)) + geom_point()
ggplot(A2_new, aes(imp100, log10(Parasitoid + 1))) + geom_point()


## 
ggplot(A2_new, aes(class_nr, Parasitoid)) + geom_point()
ggplot(A2_new, aes(len, Parasitoid)) + geom_point()
