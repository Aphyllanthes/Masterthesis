source("datenschau.R")
library(ggplot2)

## 0. Insektenhaus gegen abundanz: ##########
modelIH <- MASS::glm.nb(Abundanz ~ Insektenhaus, data = A1_IH_factor)
ggplot(A1_IH_factor, aes(Insektenhaus, Abundanz)) + geom_boxplot()


sub_IH <- A1_IH_factor %>% 
   filter(!is.na(Insektenhaus)) #%>% 
  # select(Abundanz,imp1000, Insektenhaus)
library(DHARMa)

model_IH0 <- MASS::glm.nb(Abundanz ~ imp1000 + I(imp1000^2), data = sub_IH) 
model_IH1 <- MASS::glm.nb(Abundanz ~ imp1000 + I(imp1000^2) + Insektenhaus + Insektenhaus*imp1000, data = sub_IH) 
model_IH2 <- MASS::glm.nb(Abundanz ~ imp1000 + I(imp1000^2) + Insektenhaus, data = sub_IH)
model_IH2_100 <- MASS::glm.nb(Abundanz ~ imp100 + I(imp100^2) + Insektenhaus, data = sub_IH)
model_IH2_250 <- MASS::glm.nb(Abundanz ~ imp250 + I(imp250^2) + Insektenhaus, data = sub_IH)
model_IH2_500 <- MASS::glm.nb(Abundanz ~ imp500 + I(imp500^2) + Insektenhaus, data = sub_IH)
model_IH2_2000 <- MASS::glm.nb(Abundanz ~ imp2000 + I(imp2000^2) + Insektenhaus, data = sub_IH)
model_IH2a <- MASS::glm.nb(Abundanz ~ imp1000 + I(imp1000^2) + Insektenhaus + len + class_nr, data = sub_IH) 
model_IH2b <- MASS::glm.nb(Abundanz ~ imp1000 + I(imp1000^2) + Insektenhaus + class_nr + mean_annual_temp_celsius + annual_precipitation_mm + altitude_town_m + Arable + Pastures + Forest, data = sub_IH) #
model_IH2c <- step(model_IH2b)
model_IH3 <- glm(Abundanz ~ imp1000 + I(imp1000^2) + Insektenhaus, data = sub_IH, family = "poisson")
model_IH4 <- glm(Abundanz ~ Insektenhaus, data = sub_IH, family = "poisson")
model_IH5 <- MASS::glm.nb(Abundanz ~ Insektenhaus, data = sub_IH)

model_IH2_gauss <- glm(Abundanz ~ imp1000 + I(imp1000^2) + Insektenhaus, data = sub_IH)
model_IH5_gauss <- glm(Abundanz ~ Insektenhaus, data = sub_IH)

AIC(model_IH0, model_IH1, model_IH2, model_IH2a, model_IH2b, model_IH3, model_IH4, model_IH5)

AIC(model_IH2_100, model_IH2_250, model_IH2_500, model_IH2, model_IH2_2000) # keine relevanten Unterschiede


lmtest::lrtest(model_IH2, model_IH3) # auch Likelihood-ratio test findet glm.nb besser als poisson
lmtest::lrtest(model_IH2, model_IH2_gauss) # auch Likelihood-ratio test findet glm.nb besser als normalverteilung
anova(model_IH0, model_IH2) # Insektenhaus verbessert das Modell nicht!!
anova(model_IH0, model_IH2c)
anova(model_IH5, model_IH2) # andererseits verbessert auch der Versiegelungsgrad das Modell nicht...
anova(model_IH5, model_IH2_100)  # doch im 100m-Radius verbessert der Versiegelungsgrad das Modell
anova(model_IH5, model_IH2_250) # und im 250m-Radius auch
anova(model_IH5, model_IH2_2000) # aber nicht im 500- und 2000-m-Radius.

# durch das Modell erklärte Varianz:
with(summary(model_IH0), (null.deviance-deviance)/null.deviance)
with(summary(model_IH1), (null.deviance-deviance)/null.deviance)
with(summary(model_IH2), (null.deviance-deviance)/null.deviance)
with(summary(model_IH2a), (null.deviance-deviance)/null.deviance)
with(summary(model_IH2b), (null.deviance-deviance)/null.deviance)
with(summary(model_IH3), (null.deviance-deviance)/null.deviance)
with(summary(model_IH4), (null.deviance-deviance)/null.deviance)

anova(model_IH2b, model_IH2)

sjPlot::plot_model(model_IH2, type = "pred", terms = c("Insektenhaus", "imp1000"), 
                   show.p = T, show.data = T, jitter = T) 
sjPlot::tab_model(model_IH0, model_IH2, model_IH2b,model_IH3,model_IH5) 


jtools::plot_summs(model_IH2, plot.distributions = T, scale = T)

### 0.1 DHARMa #######
#simulationOutput <- simulateResiduals(fittedModel = model_IH0, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model_IH2, plot = T)
testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput) ## knapp!!!!
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)
par(mfrow = c(1,2))
plotResiduals(simulationOutput, sub_IH$imp1000)
plotResiduals(simulationOutput, sub_IH$Insektenhaus)
testSpatialAutocorrelation(simulationOutput, 
                           x = left_join(sub_IH, Artenzahl_autocor)$lon,
                           y = left_join(sub_IH, Artenzahl_autocor)$lat)

## maybe zeroinflation is a problem

simulationOutput <- simulateResiduals(fittedModel = model_IH2b, plot = T)
testDispersion(simulationOutput)
testOutliers(simulationOutput) ##!!!!signifikant
testUniformity(simulationOutput)
testZeroInflation(simulationOutput) ## signifikant!!!!
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)
par(mfrow = c(1,2))
plotResiduals(simulationOutput, sub_IH$imp1000)
plotResiduals(simulationOutput, sub_IH$Insektenhaus)
testSpatialAutocorrelation(simulationOutput, 
                           x = left_join(sub_IH, Artenzahl_autocor)$lon,
                           y = left_join(sub_IH, Artenzahl_autocor)$lat)

# testing 2000m-Radius (100-m-Radius performs bad in DHARMa-diagnostics) model:
simulationOutput <- simulateResiduals(fittedModel = model_IH2_2000, plot = T)
testDispersion(simulationOutput)
testOutliers(simulationOutput) 
testUniformity(simulationOutput)
testZeroInflation(simulationOutput) ## signifikant!!!!
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput) ######!!!
#par(mfrow = c(1,2))
plotResiduals(simulationOutput, sub_IH$imp1000)
plotResiduals(simulationOutput, sub_IH$Insektenhaus)
testSpatialAutocorrelation(simulationOutput, 
                           x = left_join(sub_IH, Artenzahl_autocor)$lon,
                           y = left_join(sub_IH, Artenzahl_autocor)$lat)

## summary: model_IH2 ist geradeso in Ordnung und von allen modllierten Modellen das Beste.

## Darstellung effektplot
# jtools::effect_plot(model_IH2, pred = Insektenhaus, interval = TRUE, plot.points = TRUE,
#                     jitter = .2) 
#   
# jtools::effect_plot(model_IH2, pred = Insektenhaus, interval = TRUE, partial.residuals = TRUE,
#             jitter = .2)
# 
# 
# jtools::effect_plot(modelIH, pred = Insektenhaus, interval = TRUE, plot.points = TRUE,
#                     jitter = .2)
# jtools::effect_plot(modelIH, pred = Insektenhaus, interval = TRUE, partial.residuals = TRUE,
#                     jitter = .2)
# 

## Insektenhaus hat keinen Effekt auf die Abundanz - weder alleine noch im Modell mit dem Versiegelungsgrad (bei poisson-Verteilung hätte es einen Effekt gegeben - aber dieses ist nach AIC viel schlechter und nach dharma-residuals auch grottig.)

## 0.2 Plots #######
ggplot(sub_IH, aes(imp1000, Abundanz, col = Insektenhaus)) + geom_point()+
  geom_smooth(method = "glm", formula = 'y~x',  method.args = list(family = "poisson"))

formula <- y ~ x + I(x^2)  # formula(model_IH2)
formula1 <- y ~ I(x^2)
ggplot(sub_IH, aes(imp1000, Abundanz)) + geom_point() +
  geom_smooth(method = MASS::glm.nb, aes(color = Insektenhaus), se = T, formula = formula)
# +
#   geom_smooth(method = MASS::glm.nb, aes(color = "NB"), se = T, formula = formula1, col = "green")


ggplot(sub_IH, aes(Insektenhaus, Abundanz)) + #geom_point()+ #, col = imp1000
  geom_point() +
  geom_smooth(method =  MASS::glm.nb, formula = 'y~x')



# manually calculate and plot confidence interval
sub_IH$model <- stats::predict(model_IH2, newdata=sub_IH, type = "response")
err <- stats::predict(model_IH2, newdata=sub_IH, se = TRUE, type = "response")
sub_IH$ucl <- err$fit +  err$se.fit
sub_IH$lcl <- err$fit -  err$se.fit

cI <- sub_IH %>% group_by(Insektenhaus) %>% 
  summarize(min_ucl = min(ucl, na.rm = T),
            mean_ucl = mean(ucl, na.rm = T),
            sd_ucl = sd(ucl, na.rm = T),
            max_ucl = max(ucl, na.rm = T),
            min_lcl = min(lcl, na.rm = T),
            mean_lcl = mean(lcl, na.rm = T) ,
            sd_lcl = sd(lcl, na.rm = T),
            max_lcl = max(lcl, na.rm = T),
            model_mean = mean(model, na.rm = T))

g <- ggplot(sub_IH) + geom_jitter(aes(x=Insektenhaus, y = Abundanz), size = 1, colour = "darkgrey") +
  geom_errorbar(data = cI, aes(x = Insektenhaus, ymin=min_lcl, ymax=max_ucl), alpha=0.8, size = 1) +
  geom_point(data = cI, aes(x = Insektenhaus, y = model_mean), size = 3) +
  theme_bw()
g


#sjPlot::plot_model(model_IH2, type = "pred", terms = c("imp1000", "Insektenhaus"))

sjPlot::plot_model(model_IH2, type = "pred", terms = c("Insektenhaus", "imp1000"), 
                   show.p = T, show.data = T, jitter = T) 
  


##1.  Modelle zu Abundanz (Brutzellen): ##############

### 1.1 Verteilung der Abundanz testen: ##########
A1 %>% ggplot(aes(Abundanz)) + geom_histogram()

fit.exp <- fitdistrplus::fitdist(A1$Abundanz, "exp")
fit.cauchy <- fitdistrplus::fitdist(A1$Abundanz, "cauchy")
fit.geom <- fitdistrplus::fitdist(A1$Abundanz, "geom")
fit.logis <- fitdistrplus::fitdist(A1$Abundanz, "logis")
fit.nbinom <- fitdistrplus::fitdist(A1$Abundanz, "nbinom")
fit.norm <- fitdistrplus::fitdist(A1$Abundanz, "norm")
fit.pois <- fitdistrplus::fitdist(A1$Abundanz, "pois")

c(fit.exp$aic,
  fit.cauchy$aic,
  fit.geom$aic,
  fit.logis$aic,
  fit.nbinom$aic,
  fit.norm$aic,
  fit.pois$aic) ==
  min(fit.exp$aic,
      fit.cauchy$aic,
      fit.geom$aic,
      fit.logis$aic,
      fit.nbinom$aic,
      fit.norm$aic,
      fit.pois$aic)
## AIC soll möglichst niedrig sein.
## exp passt am besten


## 1.2 Modell Abundanz gegen versiegelungsgrad:

### 1.2.1 imp100 #######
model0 <- MASS::glm.nb(Abundanz ~ . + I(imp100^2), data = A1_100[,-c(1,2)]) ##
model0_step <- step(model0)
model1 <- MASS::glm.nb(Abundanz ~ imp100 + I(imp100^2), data = A1) ## nicht signifikant
model1a <- MASS::glm.nb(Abundanz ~ I(imp100^2), data = A1) ## signifikant !!
AIC(model0, model0_step, model1, model1a) # alle gleich,

anova(model1, model1a, test = "Chisq") # model1a ist ausreichend im Vergleich zu model1
anova(model0_step, model1a, test = "Chisq") #knapp signifikant Unterschiedlich - daher könnte auch das komplexere Modell verwendet werden - das ist model0_step

summary(model1a)
with(summary(model1a), 1 - deviance/null.deviance) # R^2

## DHARMa test
library(DHARMa)
testDispersion(model1a) #ok
simulationOutput <- simulateResiduals(fittedModel = model1a, plot = T)

testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput) ## knapp 
testZeroInflation(simulationOutput) ## signifikant!!!!!
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)

plotResiduals(simulationOutput, A1_100$imp100)
#plotResiduals(simulationOutput, A1_100$annual_precipitation_mm)
testSpatialAutocorrelation(simulationOutput, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)

formula =  y ~ I(x^2)

ggplot(A1_100, aes(imp100, Abundanz)) + geom_point() +
  geom_smooth(method = MASS::glm.nb, se = T, formula = formula)

#ggplot(A1_100, aes(annual_precipitation_mm, Artenanzahl_det)) + geom_point()

## zeroInflation-Problem mit glmmTMB zu lösen geht nicht, weil glmmTMB fehler produziert mit nb ....
library(glmmTMB)
fittedModel <- glmmTMB(Abundanz ~ I(imp100^2), ziformula = ~1 , family = nbinom2, data = A1_100)
summary(fittedModel)

### 1.2.2 imp250 #####
model2a <- MASS::glm.nb(Abundanz ~ . + I(imp250^2), data = A1_250[,-c(1,2)]) ##
model2a_step <- step(model2a)

model2b <- MASS::glm.nb(Abundanz ~ imp250 + I(imp250^2), data = A1_250) ## nicht signifikant
model2c <- MASS::glm.nb(Abundanz ~ I(imp250^2), data = A1_250) ## signifikant!!

AIC(model2a, model2a_step, model2b, model2c) # model2a ist am schlechtesten, alle anderen ok
anova(model2b, model2c) # nicht signifikant, also einfacherers Modell
anova(model2a_step, model2c) # knapp nichst signifikant, also einfacheres Model = model2c

## DHARMa test
library(DHARMa)
testDispersion(model2c) #ok
#testDispersion(model2a_step) #ok
simulationOutput <- simulateResiduals(fittedModel = model2c, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model2a_step, plot = T)

testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput) ## knapp...
testZeroInflation(simulationOutput) ## signifikant!!!
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)

plotResiduals(simulationOutput, I(A1_250$imp250)^2)
testSpatialAutocorrelation(simulationOutput, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)

formula =  y ~ I(x^2)
ggplot(A1_250, aes(imp250, Abundanz)) + geom_point() +
  geom_smooth(method = MASS::glm.nb, se = T, formula = formula)

summary(model2c)

### 1.2.3 imp500 #####
model3a <- MASS::glm.nb(Abundanz ~ . + I(imp500^2), data = A1_500_test[,-c(1,2)]) ##
model3a_step <- step(model3a)
model3b <- MASS::glm.nb(Abundanz ~ imp500 + I(imp500^2), data = A1_500_test) ##  signifikant
model3c <- MASS::glm.nb(Abundanz ~ I(imp500^2), data = A1_500_test) ## signifikant

AIC(model3a, model3a_step, model3b, model3c)
anova(model3b, model3c) # nicht signifikant, also einfacherers Modell
anova(model3a_step, model3c) # knapp signifikant, also komplexeres Modell = model3a_step

## DHARMa test
testDispersion(model3a_step) #ok
testDispersion(model3c) #ok
simulationOutput <- simulateResiduals(fittedModel = model3a_step, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model3c, plot = T)

testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput) ## signifikant !!!
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)

plotResiduals(simulationOutput, I(A1_500$imp500^2)) ## quantile deviations!!!
plotResiduals(simulationOutput, A1_500$altitude_town_m) 
plotResiduals(simulationOutput, A1_500$Arable)  ## quantile deviations!!!
plotResiduals(simulationOutput, A1_500$Forest) 
testSpatialAutocorrelation(simulationOutput, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)


formula =  y ~ I(x^2)
ggplot(A1_500, aes(imp500, Abundanz)) + geom_point() +
  geom_smooth(method = MASS::glm.nb, se = T, formula = formula)

summary(model3a_step)

testDispersion(model3c) #ok
simulationOutput <- simulateResiduals(fittedModel = model3c, plot = T)

testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput) ## signifikant !!!
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)

plotResiduals(simulationOutput, I(A1_500$imp500^2)) ## quantile deviations!!!

testSpatialAutocorrelation(simulationOutput, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)


formula =  y ~ I(x^2)
ggplot(A1_500, aes(imp500, Abundanz)) + geom_point() +
  geom_smooth(method = MASS::glm.nb, se = T, formula = formula)

summary(model3c)
## model3a_step schneidet in DHARMa-check etwas schlechter ab als model3c, aber model3c erklärt mehr der unerklärten varianz.

### 1.2.4 imp1000 #####
model4a <- MASS::glm.nb(Abundanz ~ . + I(imp1000^2), data = A1_1000[,-c(1,2)]) ##
model4a_step <- step(model4a)

model4b <- MASS::glm.nb(Abundanz ~ imp1000 + I(imp1000^2), data = A1_1000) ## knapp nicht signifikant
model4c <- MASS::glm.nb(Abundanz ~ I(imp1000^2), data = A1_1000) ## knapp nicht signifikant

AIC(model4a, model4a_step, model4b, model4c) # keine großen unterschiede
anova(model4b, model4c) # knapp signifikant also komplexeres Modell: model4b
anova(model4b, model4a_step) # knapp nicht signifikant - beide Modelle werden DHARMa-getestet

## DHARMa test
testDispersion(model4a_step) #ok
testDispersion(model4b) #ok
simulationOutput_a <- simulateResiduals(fittedModel = model4a_step, plot = T)
simulationOutput_b <- simulateResiduals(fittedModel = model4b, plot = T)

testDispersion(simulationOutput_a)
testDispersion(simulationOutput_b) # gleich
testOutliers(simulationOutput_a)
testOutliers(simulationOutput_b) # a ist besser
testUniformity(simulationOutput_a)
testUniformity(simulationOutput_b) #a besser
testZeroInflation(simulationOutput_a) # signifikant
testZeroInflation(simulationOutput_b) # signifikant
testGeneric(simulationOutput_a, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_a, summary = spread, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = spread, alternative = "two.sided") #gleich
testQuantiles(simulationOutput_a)
testQuantiles(simulationOutput_b) # gleich

plotResiduals(simulationOutput_a, I(A1_1000$imp1000^2))
plotResiduals(simulationOutput_b, I(A1_1000$imp1000^2))
testSpatialAutocorrelation(simulationOutput_a, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)
testSpatialAutocorrelation(simulationOutput_b, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat) # a besser


ggplot(A1_1000, aes(imp1000, Artenanzahl_det)) + geom_point()

#summary(model4a_step)

formula =  y ~ x + I(x^2)
ggplot(A1_1000, aes(imp1000, Abundanz, col = altitude_town_m)) + geom_point() +
  geom_smooth(method = MASS::glm.nb, se = T, formula = formula)

summary(model4b)
summary(model4a_step) # DHARMa identifiziert kein deutlich schlechteres Modell, darum wird das einfacherer Modell verwendet: model4b. In model4a_step ist zusätzlich noch die Höhenlage.

### 1.2.5 imp2000 #####

model5a <- MASS::glm.nb(Abundanz ~ . + I(imp2000^2), data = A1_2000[,-c(1,2)]) ##
model5a_step <- step(model5a)

model5b <- MASS::glm.nb(Abundanz ~ imp2000 + I(imp2000^2), data = A1_2000) ## knapp signifikant
model5c <- MASS::glm.nb(Abundanz ~ I(imp2000^2), data = A1_2000) ## knapp nicht signifikant

AIC(model5a, model5a_step, model5b, model5c) # keine großen Unterschiede
anova(model5b, model5c) # signifikant also komplexeres Modell: model5b
anova(model5b, model5a_step) # knapp nicht signifikant - also einfacheres Modell = model5b


## DHARMa test
testDispersion(model5b) #ok
testDispersion(model5a_step) #ok
simulationOutput_a <- simulateResiduals(fittedModel = model5b, plot = T)
simulationOutput_b <- simulateResiduals(fittedModel = model5a_step, plot = T)

testDispersion(simulationOutput_a)
testDispersion(simulationOutput_b) # a besser
testOutliers(simulationOutput_a)
testOutliers(simulationOutput_b) # a ist besser
testUniformity(simulationOutput_a)
testUniformity(simulationOutput_b) # b besser
testZeroInflation(simulationOutput_a) # signifikant
testZeroInflation(simulationOutput_b) # signifikant
testGeneric(simulationOutput_a, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_a, summary = spread, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = spread, alternative = "two.sided") #gleich
testQuantiles(simulationOutput_a) ## quantile deviations detected!!!
testQuantiles(simulationOutput_b) # b besser

plotResiduals(simulationOutput_a, I(A1_2000$imp2000^2)) ## quantile deviations detected
plotResiduals(simulationOutput_b, I(A1_2000$imp2000^2)) ## quantile deviations detected
plotResiduals(simulationOutput_a, A1_2000$imp2000) ## quantile deviations detected
plotResiduals(simulationOutput_b, A1_2000$imp2000) ## quantile deviations detected
testSpatialAutocorrelation(simulationOutput_a, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)
testSpatialAutocorrelation(simulationOutput_b, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat) 

## 1.2.5.1 imp2000 skalieren: ########
A1_2000_scl <- A1_2000 %>% 
  mutate(imp2000 = scale(imp2000))

model5a <- MASS::glm.nb(Abundanz ~ . + I(imp2000^2), data = A1_2000_scl[,-c(1,2)]) ##
model5a_step <- step(model5a)

model5b <- MASS::glm.nb(Abundanz ~ imp2000 + I(imp2000^2), data = A1_2000_scl) ## knapp signifikant
model5c <- MASS::glm.nb(Abundanz ~ I(imp2000^2), data = A1_2000_scl) ## knapp nicht signifikant

AIC(model5a, model5a_step, model5b, model5c) # keine großen Unterschiede
anova(model5b, model5c) # nicht signifikant also einfacheres Modell: model5c
anova(model5c, model5a_step) # knapp signifikant - also komplexeres Modell = model5a_step

## DHARMa test
testDispersion(model5c) #ok
testDispersion(model5a_step) #ok
simulationOutput_a <- simulateResiduals(fittedModel = model5c, plot = T)
simulationOutput_b <- simulateResiduals(fittedModel = model5a_step, plot = T)

testDispersion(simulationOutput_a)
testDispersion(simulationOutput_b) # a besser
testOutliers(simulationOutput_a)
testOutliers(simulationOutput_b) # gleich
testUniformity(simulationOutput_a)
testUniformity(simulationOutput_b) # b besser
testZeroInflation(simulationOutput_a) # signifikant
testZeroInflation(simulationOutput_b) # signifikant
testGeneric(simulationOutput_a, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_a, summary = spread, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = spread, alternative = "two.sided") #gleich
testQuantiles(simulationOutput_a) ## quantile deviations detected!!!
testQuantiles(simulationOutput_b) # b besser

plotResiduals(simulationOutput_a, I(A1_2000_scl$imp2000^2)) ## quantile deviations detected
plotResiduals(simulationOutput_b, I(A1_2000_scl$imp2000^2)) ## quantile deviations detected
plotResiduals(simulationOutput_b, A1_2000_scl$imp2000) ## quantile deviations detected
plotResiduals(simulationOutput_b, A1_2000_scl$imp2000) ## quantile deviations detected
testSpatialAutocorrelation(simulationOutput_a, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)
testSpatialAutocorrelation(simulationOutput_b, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat) ## b besser
## b besser, aber skalieren bringt nichts wegen quantile-deviations.


### 1.2.6 summary ###########

sjPlot::tab_model(model1a, model2c, model3c, model3a_step, model4b,model5b,  
                  dv.labels = c("imp100", "imp250", "imp500", "imp500b", "imp1000", "imp2000")) 

broom::tidy(model1a)
broom::tidy(model2c)
broom::tidy(model3c)
broom::tidy(model4b)
broom::tidy(model5b)


summary(model0_step)
ggplot(A1_2000, aes(imp2000, Artenanzahl_det)) + geom_point()






##### 1.3

model3 <- MASS::glm.nb(Abundanz ~ imp500 + I(imp500^2), data = A1) ## nicht signifikant
model4 <- MASS::glm.nb(Abundanz ~ imp1000 + I(imp1000^2), data = A1) ##  signifikant !!
model5 <- MASS::glm.nb(Abundanz ~ imp2000 + I(imp2000^2), data = A1) ##  signifikant !!

summary(model4)
summary(model5)

model_poisson <- glm(Abundanz ~ imp1000 + I(imp1000^2), data = A1, family = "poisson")
AIC(model1, model2, model3, model4, model_poisson)

ggplot(A1, aes(imp1000, Abundanz)) + geom_point()
ggplot(A1, aes(imp2000, Abundanz)) + geom_point()

model_all <- MASS::glm.nb(Abundanz ~ imp1000 + I(imp1000^2) + Urban + Arable + Pastures + Forest, data = A1_IH_factor)
model_step <- step(model_all)
summary(model_step)
AIC(model_step, model4)


model_str <- MASS::glm.nb(Abundanz ~ imp1000 + I(imp1000^2) + len + class_nr, data = A1_IH_factor)
AIC(model_str, model4)


glm_IH <- MASS::glm.nb(Abundanz ~  Insektenhaus, data = A1_IH_factor)

library(DHARMa)
testDispersion(model4)
#simulationOutput <- simulateResiduals(fittedModel = model1, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model2, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model3, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model4, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model5, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model_all, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model_poisson, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model_step, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model_str, plot = T)

plotResiduals(simulationOutput, filter(A1, !is.na(Abundanz))$imp1000)
hist(simulationOutput)


######autocor ######

# (Artenzahl_autocor$lat <- set_units(Artenzahl_autocor$lat,  m))
# (Artenzahl_autocor$lon <- set_units(Artenzahl_autocor$lon,  m))

distMat <- sf::st_distance(Artenzahl_autocor)
testSpatialAutocorrelation(simulationOutput,
                           x = Artenzahl_autocor$lon, 
                           y = Artenzahl_autocor$lat)

#######
?correlog
model <- model4 # or binom1 or pois1
correlog1.1 <- correlog(Artenzahl_autocor$lon, Artenzahl_autocor$lat, residuals(model), na.rm=T, increment=1, resamp=0)

# now plot only the first 20 distance classes:
par(mar=c(5,5,0.1, 0.1))
plot(correlog1.1$correlation[1:2000], type="b", pch=16, cex=1.5, lwd=1.5, 
     xlab="distance", ylab="Moran's I", cex.lab=2, cex.axis=1.5); abline(h=0)

require(spdep)
snouter.nb <- dnearneigh(as.matrix(Artenzahl_autocor[19:20]), 0, 800000) #give lower and upper distance class here!
snouter.listw <- nb2listw(snouter.nb) #turns neighbourhood object into a weighted list
#this next step takes often several minutes to run:
GlobMT1.1<- moran.test(residuals(model), listw=snouter.listw)
