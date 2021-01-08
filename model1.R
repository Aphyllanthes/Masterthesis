
source("datenschau.R")
library(tidyr)

## 0. Insektenhaus ###########
sub_IH <- A1_IH_factor %>% 
  filter(!is.na(Insektenhaus))

## 0.1 Modelle Insektenhaus: einfluss von Insektenhaus auf artenzahl:
modelIH <- MASS::glm.nb(Artenanzahl_det ~ Insektenhaus, data = sub_IH) ## signifikant
model_IH1 <- MASS::glm.nb(Artenanzahl_det ~ imp1000 + I(imp1000^2) + Insektenhaus + Insektenhaus*imp1000, data = sub_IH) ## nicht signifikant
model_IH2 <- MASS::glm.nb(Artenanzahl_det ~ imp1000 + I(imp1000^2) + Insektenhaus, data = sub_IH) # signifikant
model_IH2_100 <- MASS::glm.nb(Artenanzahl_det ~ imp100 + I(imp100^2) + Insektenhaus, data = sub_IH)
model_IH2_250 <- MASS::glm.nb(Artenanzahl_det ~ imp250 + I(imp250^2) + Insektenhaus, data = sub_IH)
model_IH2_500 <- MASS::glm.nb(Artenanzahl_det ~ imp500 + I(imp500^2) + Insektenhaus, data = sub_IH)
model_IH2_2000 <- MASS::glm.nb(Artenanzahl_det ~ imp2000 + I(imp2000^2) + Insektenhaus, data = sub_IH)


model_IH2a <- MASS::glm.nb(Artenanzahl_det ~ imp2000 + I(imp2000^2) + Insektenhaus, data = sub_IH) # signifikant

model_IH2a <- MASS::glm.nb(Artenanzahl_det ~ imp1000 + I(imp1000^2) + Insektenhaus + len + class_nr, data = sub_IH) # signifikant
model_IH3 <- glm(Artenanzahl_det ~ imp1000 + I(imp1000^2) + Insektenhaus, data = sub_IH, family = "poisson")# signifikant
model_IH4 <- glm(Artenanzahl_det ~ Insektenhaus, data = A1_IH_factor, family = "poisson")# signifikant
AIC(modelIH, model_IH1, model_IH2, model_IH3, model_IH4)


modelIH_1 <- MASS::glm.nb(Artenanzahl_det ~ Insektenhaus + imp500 + I(imp500^2), data = sub_IH)
modelIH_1a <- MASS::glm.nb(Artenanzahl_det ~ imp500 + I(imp500^2), data = sub_IH)
## anova sagt das das Insektenhotel das Modell verbessert!

anova(model_IH1, model_IH2) # model IH_2 besser 
AIC(model_IH2, model_IH2_100, model_IH2_2000, model_IH2_250, model_IH2_500)
anova(modelIH, model_IH2) # Versiegelungsgrad 1000 m verbessert das Modell nicht
anova(modelIH, model_IH2_100) # 100m Radius auch nicht
anova(modelIH, model_IH2_2000) # 250, 500 und 1000 auch nicht.

## effektplots
# jtools::effect_plot(model_IH2, pred = Insektenhaus, interval = TRUE, plot.points = TRUE,
#                     jitter = .2)
# jtools::effect_plot(model_IH2, pred = Insektenhaus, interval = TRUE, partial.residuals = TRUE,
#                     jitter = .2)
# 
# jtools::effect_plot(modelIH, pred = Insektenhaus, interval = TRUE, plot.points = TRUE,
#                     jitter = .2)
# jtools::effect_plot(modelIH, pred = Insektenhaus, interval = TRUE, partial.residuals = TRUE,
#                     jitter = .2)

sjPlot::plot_model(model_IH2, type = "pred", terms = c("Insektenhaus", "imp1000"), 
                   show.p = T, show.data = T, jitter = T) 

jtools::plot_summs(model_IH2, plot.distributions = T, scale = T)


## 0.2 Residuendiagnostik: ##########
summary(modelIH)
simulationOutput_a <- simulateResiduals(fittedModel = modelIH, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model_IH1, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model_IH2, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model_IH3, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model_IH4, plot = T)

#simulationOutput <- simulateResiduals(fittedModel = model_IH2a, plot = T)
testDispersion(simulationOutput_a)
testOutliers(simulationOutput_a)
testUniformity(simulationOutput_a)
testZeroInflation(simulationOutput_a)

testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput)
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

## Residuendignostik ergnibt: model_IH2 ist gut. 

ggplot(A1_IH_factor, aes(Insektenhaus, Artenanzahl_det)) + geom_boxplot()
## Model zwar signifikant, jedoch nur 114 Datenpunkte - und vielleicht wurden die Daten gar nicht so genau aufgenommen?

## 1. Modelle zu Anzahl der Bienenarten: ##############

### 1.1 select predictors, check for correlating predictors ######
library(Hmisc)

## imp100:
A1_100 <- A1 %>% 
  select(-imp250, -imp500, -imp1000, -imp2000) %>% 
  #select(- Artenanzahl_det, - Abundanz) %>% 
  select(-DistInsektenhaus, -Isnsektenhaus)
## x210, x231 und x310 werden wegen den vielen nullen zu TRUE FALSE:
# mutate(Arable = as.logical(ifelse(Arable == 0, 0, 1)),
#        Pastures = as.logical(ifelse(Pastures == 0, 0, 1)),
#        Forest = as.logical(ifelse(Forest == 0, 0, 1))) 

# plot(varclus(~., data=A1_100[,-(1:3)]))
# abline(h = 0.5)

A1_100 <- A1_100 %>% 
  select(-Urban)
## len und Urban muss ich noch eins raushauen: Urban wird rausgehauen und notiert dass bei hohem Anteil von Städtischem gebiet (Urban) die edgelength gering ist.

# plot(varclus(~., data=A1_100[,-(1:3)]))
# abline(h = 0.5)

# A1_100 %>% 
#   select(-location, -Abundanz) %>% 
#   pivot_longer(cols = everything(), names_to = "Praedi", values_to = "value") %>% 
#   ggplot(aes(value)) + facet_wrap(~Praedi, scales = "free") + geom_histogram() 

## transform Data: standardize continuous predictors ########
# A1_trans <- A1_new %>% 
#   select(-Urban, -location, -Abundanz, -DistInsektenhaus) %>% 
#   mutate_at(vars(-Artenanzahl_det, -Arable, -Pastures, -Forest), base::scale)

## imp250:
A1_250 <- A1 %>% 
  select(-imp100, -imp500, -imp1000, -imp2000) %>% 
  #select(- Artenanzahl_det, - Abundanz) %>% 
  select(-DistInsektenhaus, -Isnsektenhaus)
## x210, x231 und x310 werden wegen den vielen nullen zu TRUE FALSE:
# mutate(Arable = as.logical(ifelse(Arable == 0, 0, 1)),
#        Pastures = as.logical(ifelse(Pastures == 0, 0, 1)),
#        Forest = as.logical(ifelse(Forest == 0, 0, 1))) 

# plot(varclus(~., data=A1_250[,-(1:3)]))
# abline(h = 0.5)

A1_250 <- A1_250 %>% 
  select(-Urban)

# plot(varclus(~., data=A1_250[,-(1:3)]))
# abline(h = 0.5)
# pairs(A1_250[,c(8,10)])

## imp500:
A1_500 <- A1 %>% 
  select(-imp100, -imp250, -imp1000, -imp2000) %>% 
  #select(- Artenanzahl_det, - Abundanz) %>% 
  select(-DistInsektenhaus, -Isnsektenhaus)

# plot(varclus(~., data=A1_500[,-c(1,3)]))
# abline(h = 0.5)

A1_500 <- A1_500 %>% 
  select(-Urban, -len)

# plot(varclus(~., data=A1_500[,-(1:3)]))
# abline(h = 0.5)

## imp1000:
A1_1000 <- A1 %>% 
  select(-imp100, -imp250, -imp500, -imp2000) %>% 
  #select(- Artenanzahl_det, - Abundanz) %>% 
  select(-DistInsektenhaus, -Isnsektenhaus)

# plot(varclus(~., data=A1_1000[,-c(1,3)]))
# abline(h = 0.5)

A1_1000 <- A1_1000 %>% 
  select(-Urban, -len)

# plot(varclus(~., data=A1_1000[,-(1:3)]))
# abline(h = 0.5)

## imp2000:
A1_2000 <- A1 %>% 
  select(-imp100, -imp250, -imp500, -imp1000) %>% 
  #select(- Artenanzahl_det, - Abundanz) %>% 
  select(-DistInsektenhaus, -Isnsektenhaus)

# plot(varclus(~., data=A1_2000[,-c(1,3)]))
# abline(h = 0.5)

A1_2000 <- A1_2000 %>% 
  select(-Urban)

# plot(varclus(~., data=A1_2000[,-(1:3)]))
# abline(h = 0.5)

### 1.2 Verteilung der Artenzahl testen: ##########
# A1 %>% ggplot(aes(Artenanzahl_det)) + geom_histogram(binwidth = 1)

fit.exp <- fitdistrplus::fitdist(A1$Artenanzahl_det, "exp")
fit.cauchy <- fitdistrplus::fitdist(A1$Artenanzahl_det, "cauchy")
fit.geom <- fitdistrplus::fitdist(A1$Artenanzahl_det, "geom")
fit.logis <- fitdistrplus::fitdist(A1$Artenanzahl_det, "logis")
fit.nbinom <- fitdistrplus::fitdist(A1$Artenanzahl_det, "nbinom")
fit.norm <- fitdistrplus::fitdist(A1$Artenanzahl_det, "norm")
fit.pois <- fitdistrplus::fitdist(A1$Artenanzahl_det, "pois")

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



## 1.3 Modell Artenzahl gegen versiegelungsgrad: ##########
### 1.3.1 imp100 #######
model0 <- MASS::glm.nb(Artenanzahl_det ~ . + I(imp100^2), data = A1_100[,-c(1,3)]) ##
model0_step <- step(model0)
model1 <- MASS::glm.nb(Artenanzahl_det ~ imp100 + I(imp100^2), data = A1) ## nicht signifikant
model1a <- MASS::glm.nb(Artenanzahl_det ~ I(imp100^2), data = A1) ## signifikant !!
AIC(model0, model0_step, model1, model1a) # alle gleich, model0 etwas schlechter

anova(model1, model1a, test = "Chisq") # model1a ist ausreichend im Vergleich zu model1
anova(model0_step, model1a, test = "Chisq") #knapp signifikant Unterschiedlich - daher ist das komplexere Modell besser - das ist model0_step

summary(model0_step)

## DHARMa test
library(DHARMa)
testDispersion(model1a) #ok
testDispersion(model0_step) #ok
simulationOutput <- simulateResiduals(fittedModel = model1, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model0_step, plot = T)

testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput)
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)

plotResiduals(simulationOutput, A1_100$imp100)
plotResiduals(simulationOutput, A1_100$annual_precipitation_mm)
testSpatialAutocorrelation(simulationOutput, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)

ggplot(A1_100, aes(imp100, Artenanzahl_det)) + geom_point()
ggplot(A1_100, aes(annual_precipitation_mm, Artenanzahl_det)) + geom_point()

 ### 1.3.2 imp250 #####
model2a <- MASS::glm.nb(Artenanzahl_det ~ . + I(imp250^2), data = A1_250[,-c(1,3)]) ##
model2a_step <- step(model2a)

model2b <- MASS::glm.nb(Artenanzahl_det ~ imp250 + I(imp250^2), data = A1_250) ## nicht signifikant
model2c <- MASS::glm.nb(Artenanzahl_det ~ I(imp250^2), data = A1_250) ## signifikant!!

AIC(model2a, model2a_step, model2b, model2c) # model2a ist am schlechtesten, alle anderen ok
anova(model2b, model2c) # nicht signifikant, also einfacherers Modell
anova(model2a_step, model2c) # knapp signifikant, also komplexeres Model = model2a_step

## DHARMa test
library(DHARMa)
testDispersion(model2c) #ok
testDispersion(model2a_step) #ok
simulationOutput <- simulateResiduals(fittedModel = model2c, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model2a_step, plot = T)

testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput)
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)

plotResiduals(simulationOutput, A1_250$imp250)
plotResiduals(simulationOutput, A1_250$annual_precipitation_mm)
plotResiduals(simulationOutput, A1_250$mean_annual_temp_celsius)
testSpatialAutocorrelation(simulationOutput, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)

ggplot(A1_250, aes(imp250, Artenanzahl_det)) + geom_point()
ggplot(A1_250, aes(annual_precipitation_mm, Artenanzahl_det)) + geom_point()
ggplot(A1_250, aes(mean_annual_temp_celsius, Artenanzahl_det)) + geom_point()

summary(model2a_step)

### 1.3.3 imp500 #####
model3a <- MASS::glm.nb(Artenanzahl_det ~ . + I(imp500^2), data = A1_500[,-c(1,3)]) ##
model3a_step <- step(model3a)
model3b <- MASS::glm.nb(Artenanzahl_det ~ imp500 + I(imp500^2), data = A1_500) ## nicht signifikant
model3c <- MASS::glm.nb(Artenanzahl_det ~ I(imp500^2), data = A1_500) ## signifikant

AIC(model3a, model3a_step, model3b, model3c)
anova(model3b, model3c) # nicht signifikant, also einfacherers Modell
anova(model3a_step, model3c) # knapp nicht signifikant, also einfacheres Modell = model3c

## DHARMa test
testDispersion(model3a_step) #ok
testDispersion(model3c) #ok
simulationOutput <- simulateResiduals(fittedModel = model3a_step, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model3c, plot = T)

testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput)
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)

plotResiduals(simulationOutput, I(A1_500$imp500^2))
testSpatialAutocorrelation(simulationOutput, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)

ggplot(A1_500, aes(imp500, Artenanzahl_det)) + geom_point()

summary(model3c)

### 1.3.4 imp1000 #####
model4a <- MASS::glm.nb(Artenanzahl_det ~ . + I(imp1000^2), data = A1_1000[,-c(1,3)]) ##
model4a_step <- step(model4a)

model4b <- MASS::glm.nb(Artenanzahl_det ~ imp1000 + I(imp1000^2), data = A1_1000) ## knapp nicht signifikant
model4c <- MASS::glm.nb(Artenanzahl_det ~ I(imp1000^2), data = A1_1000) ## knapp nicht signifikant

AIC(model4a, model4a_step, model4b, model4c) # keine großen unterschiede
anova(model4b, model4c) # nicht signifikant also einfacheres Modell: model4c
anova(model4c, model4a_step) # knapp nicht signifikant - beide Modelle werden DHARMa-getestet

model4_pois <- glm(Artenanzahl_det ~ imp1000 + I(imp1000^2), data = A1, family = "poisson") ## knapp nicht signifikant

## DHARMa test
testDispersion(model4a_step) #ok
testDispersion(model4c) #ok
simulationOutput_a <- simulateResiduals(fittedModel = model4a_step, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model4_pois, plot = T)
simulationOutput_b <- simulateResiduals(fittedModel = model4c, plot = T)

testDispersion(simulationOutput_a)
testDispersion(simulationOutput_b) # gleich
testOutliers(simulationOutput_a)
testOutliers(simulationOutput_b) # a ist besser
testUniformity(simulationOutput_a)
testUniformity(simulationOutput_b) # b viel besser
testZeroInflation(simulationOutput_a)
testZeroInflation(simulationOutput_b) # gleich
testGeneric(simulationOutput_a, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = means, alternative = "two.sided")
testGeneric(simulationOutput_a, summary = spread, alternative = "two.sided")
testGeneric(simulationOutput_b, summary = spread, alternative = "two.sided") #a etwas besser
testQuantiles(simulationOutput_a)
testQuantiles(simulationOutput_b) # a besser

plotResiduals(simulationOutput_a, I(A1_1000$imp1000^2))
plotResiduals(simulationOutput_b, I(A1_1000$imp1000^2))
testSpatialAutocorrelation(simulationOutput_a, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)
testSpatialAutocorrelation(simulationOutput_b, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat) # b besser


ggplot(A1_1000, aes(imp1000, Artenanzahl_det)) + geom_point()

#summary(model4a_step)
summary(model4c) # DHARMa identifiziert kein deutlich schlechteres Modell, darum wird das einfacherer Modell verwendet: model4c

### 1.3.5 imp2000 #####

model5a <- MASS::glm.nb(Artenanzahl_det ~ . + I(imp2000^2), data = A1_2000[,-c(1,3)]) ##
model5a_step <- step(model5a)

model5b <- MASS::glm.nb(Artenanzahl_det ~ imp2000 + I(imp2000^2), data = A1_2000) ## knapp signifikant
model5c <- MASS::glm.nb(Artenanzahl_det ~ I(imp2000^2), data = A1_2000) ## knapp nicht signifikant

AIC(model5a, model5a_step, model5b, model5c) # keine großen Unterschiede
anova(model5b, model5c) # nicht signifikant also einfacheres Modell: model5c
anova(model5c, model5a_step) # knapp signifikant - beide Modelle werden DHARMa-getestet


## DHARMa test
testDispersion(model5c) #ok
testDispersion(model5a_step) #ok
simulationOutput_a <- simulateResiduals(fittedModel = model5c, plot = T)
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
testGeneric(simulationOutput_b, summary = spread, alternative = "two.sided") #b etwas besser
testQuantiles(simulationOutput_a)
testQuantiles(simulationOutput_b) # a besser

plotResiduals(simulationOutput_a, I(A1_2000$imp2000^2))
plotResiduals(simulationOutput_b, I(A1_2000$imp2000^2)) # a sieht schöner aus
testSpatialAutocorrelation(simulationOutput_a, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)
testSpatialAutocorrelation(simulationOutput_b, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat) # a besser


ggplot(A1_2000, aes(imp2000, Artenanzahl_det)) + geom_point()

summary(model5c)
summary(model5a_step) # DHARMa identifiziert kein deutlich schlechteres Modell, darum wird das einfacherer Modell verwendet: model5c, model5a_step kann aber auch betrachtet werden.

### 1.3.6 summary ###########

sjPlot::tab_model(model0_step, model2a_step, model3c,model4c,model5c,  
                  dv.labels = c("imp100", "imp250", "imp500", "imp1000", "imp2000")) 

broom::tidy(model0_step)
broom::tidy(model2a_step)
broom::tidy(model3c)
broom::tidy(model4c)
broom::tidy(model5c)


summary(model0_step)
## bootstrap ci: ###########
bootstrap <- function(Haeufigkeit, formel, predictor){ 
  col <- which(names(Haeufigkeit) == predictor)
  random.set <- sample(1:nrow(Haeufigkeit), nrow(Haeufigkeit), replace=T)
  
  N <- 500
  boot.matrix <- matrix(NA, nrow=100, ncol=N)
  newBlabund <- seq(min(Haeufigkeit[,col], na.rm=T),
                    max(Haeufigkeit[,col], na.rm=T), len=100)
  newDaten = expand.grid(imp1000 = newBlabund)
  names(newDaten)[1] <- predictor
  
  for (i in 1:N){
    random.set <- sample(1:100, replace=T)
    bm <- MASS::glm.nb(formel, data = Haeufigkeit[random.set,])
    preds <- predict(bm, newdata=newDaten, type = "response") # type = "response"
    boot.matrix[,i] <- preds
    #cat(i, " ")
  }
  
  pred.Blabund <- as.data.frame(
    t(apply(boot.matrix, 1, quantile, c(0.05, 0.25, 0.5, 0.75, 0.95)))
  ) 
  pred.Blabund$imp1000 <- newDaten[,1]
  pred.Blabund <- pred.Blabund %>% 
    group_by(imp1000) %>% 
    summarise_all(mean, na.rm = T)
  names(pred.Blabund)[1] <- predictor
  
  return(pred.Blabund)
}
pred.Ob <- bootstrap(
  Haeufigkeit = A1,  
  formel= formula(model3), 
  predictor = "imp500"
)


formula <- y ~ x + I(x^2)
formula1 <- y ~ I(x^2)
ggplot(A1, aes(imp500, Artenanzahl_det)) + geom_point() +
  geom_smooth(method = MASS::glm.nb, aes(color = "NB"), se = T, formula = formula)+
  geom_smooth(method = MASS::glm.nb, aes(color = "NB"), se = T, formula = formula1, col = "green")

ggplot(pred.Ob, aes(x = imp500, y = `50%`)) +
  geom_line()+
  geom_line(aes(x = imp500, y = `95%`), linetype = 2) +
  geom_line(aes(x = imp500, y = `5%`), linetype = 2) +
  geom_ribbon(aes(x=imp500, ymax=`95%`, ymin=`5%`), 
              fill="grey", alpha=.5) +
  geom_point(data = Osmia_bicornis, 
             aes(x =imp500, y = imp500 ))  +
  geom_smooth(method = MASS::glm.nb,  formula = formula, size = 0.1) #+
#xlab("Artenanzahl Blüten") +
#ylab("Artenanzahl Schmetterlinge") +
theme(legend.position=c(0.25,0.8), 
      legend.box.background = element_rect(colour = "black"))




model_all <- MASS::glm.nb(Artenanzahl_det ~ imp1000 + I(imp1000^2) + Urban + Arable + Pastures + Forest, data = A1) # imp1000 und urban sind signifikant

model_all_step <- step(model_all)
model_urban <- MASS::glm.nb(Artenanzahl_det ~ Urban + I(Urban^2), data = A1) ## knapp nicht signifikant

model_imp <- MASS::glm.nb(Artenanzahl_det ~ imp1000 + I(imp1000^2) + mean_annual_temp_celsius + annual_precipitation_mm + altitude_town_m + len + Arable, data = A1) # imp1000 und urban sind signifikant

AIC(model1, model2, model3, model4, model_all)

## DHARMa test
library(DHARMa)
testDispersion(model4)
#simulationOutput <- simulateResiduals(fittedModel = model1, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model2, plot = T)
#simulationOutput <- simulateResiduals(fittedModel = model3, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model4, plot = T)

testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput)
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)
par(mfrow = c(1,2))
plotResiduals(simulationOutput, A1$imp1000)
plotResiduals(simulationOutput, I(A1$imp1000^2))
testSpatialAutocorrelation(simulationOutput, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)

simulationOutput <- simulateResiduals(fittedModel = model4_pois, plot = T)

testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput)
means <- function(x) mean(x) # testing if mean prediction fits
spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = means, alternative = "two.sided")
testGeneric(simulationOutput, summary = spread, alternative = "two.sided")
testQuantiles(simulationOutput)
par(mfrow = c(1,2))
plotResiduals(simulationOutput, A1$imp1000) ### !!!!!
plotResiduals(simulationOutput, I(A1$imp1000^2))
testSpatialAutocorrelation(simulationOutput, 
                           x = Artenzahl_autocor$lon,
                           y = Artenzahl_autocor$lat)

## Residuendiagnostik: model4_pois fällt durch.


simulationOutput <- simulateResiduals(fittedModel = model_all, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model_all_step, plot = T) ## model ist nicht so gut wie model4
simulationOutput <- simulateResiduals(fittedModel = model_imp, plot = T)
simulationOutput <- simulateResiduals(fittedModel = model_str, plot = T)

plotResiduals(simulationOutput, filter(A1, !is.na(Abundanz))$imp1000)
hist(simulationOutput)


testResiduals(simulationOutput)

testDispersion(simulationOutput)
testOutliers(simulationOutput)
testUniformity(simulationOutput)
testZeroInflation(simulationOutput)
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




############ randomforest ############

library(randomForest)
library(ranger)

# alle Prädiktoren mit weniger als 50% importance entfernen:
rf1 <- ranger::ranger(Artenanzahl_det ~ .,
                      data=filter(A1_trans, !is.na(Isnsektenhaus)), importance="impurity")
sort(ranger::importance(rf1))
## in reduziertem Datensatz ist nichts wichtiger als 50%

rf1 <- ranger::ranger(Artenanzahl_det ~ .,
                      data=select(A1_trans, -Isnsektenhaus), importance="impurity")
sort(ranger::importance(rf1))
## fliegt raus:: Pastures, Forest, Arable, class_nr 


PCA <- prcomp(A1_trans[,-c(1,7,9,10,11)], scale = T)
biplot(PCA, xpd=T)

plot(varclus(~., data=A1_trans[,-c(1,7,9,10,11)]))
abline(h = 0.5)

# par(mfrow=c(1,3), las=1)
# randomForest::partialPlot(rf1, A1_trans, imp1000_diff)
# partialPlot(rf, loyn, L10DIST, ylim=c(10,28))
# partialPlot(rf, loyn, YR.ISOL, ylim=c(10,28))

A1_imp <- A1_trans %>% 
  select(-class_nr, -Pastures, -Forest, -Isnsektenhaus)



model_all <- MASS::glm.nb(Artenanzahl_det ~ ., data = A1_imp) # imp1000 und urban sind signifikant

simulationOutput <- simulateResiduals(fittedModel = model_all, plot = T)
AIC(model_all)

#### 2. autocorrelation ##############

## correlogram nach @zuur2009
library(ncf)
koords <- Standorte %>% 
  select(ID, lat, lon) %>% 
  mutate(ID = tolower(ID)) 

A1_koords <- A1 %>% 
  left_join(koords, by = c("location" = "ID")) %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

Artenzahl_autocor <- A1_koords %>% 
  # mutate(ID = tolower(ID),
  #        lat = as.numeric(gsub("\\.", "", lat)),#as.numeric(gsub(",", "", y))
  #        lon = as.numeric(gsub("\\.", "", lon))) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% ## 3857
  sf::st_transform(crs = 3035) %>% 
  mutate(lon = sf::st_coordinates(geometry)[,1],
         lat = sf::st_coordinates(geometry)[,2]) 

# koords <- Standorte %>% 
#   select(ID, lat, lon) %>% 
#   mutate(ID = tolower(ID),
#          lat = as.numeric(gsub("\\.", "", lat)),#as.numeric(gsub(",", "", y))
#          lon = as.numeric(gsub("\\.", "", lon)))
# 
# 
# Artenzahl_autocor <- A1 %>% 
#   left_join(koords, by = c("location" = "ID"))

testSpatialAutocorrelation(simulationOutput, 
                           x = Artenzahl_autocor$lon, y = Artenzahl_autocor$lat)
?correlog
model <- model4 # or binom1 or pois1
correlog1.1 <- correlog(Artenzahl_autocor$lon, Artenzahl_autocor$lat, residuals(model), na.rm=T, increment=1, resamp=0)

# now plot only the first 20 distance classes:
par(mar=c(5,5,0.1, 0.1))
plot(correlog1.1$correlation[1:2000], type="b", pch=16, cex=1.5, lwd=1.5, 
     xlab="distance", ylab="Moran's I", cex.lab=2, cex.axis=1.5); abline(h=0)

# make a map of the residuals
plot(Artenzahl_autocor$lon, Artenzahl_autocor$lat, col=c("blue", "red")[sign(resid(model))/2+1.5], pch=19,cex=abs(resid(model))/max(resid(model))*2, xlab="geographical x-coordinates", ylab="geographical y-coordinates")


############
Correlog <- spline.correlog(x = Artenzahl_autocor$lat, y = Artenzahl_autocor$lon, 
                            z = Artenzahl_autocor$Artenanzahl_det, xmax = 5000, latlon = T)
plot(Correlog, main = "Korrelogram der Artenzahl")

GLM_ac <- glm(Artenanzahl_det ~ ., data = A1, family= "poisson")
Correlog_Glm_ac <- spline.correlog(x = A1_koords$lat,
                                   y = A1_koords$lon,
                                   z = residuals(GLM_ac, type = "pearson"))
#plot(Correlog_Glm_ac)
Correlog_Glm_model4 <- spline.correlog(x = A1_koords$lat,
                                   y = A1_koords$lon, xmax = 5000, latlon = T,
                                   z = residuals(model4, type = "pearson"))
plot(Correlog_Glm_model4, main = "Korrelogram der Residuen")

######## 3.  Versiegelungsgrad ########

library(nlme)
A1_new %>% 
  select(- X231, -X210, -X310, -class_nr) %>% 
  pivot_longer(cols = everything(), names_to = "Praedi", values_to = "value") %>% 
  ggplot(aes(value)) + facet_wrap(~Praedi, scales = "free_x") + geom_histogram()

## glm:
glm1 <- MASS::glm.nb(Artenanzahl_det ~ imp100 + I(imp100^2), data = A1_new)
glm1 <- MASS::glm.nb(Artenanzahl_det ~ imp100 + I(imp100^2), data = A1_new,
                     correlation)
glm2 <- MASS::glm.nb(Artenanzahl_det ~ ., 
                     data=A1_new[,-c(7,9,10,11)])

fm_step <- step(glm2, trace=0)

A1_ac <- A1_imp %>% 
  bind_cols(A1_koords[,c("lat", "lon")])

m2 <- nlme::gls(Artenanzahl_det ~ imp100 + I(imp100^2), data = A1_new)

vario2 <- nlme::Variogram(m2, form = ~A1_koords$lat + A1_koords$lon, resType = "pearson")
plot(vario2, smooth = TRUE, ylim = c(0, 1.2))

gls_cor1 <- nlme::gls(Artenanzahl_det ~ imp100 + I(imp100^2), data = A1_new,
                      correlation = )
m3 <- nlme::gls(Artenanzahl_det ~ imp100 + I(imp100^2), 
          correlation = nlme::corExp(form = ~lat + lon, nugget = TRUE), data = A1_ac)
m4 <- nlme::gls(Artenanzahl_det ~ imp100 + I(imp100^2), 
          correlation = nlme::corGaus(form =  ~lat + lon, nugget = TRUE), data = A1_ac)
m5 <- nlme::gls(Artenanzahl_det ~ imp100 + I(imp100^2), 
          correlation = nlme::corSpher(form =  ~lat + lon, nugget = TRUE), data = A1_ac)
m6 <- nlme::gls(Artenanzahl_det ~ imp100 + I(imp100^2), 
          correlation = nlme::corLin(form =  ~lat + lon, nugget = TRUE), data = A1_ac)
m7 <- nlme::gls(Artenanzahl_det ~ imp100 + I(imp100^2), 
          correlation = nlme::corRatio(form =  ~lat + lon, nugget = TRUE), data = A1_ac)

AIC(m2, m3, m4, m5, m6, m7)

vario4 <- Variogram(m4, form = ~lat + lon, resType = "pearson")
plot(vario4, smooth = FALSE)


# #test rausnehmen:
# fm_neu <- glm(
#   Artenanzahl_det ~ imp100 + annual_precipitation_mm + len + class_nr + 
#     imp100:annual_precipitation_mm,
#   family=poisson, data=newdata)
# 
# ggplot(newdata, aes(imp100, Artenanzahl_det)) +
#   geom_point() + 
#   geom_smooth(method = "glm", se = F, method.args = list(family = "poisson"))
# 



# shrinkage: ########
library(glmnet)
?cv.glmnet
##
library(devtools)
install_github("biometry/FReibier/FReibier")
# glmnet requires data matrix:
dataM <- model.matrix(formel, data=A1_new)[,-1] #remove intercept
library(glmnet)
lasso.cv <- cv.glmnet(x=dataM, y=A1_new$Artenanzahl_det, alpha=1)
(lasso <- glmnet(x=dataM, y=A1_new$Artenanzahl_det, lambda=lasso.cv$lambda.min, alpha=1))
##
par(mar=c(4,4,1,1))
plot(lasso.cv, las=1)
lasso.full <- glmnet(x=dataM, y=A1_new$Artenanzahl_det)
par(mar=c(4,4,1,1), mfrow=c(1,2))
plot(lasso.full, "lambda", las=1)
plot(lasso.full, "norm", las=1)

## zeroInflation -  einfluss der nullen testen: ########
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = glmstep)
plot(simulationOutput)
testZeroInflation(simulationOutput)

library(glmmTMB)
fittedModel <- glmmTMB(Artenanzahl_det ~ imp100 + mean_annual_temp_celsius + annual_precipitation_mm + 
                         altitude_town_m + len + class_nr + X100 + X210 + X231 + X310 + 
                         imp250_diff + imp500_diff + imp1000_diff + imp100:annual_precipitation_mm + 
                         imp100:X100 + imp100:X210 + imp100:X310 + imp100:imp500_diff + 
                         mean_annual_temp_celsius:class_nr + mean_annual_temp_celsius:X100 + 
                         mean_annual_temp_celsius:X231 + annual_precipitation_mm:class_nr + 
                         annual_precipitation_mm:imp500_diff + altitude_town_m:class_nr + 
                         altitude_town_m:X231 + altitude_town_m:imp1000_diff + len:X210 + 
                         class_nr:X231 + class_nr:imp500_diff + X100:X210 + X100:X310 + 
                         X100:imp500_diff + X210:X231 + X210:imp500_diff + X210:imp1000_diff + 
                         X231:X310 + X231:imp1000_diff + X310:imp250_diff + imp500_diff:imp1000_diff, ziformula = ~1 , family = "poisson", data = A1_new)
summary(fittedModel)



###gam #########
gam1 <- mgcv::gam(Artenanzahl_det ~ s(imp100) + s(mean_annual_temp_celsius) +
                    s(annual_precipitation_mm) + s(altitude_town_m) + s(len) + s(class_nr) +
                    s(X100) + s(X210) + s(X231) + s(X310) + s(imp250_diff) + s(imp500_diff) +
                    s(imp1000_diff),
                  data=A1_trans, family = mgcv::nb())

#summary(gam1)
gam1_cs <- gam(Aphelocephala.nigricincta ~ s(TDIFF, bs="cs") +
                 s(logPRE_SUMMER, bs="cs") + s(logPRE_WINTER, bs="cs") +
                 s(ELEV, bs="cs") + s(ASPECT, bs="cs") + s(TMIN_JUL, bs="cs") +
                 8
               s(test1, bs="cs") + s(test2, bs="cs") + s(test3, bs="cs"),
               data=newdata, family = "binomial")
#summary(gam1_cs)
anova(gam1, gam1_cs, test = "LRT") # test = likelihood ratio test



#Add two random valued uniform predictors to the data set. These serve as controls: they should have no effect.
newdata <- A1_trans %>% 
  mutate(test1 = scale(runif(nrow(.)))[,1],
         test2 = scale(runif(nrow(.)))[,1]) %>% 
  select(-imp250, -imp500, -imp1000)

PCA <- prcomp(newdata[,-c(1)], scale = T)
biplot(PCA, xpd=T)
plot(varclus(~., data=newdata[,-c(1)]))
abline(h = 0.5)

## glm: #######
fm <- glm(Artenanzahl_det ~ (.)^2,
          family=poisson, data=newdata)

fm_step <- step(fm, trace=0)
#test rausnehmen:
fm_neu <- glm(
  Artenanzahl_det ~ imp100 + annual_precipitation_mm + len + class_nr + 
    imp100:annual_precipitation_mm,
  family=poisson, data=newdata)

ggplot(newdata, aes(imp100, Artenanzahl_det)) +
  geom_point() + 
  geom_smooth(method = "glm",  se = F, method.args = list(family = "poisson"))


### modelle zu Nestanzahl: ###########

rf_nests <- nests %>% group_by(location) %>% summarise(nestzahl = n()) %>% 
  ungroup() %>% 
  left_join(Praedis, by = c("location" = "ID")) %>% 
  select(-location)
rf <- ranger(nestzahl ~ . , data = rf_nests, importance = "impurity")
rf
sort(ranger::importance(rf))
partialPlot(rf, rf_nests, imp100)

## transform Data: standardize continuous predictors
nests_trans <- rf_nests %>% 
  mutate_at(vars(-nestzahl), base::scale)

plot(varclus(~., data=nests_trans[,-1]))
abline(h = 0.5)

fm <- glm(nestzahl ~ (.)^2,
          family=poisson, data=nests_trans)

fm_log <- glm(log(nestzahl) ~ (.)^2,
              data=nests_trans)

fm_step <- step(fm, trace=0)

############# plots ######
rf_nests %>% 
  pivot_longer(cols = imp100:class_nr, names_to = "Praedi", values_to = "value") %>% 
  ggplot(aes(value, nestzahl)) + facet_wrap(~Praedi, scales = "free_x") + geom_point()

rf_nests %>% 
  ggplot(aes(imp100, nestzahl)) + geom_point()

rf_nests %>% 
  ggplot(aes(imp100, log(nestzahl))) + geom_point()

rf_nests %>% filter(nestzahl < 100) %>% 
  ggplot(aes(imp100, nestzahl)) + geom_point()

rf_nests %>% 
  pivot_longer(cols = everything(), names_to = "Praedi", values_to = "value") %>% 
  ggplot(aes(value)) + facet_wrap(~Praedi, scales = "free") + geom_histogram()


A1_new %>% pivot_longer(cols = everything(), names_to = "Praedi", values_to = "value") %>% 
  ggplot(aes(value)) + facet_wrap(~Praedi, scales = "free_x") + geom_histogram()

ggplot(A1_new, aes(class_nr, Artenanzahl_det)) + geom_point()
ggplot(A1_new, aes(len, Artenanzahl_det)) + geom_point()


ggplot(A1, aes(imp100, Abundanz, col = Isnsektenhaus)) + geom_point()


A1_trans %>% 
  pivot_longer(cols = everything(), names_to = "Praedi", values_to = "value") %>% 
  ggplot(aes(value)) + facet_wrap(~Praedi, scales = "free_x") + geom_histogram()

