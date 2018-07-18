library(readr)
library(ggplot2)
library(corrplot)

states.data <- readRDS("C:/Users/jason/Desktop/Capstone/Ch 7 Linear Regressions/dataSets/states.rds")
# Exercise: Least Squares Regression
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Energy Consumed VS Metro Residents
#1) Plot data before fitting model
#2) Print and interpret the model 'summary'
#3) 'plot' the model to look for deviations from modeling assumptions

#~~METRO~~~
# 1)
states.energy.metro <- subset(states.data[-9,], select = c("energy", "metro")) #Remove D.C., row 9 due to NA's
summary(states.energy.metro)
cor(states.energy.metro) # -0.340 correlation

ggplot(states.energy.metro, aes(x = metro, y = energy)) + geom_point()


# 2)
energy.mod <- lm(energy ~ metro, data = states.data)
summary(energy.mod) #low significance with metro Pr = 0.0158, slightly positive
#R-squared = 0.1154
#Adjusted = 0.097
coef(summary(energy.mod))


# 3)
par(mfrow=c(2,2))
par(mar = rep(2,4))
plot(energy.mod)
#Res vs Fitted: Some outliers (19, 20, 51) but very small residuals
#Q-Q: Normally distributed except with points(19, 20, 51)
#Scale-Location: Variance is reasonably equal -> homoskedasticity
#Res vs Lev: No points fall outside of Cook's Distance

#~~~METRO + TOXIC~~~
# 1)
states.energy.toxic <- subset(states.data[-9,], select = c("energy", "metro", "toxic"))
summary(states.energy.toxic)
cor(states.energy.toxic) #toxic cor = 0.56
ggplot(states.energy.toxic, aes(x = toxic, y = energy)) + geom_point()

# 2)
toxic.mod <- lm(energy ~ metro + toxic, data = states.data)
summary(toxic.mod)
#R-squared = 0.3676
#Adjusted = 0.3407 (improvement)

# 3)
par(mfrow=c(2,2))
par(mar = rep(2,4))
plot(toxic.mod)
#Res vs Fit: Good, mostly horizontal, few outliers (51-WY, 2-AK)
#Q-Q: Mostly normal save (45-UT, 51, 2)
#Scale-Loc: Nearly horizontal, so mostly homoskedastic
#Res vs Lev: 2 and 19 are just barely within range, 45 is on cook's line. May want to exclude for better results.


#~~~ METRO + TOXIC + Misc~~~~~

# 1)
states.energy.misc <- subset(states.data[-9,], select = c("energy", "area", "metro", "toxic", "csat", "vsat", "expense", "high", "college"))
summary(states.energy.misc)
cor(states.energy.misc)

ggplot(states.energy.misc, aes(x = area + metro + toxic + csat + vsat + expense + high + college, y = energy)) + geom_point()

# 2)
misc.mod <- lm(formula = energy ~ area + metro + toxic + green + csat + vsat + expense + high + college, data = states.data)
summary(misc.mod)
#R-squared = 0.8043
#Adjusted = 0.7579

# 3)
par(mfrow=c(2,2))
par(mar = rep(2,4))
plot(misc.mod)

#Res vs Fitted: appears to have some outliers visible (45, 19)
#Q-Q: appears to have some tail skew (Q-Q graph varies from line at high and low ends, 45, 44, 19)
#Scale-Location: Close to horizontal, so nearly homoskedastic (few outliers could fix)
#Res vs Leverage: 2, 45, 19 all very close or beyond cook's distance. May exclude and check again.

#check with excluded values (2, 19, 45):
states.exclude <- subset(states.data[-c(2,9,19,45), ], select = c("energy", "area", "metro", "toxic", "green", "msat", "miles", "csat", "vsat", "expense", "high", "college"))
exclude.mod <- lm(formula = energy ~ area + metro + toxic + green + csat + vsat + expense + high + college, data = states.exclude)
summary(exclude.mod)
#R-squared = 0.8542
#Adjusted = 0.8178
plot(exclude.mod)
#Removing these 3 outliers (2, 19, 45) have greatly improved the model. From the Res vs Fit graph, its very horizontal.
#Only 1 point (48) is significantly off the graph on the Q-Q line. Scale-location is relatively homoscedastic below 500
#Removing the outlier at ~780 should improve this. All but point 44 are within Cook's distance and the graph has improved.
#Removing the 3 rows from above should be carefully considered before doing so to truly determine if they are waranted to be removed or not.


#These last two models are significantly better than the original one, even before dropping outliers
#Original                    Later Model
#R-squared = 0.1154          R-squared = 0.8043
#Adjusted = 0.097            Adjusted = 0.7579

sat.expense.by.percent <- lm(csat ~ expense*income, data = states.data)
coef(summary(sat.expense.by.percent))

str(states.data$region)
states.data$region <- factor(states.data$region)
sat.region <- lm(csat ~ region, data = states.data)

contrasts(states.data$region)
coef(summary(lm(csat ~ C(region, base = 4), data = states.data)))
coef(summary(lm(csat ~ C(region, contr.helmert), data = states.data)))


#4 Test Model (now that I know what I'm doing =D and can check correlation)
states_numbers <- which(sapply(states.data, is.numeric))
numStates <- names(states_numbers)
states_numVar <- states.data[, states_numbers]
cor_states <- cor(states_numVar, use = "pairwise.complete.obs")
cor_states_sorted <- as.matrix(sort(cor_states[, "energy"], decreasing = TRUE))
cor_states_sorted
# green 0.7706
# area 0.6626
# toxic 0.5624

modeltest <- lm(energy ~ green + area + toxic + metro, data = states.exclude)
summary(modeltest)
coef(summary(modeltest))
# adjusted R^2 = 0.8091

# Exercise: interactions and factors
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1)
states.num <- states.data[-9, -c(1:2)]
ggplot(states.num, aes(x =  metro +green + area + toxic + toxic*green, y = energy)) + geom_point() #plot to see trends

interaction1 <- lm(energy ~ metro +green + area + toxic + toxic*green, data = states.num)
interaction1
summary(interaction1)
#t value = 2.807 for green*toxic, R^2(adjust) = 0.7941
coef(summary(interaction1))
plot(interaction1)
# Slightly lower R^2 value (0.7987 vs 0.8134) but very minimal difference while providing another variable (green*toxic)
# with a t value higher than green/area/toxic by themselves. This suggests that greenhouse gases and toxics influence each
# other. This is true, as with my background in chemistry, I know that often toxic chemicals that are solid and liquid can
# give off vapors which then collect in the atmosphere.


# 2) 
states.region <- states.data[-9, ]
ggplot(states.region, aes(x = metro + green + area + toxic + toxic*green , y = energy, color = region)) + 
  geom_point() +
  facet_grid(. ~ region)

# NE region has a significantly lower average energy than other regions
# West and South have high outliers for energy consumed (AK in the west, LA and TX in the south)
# There are some differences by region. NE consumes the least energy.Also the model of metro + green + area + toxic + toxic*green
# has much lower values for NE vs the higher ones for the west.

