NH11 <- readRDS("C:/Users/jason/Desktop/Capstone/Ch 7 Linear and Logistic Regressions/Logistic Regression 2/dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

# 1) predict ever worked (18949 missing)
str(NH11$everwrk) # check structure

NH11$everwrk <- factor(NH11$everwrk, levels = c("2 No", "1 Yes")) # reduces leves to only yes and no

# check levels
levels(NH11$everwrk)

# Create the regression model
worked_glm <- glm(everwrk ~ r_maritl, 
                  data = NH11, family = "binomial")
coef(summary(worked_glm))
summary(worked_glm)
# Living with partner and Divorced show significance but they also have high log coefficient values of 0.44 and 0.73,
# which signify larger odds of working. Conversely, widowed [-0.68] and never married [-0.34] for example, have large
# negative log coeficcients which represent lower probabilities of working.

workmod <- coef(summary(worked_glm))
workmod[, "Estimate"] <- exp(coef(worked_glm)) # this converts the coeffiecients from log values into ones more easily 
workmod                                        # readable probability values for odds of working

# 2) Predict the probability of working for each level of marital status
# Create a dataset with predictors set at desired levels
predDat <- with(NH11, 
                expand.grid(r_maritl = unique(r_maritl)))

plotmod <- cbind(predDat, predict(worked_glm, type = "response",
                       se.fit = TRUE, interval = "confidence",
                       newdata = predDat))

#Plot the results
library(effects)
library(scales)

ggplot(plotmod, aes(x = r_maritl, y=fit))+
  geom_point() +
  labs (x= "Marital Status", y= "Chance of working") +
  scale_x_discrete(labels = wrap_format(10))

# as expected, Divorced had the largest coefficient [0.83] and never married had the lowest [-0.93]


