# book: Design and Analysis of Experiments with R
# review: the theory is a mess, but can be used as a guide for which packages to use for specific plans

set.seed(42)

f = factor(rep(c(35, 40, 45), each = 4))
f

# randomize
f1 = sample(f, 12)
f1

# numbers of observations
e = 1:12
e

plan = data.frame(loaf = e, time = f1)
plan

getwd()
setwd("/home/tony/source/design-of-experiments")
write.csv(plan, file = 'plan.csv', row.names = FALSE)

bread = read.csv("plan.csv")
bread

height = c(9.75, 8.75, 4.5, 6.5, 6.5, 5.0, 10.5, 5.5, 6.5, 9.5, 8.25, 6.75)
bread = data.frame(loaf = plan$loaf, time = plan$time, height = height)
bread


install.packages('daewr')
library(daewr)

model1 = lm(height ~ time, data = bread)
summary(model1)

# - 1 removes the free coefficient
model2 = lm(height ~ time - 1, data = bread)
summary(model2)

install.packages('gmodels')
library(gmodels)

# compare 35 to 40 minutes
fit.contrast(model1, "time", c(1, -1, 0))
# the same fit.contrast won't work on model2, because it is missing its free coefficient

# build ANOVA table
model3 = aov(height ~ time, data = bread)
summary(model3)

plot(model3, which = 1)
plot(model3, which = 2) # qq-plot

library(MASS)
bc = boxcox(model3)
lambda = bc$x[which.max(bc$y)]
lambda

# when you have observations close to being outliers, you can normalize (transform) the data
breadtr = transform(bread, heighttr = height ^ lambda)
breadtr

model4 = aov(heighttr ~ time, data = breadtr)
summary(model4) # the p-value is smaller for model4 than for model3
summary(model3)

# the probability distributions in the plots are hardly influenced by transformations
plot(model4, which = 2)

# use this plot to see if the order of the observations made in time has a functional dependence
# it is possible to have a time series
plot(residuals(model4) ~ loaf, data = bread)

model3tukey = TukeyHSD(model3, ordered = T)
model3tukey

contr.poly(3)
contrasts(bread$time)

install.packages('agricolae')
library(agricolae)

# Newman-Koils test
print(SNK.test(model3, 'time', group = FALSE))

install.packages('multcomp')
library(multcomp)

contrd = glht(model3, linfct = mcp(treat = 'Dunnet'), alternative = 'greater')
summary(contrd)
