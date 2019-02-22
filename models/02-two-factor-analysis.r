library(daewr)
data(COdata)
COdata
help(COdata)

model1 = aov(CO ~ Eth * Ratio, data = COdata)
summary(model1)

model2 = aov(CO ~ Eth + Ratio + Eth:Ratio, data = COdata)
summary(model2)

model3 = aov(CO ~ Eth + Ratio, data = COdata)
summary(model3)

# can be used to compute means for each factor level for each factor
model.tables(model1, type = 'means', se = T)

# we can also take the effects
model.tables(model1, type = 'effects', se = T)

library(gmodels)

interaction.plot(COdata$Eth, COdata$Ratio, COdata$CO, type = 'b')
plot.design(COdata)

tukey = TukeyHSD(model1, "Eth")
tukey

plot(tukey)

# provides estimates of the contrasts of the model
coef(model1)

fullnormal(coef(model1)[-1], alpha = 0.025)
