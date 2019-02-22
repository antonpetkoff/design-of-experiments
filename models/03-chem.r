# homework: see the example on page 96 in Dean & Voss's book

library(daewr)
data(chem)
chem

model1 = aov(y ~ A * B * C * D, data = chem)
model1

model2 = lm(y ~ A * B * C * D, data = chem)
model2

coef(model2)

# effects without the Intercept
coef(model2)[-1]

fullnormal(coef(model2)[-1], alpha = 0.025)

LGB(coef(model2)[-1])

model6 = aov(y~A*B, data = chem)
model6
