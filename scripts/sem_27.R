library(rio) # импорт данных
library(plm) # панельные данные


d = import("/Users/polinapogorelova/Desktop/Panel_2.xls")

ols = lm(lnOpEx ~ lnTC, data = d)
summary(ols)

pooled = plm(lnOpEx ~ lnTC, data = d, index = c("i", "t"), model = "pooling")
summary(pooled)

pooled_dummy = plm(lnOpEx ~ lnTC + D2 + D3 + D4, data = d, index = c("i", "t"), model = "pooling")
summary(pooled_dummy)

# Модель с фиксированными эффектами
fixed = plm(lnOpEx ~ lnTC, data = d, index = c("i", "t"), model = "within")
summary(fixed)
fixef(fixed)

# Модель со случайными эффектами
random = plm(lnOpEx ~ lnTC, data = d, index = c("i", "t"), model = "random")
summary(random)

# Выбор модели
# Pooled vs FE: F-test. H0: Pooled H1: FE
pFtest(fixed, pooled)
# Pooled vs RE: Breusch-Pagan test. H0: Pooled H1: RE
plmtest(pooled, type = c("bp"))
# FE vs RE: Hausman test. H0: RE H1: FE
phtest(fixed, random)

# Two-way FE
fixed.time = plm(lnOpEx ~ lnTC + factor(t), data = d, index=c("i", "t"), model = "within")
summary(fixed.time)
