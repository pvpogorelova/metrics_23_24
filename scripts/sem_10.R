# Семинар 10: гетероскедастичность

library(rio) # импорт и экспорт данных в разных форматах
library(dplyr) # манипуляции с данными
library(lmtest) # тест Бройша-Пагана
library(sandwich) # оценка дисперсии при гетероскедастичности
library(UStatBookABSC) # WLS
library(estimatr) # получение робастных оценок
library(ggpubr) # для графиков
library(skimr) # для описательных статистик


d <- import("/Users/polinapogorelova/Downloads/dataset 4/dataset/ch5/Heterosk_5.xls")
d$Time <- as.numeric(d$Time)
d$Expend <- as.numeric(d$Expend)
reg <- lm(Expend ~ Time + MPrice + Age1 + Age2 + Age3 + Age4, data = d)
summary(reg)
# Сохраним прогнозные значения
d$expend_pred <- fitted(reg)
# Сохраним остатки модели
d$resid <- residuals(reg)
# и создадим две переменные-модуль и квадрат остатков
d$abs_resid <- abs(d$resid)
d$resid2 <- (d$resid)^2

# Построим график зависимости остатков модели от прогнозных значений
ggplot(d) + geom_point(aes(x = expend_pred, y = resid)) +
  labs(x = "Остатки модели", y = "Прогноз затрат",
       title = "Остатки-прогноз")

# Проверим наличие гетероскедастичности с помощью тестов.

# Начнём с теста Уайта.
# Реализуем данный тест вручную
# Построим регрессию квадрата остатков на объясняющие переменные, их квадраты и их попарные произведения
reg_white <- lm(resid2 ~ 1 + (Time + MPrice + Age1 + Age2 + Age3 + Age4)^2 + I(Time^2) + I(MPrice^2), data = d)
summary(reg_white) # так как в целом модель значима (p-value=0 < 0.05), то гипотеза H1 не отвергается, то есть есть гетероскедастичность
hi2_obs <- nrow(d)*(summary(reg_white))$r.squared
hi2_crit <-  qchisq(.95, df=17)
hi2_obs
hi2_crit

# Проведём тест Бройша-Пагана.
# Классическая версия Бройша-Пагана реализуется в R по команде:
help(bptest)
bptest(reg, studentize = FALSE)

# Рассмотрим способы корректировки гетероскедастичности.
# Способ 1. Взвешенный МНК.
weight_reg = lm(abs_resid ~ Time, data = d)
summary(weight_reg)
hatw = 1/predict(weight_reg)^2

reg_wls = lm(data = d,
             Expend ~ Time + MPrice + Age1 + Age2 + Age3 + Age4,
             weights = hatw)
summary(reg_wls)

# Способ 2. Робастные ошибки в форме Уайта.
reg_hc3 = lm_robust(data = d,
                   Expend ~ Time + MPrice + Age1 + Age2 + Age3 + Age4, se_type = "HC3")
summary(reg_hc3)

# Робастные оценки коэффициентов регрессии являются состоятельными.

