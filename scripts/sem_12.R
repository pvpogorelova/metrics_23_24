# Семинар 12: выбор функциональной зависимости

library(rio) # импорт и экспорт данных в разных форматах
library(dplyr) # манипуляции с данными
library(lmtest) # тесты
library(estimatr) # получение робастных оценок
library(ggpubr) # для графиков
library(skimr) # для описательных статистик
library(MASS)
library(car)

d <- import("/Users/polinapogorelova/Desktop/dataflats.xlsx") # импорт данных
head(data)

# Переименуем столбцы с «неговорящими» названиями
d <- rename(d, n = A, price = B)
d <- mutate(d, price_sq = price/totsp) # добавим переменную price_sq
d <- na.omit(d)

# 1. # Тест Рамсея
# H0: нет пропущенных переменных
# H1: есть пропущенные переменные
resettest(price_sq ~ totsp + livesp + kitsp + dist + metrdist, power = 2, data = d)
# Так как. p-value = 0, гипотеза H1 не отвергается, то есть в модели есть пропущенные переменные

# 2.J-тест для невложенных моделей
model_1 <- lm(price_sq ~ livesp + kitsp + dist + metrdist + walk + brick, data = d)
model_2 <- lm(price_sq ~ totsp + livesp + kitsp + dist + metrdist + brick, data = d)
jtest(model_1, model_2)

# 3. PE-тест Дэвидсона-Маккинона (выбор между линейной и полулогарифмической (или линейной в логарифмах)) моделями
model_lin <- lm(price_sq ~ totsp + livesp + kitsp + dist + metrdist, data = d)
model_log <- lm(log(price_sq) ~ totsp + livesp + kitsp + dist + metrdist, data = d)
petest(model_lin, model_log)

# 4. Преобразование Бокса-Кокса и тест Бокса-Кокса
bc <- boxcox(model_lin)

# Тест Бокса-Кокса: выбор между линейной и линейной в логарифмах моделями
# H0: lambda = 0 (полулогарифмическая)
# H1: lambda != 0 (неполулогарифмическая)

# H0: lambda = 1 (линейная)
# H1: lambda != 1 (нелинейная)
summary(powerTransform(model_lin))  #  результаты LR-теста

# 5. Метод Зарембки (частный случай преобразования Бокса-Кокса): выбор между линейной и полулогарифмической моделями
# H0: качество подгонки линейной и полулогарифмической моделей одинаковое
# H1: модель с меньшей RSS лучше
g_price_sq <- exp(mean(log(d$price_sq)))
exp(mean(log(d$price_sq[d$price_sq>0]))) # геометрическое среднее
g_price_sq # геометрическое среднее (автоматически)

d$y_new <- d$price_sq/g_price_sq
d$ln_y_new <- log(d$y_new)

model1 <- lm(y_new ~ totsp + livesp + kitsp + dist + metrdist, data = d)
model2 <- lm(log(y_new) ~ totsp + livesp + kitsp + dist + metrdist, data = d)

n = nrow(d)
xi_2 <- n/2*abs(log(deviance(model1)/deviance(model2))) # значение тестовой статистики
xi_2
xi_crit <- qchisq(.95, df = 1) # критическое значение
xi_crit # так как наблюдаемое значение статистики больше критического, то на 5% уровне мы не можем отвергнуть гипотезу H1,
# то есть модели имеют разное качество подгонки
# Выбор модели производится бы на основе RSS (чем RSS меньше, тем лучше)
deviance(model1) # RSS для линейной модели
deviance(model2) # RSS для полулогарифмической модели (оказалась лучше)

# 6. Тестирование нормальности остатков модели с помощью теста Харке-Бера
# H0: остатки имеют нормальное распределение
# H1: распределение остатков отлично от нормального
install.packages('tseries') # устанавливаем необходимый пакет
library(tseries) # подключаем библиотеку

jarque.bera.test(resid(model1)) # так как p-value > 0.05, то гипотеза H0 не отвергается на любом разумном уровне значимости,
# то есть остатки линейной модели можно считать нормальными

jarque.bera.test(resid(model2)) # так как p-value > 0.05, то гипотеза H0 не отвергается на любом разумном уровне значимости,
# то есть остатки линейной модели можно считать нормальными

