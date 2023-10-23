# Семинар 7. Обнаружение влиятельных наблюдения. Безусловное прогнозирование.
library(tidyverse) # манипуляции с данными и построение графиков
library(sjPlot) # красивые графики для линейных моделей
library(rio) # импорт/экспорт данных
library(car) # тестирование гипотез
library(lmtest) # тестирование гипотез
library(ggfortify)
library(broom)

# Задание 1: обнаружение влиятельных наблюдений и выбросов.
data = import('/Users/polinapogorelova/Downloads/dataset 2/dataset/ch2/Chow_2.xls') # импортируем данные
reg = lm(C ~ Y + D2 + D3 + D4, data)
summary(reg)

# 1. Показатель воздействия наблюдения или «разбалансировки»: точки левериджа (h_ii - показатель влияния, рычаг)
n = nrow(data)
p = 5
print(augment(reg)$.hat,4)
plot(augment(reg)$.hat, type = 'h')
as.vector(which(abs(augment(reg)$.hat) > 2*p/n)) # точки левериджа не обнаружены
# Не все наблюдения, которые можно отнести к классу «leverage point», являются влиятельными.
# Невыявлено наблюдений с высоким потенциалом воздействия на параметры модели.

# Влиятельные наблюдения (influential observation). Расчёт статистик влияния (Influence Statistics)
# 2. Анализ влиятельности наблюдений: DFFITS
dffits = as.data.frame(dffits(reg))
dffits
head(dffits)
thresh_dffits = 2*sqrt(p/n)
thresh_dffits
as.vector(which(abs(dffits) > thresh_dffits))

# 3. Анализ влиятельности наблюдений: DFBETAS
dfbetas = as.data.frame(dfbetas(reg))
head(dfbetas)
threshold = 2 / sqrt(n)
threshold
as.vector(which(abs(dfbetas$Y) > threshold))
as.vector(which(abs(dfbetas$D2) > threshold))
as.vector(which(abs(dfbetas$D3) > threshold))
as.vector(which(abs(dfbetas$D4) > threshold))

# Визуализируем DFBETAS для всех коэффициентов при переменных
par(mfrow = c(2,2)) # задаем раметку для четырех графиков

# график DFBETAS для Y
plot(dfbetas$Y, type = 'h')
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)

# график DFBETAS для D2
plot(dfbetas$D2, type = 'h')
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)

# график DFBETAS для D3
plot(dfbetas$D3, type = 'h')
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)

# график DFBETAS для D4
plot(dfbetas$D4, type = 'h')
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)

# 4. Анализ выбросов (outlier): стандартизированные и стьюдентизированные остатки
rstandard(reg)
as.vector(which(abs(rstandard(reg)) > 2))
rstudent(reg) # стьюдентизированные остатки
crit = qt(.95, n-p)
as.vector(which(abs(rstudent(reg)) > crit))

# 5. Таблица с метриками для выявления влиятельных наблюдений
influence.measures(reg)

# Переоценим модель reg, исключив из набора данных наблюдения с номерами 14, 15, 16, 19
data_new = data[-c(14,15,16,19),]
data_new
nrow(data_new)
reg_new = lm(C ~ Y + D2 + D3 + D4, data = data_new)
summary(reg_new)

# Задание 2: Безусловное прогнозирвоание
new_data = data.frame(Y = 106.4, D1 = 0, D2 = 0, D3 = 0, D4 = 1) # добавим новое наблюдение
yhat = predict(reg, newdata = new_data, se = TRUE, interval = "predict", level = .95) # построим точечный и интервальный безусловный прогноз
yhat$fit
