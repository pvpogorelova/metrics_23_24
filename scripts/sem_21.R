# Семинар 21. ARMA процессы, тесты на стациорнарность.
# Установка пакетов
install.packages("tidyverse")
install.packages("fpp3")
install.packages("lubridate")
install.packages("rio")
install.packages("ggplot2")
install.packages("patchwork")

# Подключение библиотек
library(tidyverse) # обработка данных
library(fpp3) # все и сразу для рядов
library(lubridate) # все и сразу для рядов
library(car) # тест Дарбина-Уотсона
library(lmtest) # тест Бройша-Годфри
library(rio) # импорт данных
library(ggplot2) # графики
library(patchwork) # склейка графиков
library(rvest) # сбор данных
library(urca) # тесты на единичный корень

# Задание 1. Тестирование автокорреляции
us_change

# Оценим регрессию потребления на доход
model = lm(Consumption ~ Income, data = us_change)

# Тест Дарбина-Уотсона
# H0: нет автокорреляции 1-го порядка
durbinWatsonTest(model)

# Тест Бройша-Годфри
# H0: нет автокорреляции порядка p
bgtest(Consumption ~ Income, data = us_change, order = 4)

# Задание 2
# создаём ряд с нуля
n_obs = 100
set.seed(777)
ts = tsibble(date = yearmonth(ymd('2010-01-01') + months(0:(n_obs - 1))),
             wn = rnorm(n_obs, mean = 10, sd = 2), # процесс белого шума: y_t = eps_t
             iid = rnorm(n_obs, mean = 0, sd = 4), # процесс независимых наблюдений: y_t = mu + eps_t
             rwalk = 10 + cumsum(rnorm(n_obs, mean = 0, sd = 1)), # процесс случайного блуждания (Random Walk) с дрейфом
             index = date)
ts
p1 = autoplot(ts, wn)
p2 = autoplot(ts, iid)
p3 = autoplot(ts, rwalk)

(p1 + p2) / p3 # изобразим на одном графике

gg_season(ts, rwalk)
gg_subseries(ts, rwalk)

# зависимость текущего значения от лагированных
gg_lag(ts, rwalk)
gg_lag(ts, iid)

# несколько графиков сразу
gg_tsdisplay(ts, rwalk, plot_type = 'season')

# Задание 3. MA(q)-процессы

n_obs = 500

# MA(1): x_t = u_t + 0.9*u_(t-1)
# MA(2): y_t = u_t - 0.6*u_(t-1) + 0.8*u_(t-2)
help(arima.sim)
data_ma = tibble(x = arima.sim(n = n_obs, model = list(ma = 0.9)),
                 y = arima.sim(n = n_obs, model = list(ma = c(-0.6, 0.8))))

data_ma$date = yearweek( ymd('1980-01-01') + weeks(0:(n_obs-1)))
data_ma = as_tsibble(data_ma, index = date)
data_ma %>% autoplot(x)

p1 = data_ma %>% autoplot(x) +
  labs(title = "Процесс MA(1)")

p2 = data_ma %>% autoplot(y) +
  labs(title = "Процесс MA(2)")

(p1) / (p2)

gg_tsdisplay(data_ma, x, plot_type = 'partial')
gg_tsdisplay(data_ma, y, plot_type = 'partial')

# Задание 4. AR(p)-процессы
n_obs = 50

set.seed(1000)

# x: AR(1): x_t = 4 + 0.5*x_(t-1) + u_t, u_t ~ WN(0, 4)
# y: AR(2): y_t = 0.5*y_(t-1) + 0.06*y_(t-2) + u_t, u_t ~ WN(0, 1)

data_ar = tibble(x = 4 + arima.sim(n = n_obs,
                                   model = list(ar = 0.5, sd = 2)),
                 y = arima.sim(n = n_obs,
                               model = list(ar = c(0.5, 0.06))))

date = seq(as.Date("1996-01-01"), as.Date("2020-12-31"), by = "1 quarter")
data_ar$date = yearquarter(date)

data_ar = as_tsibble(data_ar, index = date)

gg_tsdisplay(data_ar, x, plot_type = 'partial')
gg_tsdisplay(data_ar, y, plot_type = 'partial')

# Задание 5. ARMA(p,q)-процессы
# x: ARMA(1,1): x_t = 0.7*x_(t-1) + u_t - 0.4*u_(t-1), u_t ~ WN(0, 1)
# y: ARMA(2,1): y_t = 0.6*y_(t-1) + 0.3*y_(t-2) + u_t + 0.7*u_(t-1), u_t ~ WN(0, 1)

data_arma = tibble(x = arima.sim(n = n_obs,
                                 model = list(ar = 0.7, ma = -0.4)),
                   y = arima.sim(n = n_obs,
                                 model = list(ar = c(0.6, 0.3), ma = 0.7)))

data_arma$date = yearmonth(ymd('2010-01-01') + months(0:(n_obs-1)))
data_arma = as_tsibble(data_arma, index = date)

gg_tsdisplay(data_arma, x, plot_type = 'partial')
gg_tsdisplay(data_arma, y, plot_type = 'partial')


train = filter(data_arma, year(date) < 2017)
test = filter(data_arma, year(date) >= 2017)


models = model(train,
               ar1 = ARIMA(x ~ pdq(1,0,0) + PDQ(0,0,0)),
               ar2 = ARIMA(x ~ pdq(2,0,0) + PDQ(0,0,0)),
               arma = ARIMA(x ~ pdq(1,0,1:2) + PDQ(0,0,0))
)

models

report(models$arma[[1]])

fcst = forecast(models, test)
autoplot(fcst) +
  autolayer(data_arma, x)

# Задание 6. Пример на ежемесячных данных о среднедушевом потреблении
d = import('/Users/polinapogorelova/Desktop/АВР/income.xlsx')
d = mutate(d, date = yearmonth(ymd('1994-01-01') + months(0:263)) )
d = as_tsibble(d, index = date)

d %>% autoplot(income)

# ADF с константой
# H0: ts = ARIMA(p, 1, q) + trend (нестационарный ряд)
# Ha: ts = ARIMA(p, 0, q) + const (стационарный ряд)
summary(ur.df(d$income, type = 'drift',
              selectlags = 'AIC'))  # H0 не отвергается на 5% уровне значимости


# Разделим данные на train и test
train = filter(d, year(date) < 2015)
test = filter(d, year(date) >= 2015)


models = model(train,
               model1 = ARIMA(income ~ pdq(1,0,1) + PDQ(0,1,0)),
               auto = ARIMA(income)
)

models

report(models$model1[[1]])
report(models$auto[[1]])

fcst = forecast(models, test)

accuracy(fcst,d) %>%
  arrange(MAE)

autoplot(fcst) +
  autolayer(test, income)
