# Семинар 9: Мультиколлинеарность: Ridge, Lasso, PCA.

library(tidyverse) # манипуляции с данными и построение графиков
library(sjPlot) # красивые графики для линейных моделей
library(rio) # импорт/экспорт данных
library(car) # тестирование гипотез + vif
library(lmtest) # тестирование гипотез
library(Hmisc) # данные cars
library(skimr) # описательные статистики
library(glmnet)  # LASSO + ridge
library(factoextra) # симпатичные графики для метода главных компонент


# Задание 1: Признаки мультиколлинеарности
# Сгенерируем y, x1, x2, x3 и eps, так чтобы между x1 и x2 была высокая корреляция
set.seed(1)
n = 100
p = 3

x2 = rnorm(n,100,16)
x3 = rnorm(n,-5,2)
eps = rnorm(n,0,4)

x1 = 0.8*x2 + rnorm(n, 0, 0.04)

y = 0.5 + 1.2*x1 - 2.3*x2 + 0.4*x3 + eps # DGP
data = tibble(y = y, x1 = x1, x2 = x2, x3 = x3, eps = eps)

# Построим корреляционную матрицу для x1,x2 и x3
rcorr(as.matrix(data[,2:4]))

# Оценим модель регрессии y = beta0 + beta1*x1 + beta2*x2 + beta3*x3 + eps по сгенерированным данным.
model1 = lm(data = data, y ~ x1 + x2 + x3)
# Сравним оценки коэффициентов с истинными значениями параметров beta
summary(model1)

# Рассчитаем показатели VIF вручную для каждой объяcняющей переменной.
# Если VIF(x) больше 10, то возможно есть проблема мультиколлинеарности

# VIF(x1)
1/(1 - summary(lm(x1 ~ 1 + x2 + x3, data = data))$r.squared)
# VIF(x2)
1/(1 - summary(lm(x2 ~ 1 + x1 + x3, data = data))$r.squared)
# VIF(x3)
1/(1 - summary(lm(x3 ~ 1 + x1 + x2, data = data))$r.squared)

# Сравним с результатами команды vif()
vif(model1) # получили тот же результат

# Теперь попробуйте увеличить объем выборки n до 10000 наблюдений и переоценить модель.


# Задача 2. Principal Component Analysis (метод главных компонент - МГК)
# Попробуем реализовать МГК на данных dataflats
data = import("/Users/polinapogorelova/Desktop/Эконометрика ИП/ИП 22:23/Данные/dataflats.xlsx") # импорт данных
head(data)
data = mutate(data, price_sq = price/totsp) # добавим переменную price_sq
y = data$price_sq

rcorr(as.matrix(data[2:6]))

# Метод главных компонент с предварительной стандартизацией переменных
d = data[2:6]
head(d)
d.pca = prcomp(d, scale = TRUE)
d.pca
# извлечем первую главную компоненту:
pca1 = d.pca$x[, 1]
head(pca1)

# выборочная дисперсия каждой компоненты:
summary(d.pca) # например, первые три компоненты имеют суммарную выборочную дисперсию равную 90% от суммарной выборочный дисперсии
# стоимости квадратного метра

# а вот первая главная компонента, отвечающая за планировку квартиры, слабо дифференцирует квартиры по стоимости квадратного метра
cor(y, pca1)

# выборочная дисперсия каждой компоненты на графике:
fviz_eig(d.pca)

# исходный набор данных в новых осях по горизонтали — pc1 по вертикали — pc2
# (если какие-то из наблюдений выделяются на графике, то, возможно, они являются нетипичными)
fviz_pca_biplot(d.pca, repel = TRUE)

# оценим регрессию цены квадратного метра на главные компоненты и на дамми walk
model = lm(y ~ d.pca$x[, 1] + d.pca$x[, 2] + d.pca$x[, 3] + d.pca$x[, 4] + d.pca$x[, 5] + data$walk)
summary(model)



# Задание 3. Ridge и LASSO
data = cars # рассмотрим встроенный набор данных cars
help(cars) # описание датасета
plot(cars$speed, cars$dist) # связь нелинейная, попробуем включить в модель квадрат и куб переменной speed дял учета нелинейности
data['speed2'] = data['speed'] ^ 2
data['speed3'] = data['speed'] ^ 3

# Несмотря на то, что у нас нет строгой мультиколлинеарности, мы вправе ожидать частичную мультиколлинеарность
cor(data)

y_unscaled = as.matrix(data[,2]) # сохраним исходную зависимую переменную в матрицу y_unscaled
x_unscaled = as.matrix(data[,c(1,3,4)]) # сохраним исходные объясняющие переменные в матрицу y_unscaled


# Оценим LASSO при lambda = 0.1
# Параметр alpha = 1, если оценивается LASSO и 0, если оценивается Ridge
model_lasso = glmnet(x_unscaled, y_unscaled, alpha = 1, lambda = 0.1,
                     standardize = FALSE,
                     standardize.response = FALSE, intercept = TRUE)
model_lasso$beta
model_lasso$a0

# Теперь посмотрим как изменятся оценки коэффициентов, перебирая разные lambda
lambdas = c(0.0001, 0.001, 0.01, 0.1, 1) # зададим набор разных lambda
lambdas
model_lasso = glmnet(X, y_unscaled, alpha = 1, lambda = lambdas,
                     standardize = FALSE,
                     standardize.response = FALSE, intercept = TRUE)
model_lasso$lambda
model_lasso$beta # оценки коэффициентов при разных lambda

# В угловых решениях некоторые оценки коэффициентов в точности оказываются равными нулю, т.е. LASSO без всякой проверки гипотез в каком-то смысле
# делит коэффициенты на “значимые” и “незначимые”. Оценка LASSO всегда есть, даже в случае жесткой мультиколлинеарности или если регрессоров больше, чем наблюдений.

plot(model_lasso, xvar = "lambda", label = TRUE) # график зависимости оценок коэффициентов от ln(lambda)

# Теперь попробуем подобрать оптимальный гиперпараметр lambda, используя алгоритм кросс-валидации
cv_lambda = cv.glmnet(x_unscaled,y_unscaled,alpha = 1,
                      standardize = TRUE,
                      standardize.response = FALSE, intercept = TRUE,
                      nfolds = 10)
cv_lambda # оптимальное lambda

# Теперь получим оценки коэффициентов lasso, используя оптимальную lambda
model_lasso_opt = glmnet(x_unscaled, y_unscaled, alpha = 1, lambda = cv_lambda$lambda.1se,
                         standardize = TRUE,
                         standardize.response = FALSE, intercept = TRUE)
coef(model_lasso_opt)


# Ridge-regression
# Оценим ридж-регрессию при lambda = 0.1
model_rr = glmnet(X, y_unscaled, alpha = 0, lambda = 0.1,
                  standardize = FALSE,
                  standardize.response = FALSE, intercept = TRUE)
coef(model_rr) # оценки коэффициентов модели

# Теперь посмотрим как изменятся оценки коэффициентов, перебирая разные lambda
lambdas = c(0.0001, 0.001, 0.01, 0.1, 1) # зададим набор разных lambda
lambdas
model_rr = glmnet(X, y_unscaled, alpha = 0, lambda = lambdas,
                  standardize = FALSE,
                  standardize.response = FALSE, intercept = TRUE)

coef(model_rr) # оценки коэффициентов при разных lambda
plot(model_rr, xvar = "lambda", label = TRUE) # график зависимости оценок коэффициентов от ln(lambda)


# Подберем оптимальную lambda для ridge регрессии
cv_lambda_rr = cv.glmnet(X, y_unscaled, alpha = 0,
                         standardize = FALSE,
                         standardize.response = FALSE, intercept = TRUE, nfolds = 10)
cv_lambda_rr

# Оценим модель для оптимального lambda
model_rr_opt = glmnet(X, y_unscaled, alpha = 0, lambda = cv_lambda_rr$lambda.min,
                      standardize = FALSE,
                      standardize.response = FALSE, intercept = TRUE)
coef(model_rr_opt)
predict(model_rr_opt, X)
