# Семинар 15. Оценка эффектов воздействия
library(ggplot2) # графики
library(plotly) # графики
library(rio) # импорт и экспорт данных в разных форматах
library(readr) # импорт
library(foreign) # импорт
library(rdrobust) # RDD

# Задание 1. DID
# Card D., Krueger A.B. (1994). Minimum Wages and Employment: A Case Study of the Fast-Food Industry in New Jersey and Pennsylvania
# В 1992 году в штате Нью-Джерси минимальный размер оплаты труда был увеличен с 4,25 до 5,05 долларов.

# Гипотеза: повышение МРОТ должно повлиять на изменение занятости работников с низкой квалификацией.

d <- read_csv("https://docs.google.com/uc?id=10h_5og14wbNHU-lapQaS1W6SBdzI7W6Z&export=download")

# Построим гистограммы распределения почасовой зарплаты в двух штатах до повышения МРОТ (февраль,1992) как в статье
x_st_wage_before_nj <- d$x_st_wage_before[data$d_nj == 1] # почасовая зарплата в Нью-Джерси до повышения
x_st_wage_before_pa <- d$x_st_wage_before[data$d_pa == 1] # почасовая зарплата в Пенсильвании до повышения

xbins <- list(start = 4.20, end = 5.60, size = 0.1)

pic1 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = x_st_wage_before_nj,
                xbins = xbins,
                histnorm = "percent",
                name = "Нью-Джерси") %>%
  add_histogram(x = x_st_wage_before_pa,
                xbins = xbins,
                histnorm = "percent",
                name = "Пенсильвания") %>%
  layout(barmode = "group", title = "Февраль 1992 (до повышения МРОТ)",
         xaxis = list(tickvals = seq(4.25, 5.55, 0.1),
                      title = "Почасовая зарплата, в долл."),
         yaxis = list(range = c(0, 40)),
         margin = list(b = 100,
                       l = 80,
                       r = 80,
                       t = 80,
                       pad = 0,
                       autoexpand = TRUE))
pic1

# Построим гистограммы распределения почасовой зарплаты в двух штатах после повышения МРОТ (ноябрь, 1992)
x_st_wage_after_nj <- d$x_st_wage_after[data$d_nj == 1] # почасовая зарплата в Нью-Джерси после повышения
x_st_wage_after_pa <- d$x_st_wage_after[data$d_pa == 1] # почасовая зарплата в Пенсильвании после повышения

pic2 <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = x_st_wage_after_nj,
                xbins = xbins,
                histnorm = "percent",
                name = "Нью-Джерси") %>%
  add_histogram(x = x_st_wage_after_pa,
                xbins = xbins,
                histnorm = "percent",
                name = "Пенсильвания") %>%
  layout(barmode = "group", title = "Ноябрь, 1992",
         xaxis = list(tickvals = seq(4.25, 5.55, 0.1),
                      title = "Почасовая зарплата, в долл."),
         yaxis = list(range = c(0, 100)),
         margin = list(b = 100,
                       l = 80,
                       r = 80,
                       t = 80,
                       pad = 0,
                       autoexpand = TRUE))
pic2

# На рисунках pic1 и pic2 показано распределение начальной заработной платы в двух штатах до и после повышения.
# До повышения МРОТ (февраль, 1992) распределение в Нью-Джерси и Пенсильвании было очень похожим.
# После повышения МРОТ практически все рестораны в Нью-Джерси, которые платили менее 5,05 долларов в час,
# сообщили о стартовой заработной плате, равной новой ставке.

# Рассчитаем эффект воздействия с помощью DID
table_res = data %>% group_by(d_nj) %>%
  summarise(mean.before = mean(y_ft_employment_before, na.rm = TRUE),
            mean.after = mean(y_ft_employment_after, na.rm = TRUE),
            var.before = var(y_ft_employment_before, na.rm = TRUE),
            var.after = var(y_ft_employment_after, na.rm = TRUE),
            n.before = sum(!is.na(y_ft_employment_before)),
            n.after = sum(!is.na(y_ft_employment_after))) %>%
  mutate(se.mean.before = sqrt(var.before/n.before)) %>%
  mutate(se.mean.after = sqrt(var.after/n.after))

# Эффект воздействия
delta = (table_res$mean.after[2] - table_res$mean.before[2]) - (table_res$mean.after[1] - table_res$mean.before[1])
delta


# Задание 2. RDD
# Импортируем данные
data_rdd <- foreign::read.dta("https://docs.google.com/uc?id=1xWHmST5FYcfLxe9V7Hwqd2LIy_A3ninG&export=download")
# Оставим только используемые переменные
data_rdd <- data_rdd %>% rename(x_score_victorymargin = rv,
                                y_donationshare = dv_money,
                                cov_statelevel = statelevel,
                                cov_total_race_money = total_race_money,
                                cov_total_votes = total_votes,
                                cov_dem_inc = dem_inc,
                                cov_rep_inc = rep_inc,
                                cov_total_group_money = total_group_money) %>%
  select(x_score_victorymargin,
                y_donationshare,
                cov_statelevel,
                cov_total_race_money,
                cov_total_votes,
                cov_dem_inc,
                cov_rep_inc,
                cov_total_group_money,
                state,
                dist,
                year)

data_rdd$colour[data_rdd$x_score_victorymargin >= 0] <- "Treatment Group"
data_rdd$colour[data_rdd$x_score_victorymargin < 0] <- "Control Group"

plot_ly(data = data_rdd,
        type = "scatter",
        mode = "markers",
        x = data_rdd$x_score_victorymargin,
        y = data_rdd$y_donationshare,
        color = data_rdd$colour,
        marker = list(size = 3)) %>%
  layout(xaxis = list(title = "Democratic margin of victory at t", dtick = 25),
         yaxis = list(title = "Democratic share of contributions at t+1"))


# Total money in race
summary(rdrobust(data_rdd$cov_total_race_money, data_rdd$x_score_victorymargin, all = TRUE))
rdplot(data_rdd$cov_total_race_money, data_rdd$x_score_victorymargin,
       x.lim = c(-10,10),
       y.lim = c(0,400000),
       x.lab = "Democratic margin of victory at t",
       y.lab = "Total money in race at t+1", title = "")


# Total votes in race
summary(rdrobust(data_rdd$cov_total_votes, data_rdd$x_score_victorymargin,all = TRUE))
rdplot(data_rdd$cov_total_votes,data_rdd$x_score_victorymargin,
       x.lim = c(-10,10),
       y.lim = c(0,50000),
       x.lab = "Democratic margin of victory at t",
       y.lab = "Total votes in race at t+1", title = "")


# Локальная полиномиальная регрессия в RDD
rdd <- rdrobust(data_rdd$y_donationshare, data_rdd$x_score_victorymargin, c = 0, all = TRUE)
summary(rdd)

# Изобразим результаты RDD на графике
rdplot(data_rdd$y_donationshare, data_rdd$x_score_victorymargin, c = 0,
       x.lab = "Democratic margin of victory at t",
       y.lab = "Democratic share of contributions at t+1",
       title = "RDD")

# Плацебо тест: попробуем изменить cutoff
# cutoff (значение переменной отбора) c = 1
summary(rdrobust(data_rdd$y_donationshare,data_rdd$x_score_victorymargin, c = 1, all = TRUE))
# cutoff (значение переменной отбора) c = -3
summary(rdrobust(data_rdd$y_donationshare,data_rdd$x_score_victorymargin, c = -3, all = TRUE))
