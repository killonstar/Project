# Main script

# Library

library(rusquant)
library(quantmod)
library(magrittr)
library(rlist)
library(lpSolve)


# Packages

source("packages/libfun.R")


# Основные параметры (время и период)
from.date <- "2013-09-26" # Параметр для изменения !!!
to.date <- "2014-09-26" # Параметр для изменения !!!
period <- "day" # Параметр для изменения !!!

# Первая корзина
tickers1 <- c("LKOH", "GAZP") # Параметр для изменения !!!
list1 <- BasFun1(tickers1,from.date,to.date,period) # Первая корзина

# Вторая корзина
tickers2 <- c("SBER") # Параметр для изменения !!!
list2 <- BasFun1(tickers2,from.date,to.date,period) # Вторая корзина

# Ввод индекса
tickers3 <- c("MICEX") # Параметр для изменения !!!
list3 <- BasFun1(tickers3,from.date,to.date,period) # Вторая корзина

# Приращения, ковариация, дисперсия индекса и беты
incr.bas1 <- Increment(list1) # Приращение первой корзины
incr.bas2 <- Increment(list2) # Приращение второй корзины
incr.index <- Increment(list3) # Приращение индекса
var.index <- as.numeric(lapply(incr.index, function(x) var(x))) # Дисперсия индекса

covar.bas1 <- Covar(incr.bas1) # Ковариация первой корзины
covar.bas2 <- Covar(incr.bas2) # Ковариация второй корзины

beta.bas1 <- lapply(covar.bas1, function(x) Beta(x)) # Беты первой корзины
beta.bas2 <- lapply(covar.bas2, function(x) Beta(x)) # Беты второй корзины

irrfac <- Irr(tickers1,tickers2) # Коэффициент неравномерности
sum.betaall <- Summa(beta.bas1, beta.bas2) # Общая сумма бет в двух корзинах
inter.bas1 <- Inter2(beta.bas1,beta.bas2) # Расчет корзин
inter.bas2 <- Inter4(beta.bas1,beta.bas2) # Расчет корзин

# сумма весов двух корзин
sum.weightall <- Summa(inter.bas1, inter.bas2) # Общая сумма весов в двух корзинах
weightper.basall <- WeightCOOL(inter.bas1,inter.bas2) # Идеальный вес в процентах

# Спецификация контрактов (количество лотов)
tick1 <- tickers1 # 
tick2 <- tickers2 # 
# tick1 вывести для вписания правильного значения лотов
writing1 <- c(10,100) # Параметры для изменения !!!
# tick2 вывести для вписания правильного значения лотов
writing2 <- c(100) # Параметры для изменения !!!

# Средние цены, ср.цены с учетом лотов, сумма ср.цен с лотами в двух корзинах
price.info <- PriceInfo(list1,list2,writing1,writing2)

# Оптимальное кол-во лотов
# weightper.basall - вывести для сравнения с lot и выбора оптимальных значений
lot <- FunCOMBIN(tickers1,tickers2,price.info[[3]],price.info[[4]]) # Таблица оптимальных лотов (вывести)

# Спред
# Вывести tickers1 и tickers2
lot.spread1 <- c(5,1) # Указываем количество лотов, которые мы выбрали для первой корзины
lot.spread2 <- c(1) # Указываем количество лотов, которые мы выбрали для второй корзины
Spread(list1,writing1,lot.spread1,list2,writing2,lot.spread2) # График (тоже вывести)
