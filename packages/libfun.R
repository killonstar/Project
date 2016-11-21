# libfun


# Загрузка котировок
BasFun1 <- function(n1,n2,n3,n4){
  n.ticker <- length(n1)
  for(i in 1:n.ticker){
    data <- getSymbols(n1[i], from = n2, to = n3, period = n4, src = "Finam", auto.assign = T)
    text <- paste0("bas1.",n1[i]," <- ",n1[i],"[,4]")
    eval(parse(text=text))
  }
  list1 <- list()
  for(i in 1:n.ticker){
    text <- paste0('list1 <- list.append(list1, bas1.',n1[i],')')
    eval(parse(text=text))
  }
  list1 <- do.call(merge, list1)
  return(list1) 
}

# Приращение
Increment <- function(n1){
  x <- as.data.frame(log(n1 / lag(n1)))
  x[is.na(x)] <- 0
  return(x)
}

# Ковариация
Covar <- function(n1, incr.index){
  cov(incr.index, n1) 
}

# Беты
Beta <- function(n1, var.index){
  n1/var.index
}

# Коэффициент неравномерности (большую должны делить на меньшую)
Irr <- function(n1,n2){
  x1 <- length(n1)
  x2 <- length(n2)
  if(x1 < x2){
    z <- x2/x1
  }
  else{
    z <- x1/x2
  }
  return(z)
}

# Расчет суммы
Summa <- function(n1,n2){
  x1 <- rowSums(as.data.frame(n1))
  x2 <- rowSums(as.data.frame(n2))
  x3 <- x1 + x2
  return(x3)
} 

# Расчет первой корзины по отношению ко второй
Inter2 <- function(n1,n2, sum.betaall, irrfac){
  x1 <- length(n1)
  x2 <- length(n2)
  Inter1 <- function(n1, sum.betaall, irrfac){
    sum.betaall/n1*irrfac
  }
  if(x1 < x2){
    z <- lapply(n1, function(x) Inter1(x, sum.betaall, irrfac))
  }
  else
  {
    z <- lapply(n2, function(x) Inter1(x, sum.betaall, irrfac))
  }
  return(z)
}

Inter4 <- function(n1,n2, sum.betaall, irrfac){
  x1 <- length(n1)
  x2 <- length(n2)
  Inter3 <- function(n1, sum.betaall, irrfac){
    sum.betaall/n1
  }
  if(x1 > x2){
    z <- lapply(n1, function(x) Inter3(x, sum.betaall, irrfac))
  }
  else
  {
    z <- lapply(n2, function(x) Inter3(x, sum.betaall, irrfac))
  }
  return(z)
}

# Идеальный вес в процентах
WeightCOOL <- function(n1,n2, inter.bas1, inter.bas2, tickers1, tickers2, sum.weightall){
  x1 <- n1
  x2 <- n2
  y1 <- length(inter.bas1)
  y2 <- length(inter.bas2)
  z1 <- length(tickers1)
  z2 <- length(tickers2)
  Weightcool <- function(n1, sum.weightall){
    n1/sum.weightall*100
  }
  weightper.bas1 <- lapply(x1, function(x) Weightcool(x, sum.weightall)) 
  weightper.bas2 <- lapply(x2, function(x) Weightcool(x, sum.weightall))
  weightper.basall <- list()
  if(y1 == z1){
    weightper.basall <- list.append(weightper.basall, weightper.bas1, weightper.bas2)
    weightper.basall <- round(as.numeric(as.data.frame(weightper.basall)), digits = 3)
  }
  else{
    weightper.basall <- list.append(weightper.basall, weightper.bas2, weightper.bas1)
    weightper.basall <- round(as.numeric(as.data.frame(weightper.basall)), digits = 3)
  }
  return(weightper.basall)
}

PriceInfo <- function(n1,n2,n3,n4)
{
  ob.mean.bas1 <- as.data.frame(as.list(colMeans(n1, na.rm = T))) # mean.bas1
  ob.mean.bas2 <- as.data.frame(as.list(colMeans(n2, na.rm = T))) # mean.bas1
  ob.price1 <- round(ob.mean.bas1, digits = 3) * n3 # Цена первой корзины с учетом лотов для фьючерсов # price1
  ob.price2 <- round(ob.mean.bas2, digits = 3) * n4 # Цена второй корзины с учетом лотов для фьючерсов # price2 
  ob.sum.meanall <- rowSums(ob.price1) + rowSums(ob.price2) # Общая сумма цен с лотами в двух корзинах # sum.meanall
  x <- ls()
  vec <- grep("ob.", x) %>% x[.]
  len <- length(vec)
  list4 <- list()
  for(i in 1:len){
    text <- paste0('list4 <- list.append(list4,',vec[i],')')
    eval(parse(text=text))
  }
  return(list4)
}

# Создание комбинации и объединение корзин
FunCOMBIN <- function(n1,n2,n3,n4, weightper.basall){
  length.basaall <- length(n1) + length(n2) # Количество инструментов в корзинах
  meanbas.all <- list() # Empty list
  m1 <- n3
  m2 <- n4
  meanbas.all <- round(unlist(list.append(meanbas.all, m1, m2)), digits = 3) # Объединение двух корзин в вектор
  lotik <- seq(1,8,by=1) # Создание комбинации
  combin <- matrix(rep(lotik, length.basaall), ncol = length.basaall)
  combin <- expand.grid(as.data.frame.matrix(combin)) # Готовая комбинация
  combin.basall <- data.frame(mapply(`*`,combin,meanbas.all)) # Цена с учетом лотов
  combin.sum <- as.data.frame(rowSums(combin.basall)) # сумма цен с лотами
  combin.percent <- round(data.frame(mapply(`/`,combin.basall,combin.sum))*100, digits = 3) # 
  combin.percent <- as.data.frame(list.append(combin.percent, combin))
  weightper.basall <- weightper.basall 
  Result <- function(n1,n2){
    com <- n1
    wec <- n2
    two <- com
    copy.two <- two
    copy.two$TWO <- copy.two[,1] + copy.two[,2]
    copy.two$IDTWO <- wec[1] + wec[2]
    copy.two$ReTWO <- abs(copy.two$IDTWO - copy.two$TWO)
    three <- copy.two[copy.two$ReTWO <= 12,]
    three$V1R <- abs(wec[1] - three$V1)
    four <- three[three$V1R <= 3,]
    four$V2R <- abs(wec[2] - four$V2)
    five <- four[four$V2R <= 3,]
  }
  lot <- Result(combin.percent,weightper.basall[1:2])
  return(lot)
}

Result <- function(n1,n2){
  com <- n1
  wec <- n2
  two <- com
  copy.two <- two
  copy.two$TWO <- copy.two[,1] + copy.two[,2]
  copy.two$IDTWO <- wec[1] + wec[2]
  copy.two$ReTWO <- abs(copy.two$IDTWO - copy.two$TWO)
  three <- copy.two[copy.two$ReTWO <= 12,]
  three$V1R <- abs(wec[1] - three$V1)
  four <- three[three$V1R <= 3,]
  four$V2R <- abs(wec[2] - four$V2)
  five <- four[four$V2R <= 3,]
}

# Спред
Spread <- function(list1,writing1,lot.spread1,list2,writing2,lot.spread2){
  bas1.sum <- as.data.frame(list1) # Date.frame
  bas1.sum <- data.frame(mapply(`*`,bas1.sum,writing1)) # Корректируем цены для фьючерсов
  bas1.sum <- data.frame(mapply(`*`,bas1.sum,lot.spread1)) # Умножаем на количество лотов, которые мы выбрали для первой корзины
  bas1.sum <- as.data.frame(rowSums(bas1.sum)) # сумма цен с лотами
  
  bas2.sum <- as.data.frame(list2) # Date.frame
  bas2.sum <- data.frame(mapply(`*`,bas2.sum,writing2)) # Корректируем цены для фьючерсов
  bas2.sum <- data.frame(mapply(`*`,bas2.sum,lot.spread2)) # Умножаем на количество лотов, которые мы выбрали для второй корзины
  bas2.sum <- as.data.frame(rowSums(bas2.sum)) # сумма цен с лотами
  
  basall.spread <- abs(bas1.sum - bas2.sum) 
  a <- plot(basall.spread[,1], type = "l") # График спреда
  min.spread <- min(basall.spread[,1]); max.spread <- max(basall.spread[,1])
  mean.spread <- c(min.spread, max.spread)
  mean.spread <- abs(mean(mean.spread)) # Средний спред
  abline(h = mean.spread, col = "red", lw = 2) # Линия среднего спреда на графике
}