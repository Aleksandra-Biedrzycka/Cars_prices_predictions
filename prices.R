

#####################################################################################

## Element pipeline'u do projektu "Projekt własny- predykcja cen BMW:
## Pozyskanie danych metodą webscrapin

#####################################################################################

library(rvest)
library(stringr)
library(progress)
library(ggplot2)
library(dplyr)

# Scraping
N = 500
link = 'https://www.otomoto.pl/osobowe/bmw?page='
for(i in 1:N){
  download.file(paste0(link, i), destfile = paste0("scrapedpage-", i,".html"), quiet=TRUE)
  page <- read_html(paste0("scrapedpage-", i,".html"))
}

# Data
N = 250
results <- data.frame('price'=integer(), 'year'=integer(), 'mileage'=integer(), 'engine'=integer())
pb <- progress_bar$new(total=N)
for(i in 1:N){
  page <- read_html(paste0("scrapedpage-", i,".html"))
  
  price <- page %>% html_nodes(xpath = '//span[@class="ooa-1bmnxg7 e1b25f6f11"]') %>% html_text()
  eur_index <- str_detect(price, "EUR")
  price <- as.integer(str_replace_all(price, " |PLN|EUR", ""))
  price[eur_index] <- price[eur_index]*4.7
  print(price)
  
  year <- page %>% html_nodes(xpath = '//article/div/div/ul[1]/li[1]') %>% html_text() %>% str_replace(., " ", "") %>% as.integer()
  
  mileage <- page %>% html_nodes(xpath = '//article/div/div/ul[1]/li[2]') %>% html_text() %>% str_replace_all(., " |km", "") %>% as.integer()
  
  engine <- page %>% html_nodes(xpath = '//article/div/div/ul[1]/li[3]') %>% html_text() %>% str_replace_all(., " |cm3", "") %>% as.integer()
  
  
  if(length(price) == 0) price <- NA
  if(length(year) == 0) year <- NA
  if(length(mileage) == 0) mileage <- NA
  if(length(engine) == 0) engine <- NA
  
  print(price)
  print(year)
  
  results <- rbind(results, data.frame('price'=price, 'year'=year, 'mileage'=mileage, 'engine'=engine))
  print(dim(results))
  pb$tick()
}
summary(results)
hist(results$year)

results$age <- 2023-results$year

write.csv(results, "bmw.csv")

#opcionalne analizy wstępne:

#results <- read.csv("bmw.csv")

plot(x=results$year, y=results$price, pch=19)
summary(lm(price ~ year, data=results))
abline(lm(price ~ year, data=results))

lm(price ~ age, data=results)
plot(x=results$age, y=results$price, pch=19, xlab="Car age", ylab="Price")
abline(lm(price ~ age, data=results), lwd=2, col="dark red")


fit_p2 <- lm(price ~ age + I(age^2), data=results)
summary(fit_p2)
points(x = 0:50, y=predict(fit_p2, data.frame('age'=0:50)), lwd=2, col="pink", type='l')

fit_p3 <- lm(price ~ age + I(age^2) + I(age^3), data=results)
AIC(fit_p3)
summary(fit_p3)
points(x = 0:50, y=predict(fit_p3, data.frame('age'=0:50)), lwd=2, col="navy blue", type='l')

fit_log <- lm(price ~ age + I(log(age+1)), data=results)
AIC(fit_log)
summary(fit_log)
points(x = 0:50, y=predict(fit_log, data.frame('age'=0:50)), lwd=2, col="darkgoldenrod1", type='l')

