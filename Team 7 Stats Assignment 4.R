install.packages("readxl")
library("readxl")

reviews_data <- read_excel(file.choose(), col_names = TRUE)
star_ratings <- c(reviews_data$printed_star)

mean.total <- mean(star_ratings)
stdev.total <- sd(star_ratings)/sqrt(length(star_ratings))

upper.95.total <- mean.total + 2*stdev.total
lower.95.total <- mean.total - 2*stdev.total

barplot(table(star_ratings), xlab = ("Star Ratings for Trattoria Vico"), ylab = ("Number of reviews"))

Local.stars <- reviews_data$printed_star[reviews_data$patron_type == 'local']
Tourist.stars <- reviews_data$printed_star[reviews_data$patron_type == 'tourist']

mean.local <- mean(Local.stars)
stdev.local <- sd(Local.stars)/sqrt(length(Local.stars))

upper.95.local <- mean.local + 2*stdev.local
lower.95.local <- mean.local - 2*stdev.local

mean.tourist <- mean(Tourist.stars)
stdev.tourist <- sd(Tourist.stars)/sqrt(length(Tourist.stars))

upper.95.tourist <- mean.tourist + 2*stdev.tourist
lower.95.tourist <- mean.tourist - 2*stdev.tourist


data.foodprice <- read_excel(file.choose(), col_names = TRUE)
data.foodprice$food[data.foodprice$food == "NA"] <- NA
data.foodprice$price[data.foodprice$price == "NA"] <- NA
clean_data.food <- data.foodprice[!is.na(data.foodprice$food), ]
clean_data.price <- data.foodprice[!is.na(data.foodprice$price), ]

class(clean_data.food$food)
class(clean_data.price$price)
clean_data.food$food <- as.numeric(clean_data.food$food)
clean_data.price$price <- as.numeric(clean_data.price$price)
mean(clean_data.food$food, na.rm = TRUE)
mean(clean_data.price$price, na.rm = TRUE)

xbar_foodweekday <- mean(clean_data.food$food[clean_data.food$day_type == "weekday"])
weekdays <- length(clean_data.food$food[clean_data.food$day_type == "weekday"])
sd_foodweekday <- sd(clean_data.food$food[clean_data.food$day_type == "weekday"]) / sqrt(weekdays)
lower95_foodweekday <- xbar_foodweekday - 2*sd_foodweekday
upper95_foodweekday <- xbar_foodweekday + 2*sd_foodweekday

xbar_foodweekend <- mean(clean_data.food$food[clean_data.food$day_type == "weekend"])
weekends <- length(clean_data.food$food[clean_data.food$day_type == "weekend"])
sd_foodweekend <- sd(clean_data.food$food[clean_data.food$day_type == "weekend"]) / sqrt(weekends)
lower95_foodweekend <- xbar_foodweekend - 2*sd_foodweekend
upper95_foodweekend <- xbar_foodweekend + 2*sd_foodweekend

xbar_pricelocal <- mean(clean_data.price$price[clean_data.price$patron_type == "local"])
locals <- length(clean_data.price$price[clean_data.price$patron_type == "local"])
sd_pricelocal <- sd(clean_data.price$price[clean_data.price$patron_type == "local"]) / sqrt(locals)
lower95_pricelocal <- xbar_pricelocal - 2*sd_pricelocal
upper95_pricelocal <- xbar_pricelocal + 2*sd_pricelocal

xbar_pricetourist <- mean(clean_data.price$price[clean_data.price$patron_type == "tourist"])
tourists <- length(clean_data.price$price[clean_data.price$patron_type == "tourist"])
sd_pricetourist <- sd(clean_data.price$price[clean_data.price$patron_type == "tourist"]) / sqrt(tourists)
lower95_pricetourist <- xbar_pricetourist - 2*sd_pricetourist
upper95_pricetourist <- xbar_pricetourist + 2*sd_pricetourist
