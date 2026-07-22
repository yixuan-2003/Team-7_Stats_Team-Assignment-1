install.packages("readxl")
library("readxl")
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
