library(ggplot2)
library(dplyr)

# read data .csv 
usedcars <- read.csv("/home/hyuri/Dropbox/study/Machine-Learning-with-R-datasets-master/usedcars.csv",
                     stringsAsFactors = FALSE)
# str and function return structures of datas, vectos, list and et..
str(usedcars)

# summary statistcs describe variables numerics, return values (Min, 1 Qu, Medina, 3 Qu, Mean, Max)
summary(usedcars$year)
summary(usedcars$price)
summary(usedcars$mileage) 
# or
summary(usedcars[c("year", "price" ,  "mileage")])


############## Numerics Variables ##################################################
# boxplot
boxplot(usedcars$price, main="Boxplot of Used Car Prices",
        ylab="Price ($)")

ggplot(usedcars, aes(,price))+
  geom_boxplot()

boxplot(usedcars$mileage, main="Boxplot of Used Car Prices",
        ylab="Price ($)")

ggplot(usedcars, aes(,mileage))+
  geom_boxplot()

# histogram
hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price ($)")

hist(usedcars$mileage, main = "Histogram of Used Car mileage",
     xlab = "Price ($)")

ggplot(usedcars, aes(x=price)) + 
  geom_histogram()


# variance and standart deviation
var(usedcars$price)
sd(usedcars$price)

############## Categorical Variables ##################################################

table(usedcars$model)
table(usedcars$color)
table(usedcars$transmission)
table(usedcars$year)

table_model <- table(usedcars$model)
prop.table(table_model)

color_pdc <- table(usedcars$color)
color_pdc <- prop.table(color_pdc)*100
round(color_pdc, digits = 1)


# A scatterplot is a diagram that visualizes a bivariate relationship

plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")

# basic scatterplot
ggplot(usedcars, aes(x=mileage, y=price)) + 
  geom_point()

library(gmodels)
# examine a relationship between two nominal variables, a two-way cross-tabulation

usedcars$conservative <- usedcars$color %in% c("Black", "Gray", "Silver", "White")
table(usedcars$conservative)

CrossTable(x = usedcars$model, y = usedcars$conservative, chisq = TRUE )
