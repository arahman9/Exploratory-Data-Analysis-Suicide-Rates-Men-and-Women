library(readr)
library(dplyr)

#Data Frame of suicide rates
suicide_rates<- read_csv("Downloads/master 2.csv")

Sr15 <- filter(suicide_rates, year == 2015) #Suicide Rates in 2015
Sr15m <- filter(suicide_rates, sex == 'male' & year == 2015) #Male suicide rates in 2015
Sr15f <- filter(suicide_rates, sex == 'female' & year == 2015) #Female suicide rates in 2015

#Age groups Male
Sr15m_1524 <- filter(Sr15m, age == '15-24 years')
Sr15m_2534 <- filter(Sr15m, age == '25-34 years')
Sr15m_3554 <- filter(Sr15m, age == '35-54 years')
Sr15m_5574 <- filter(Sr15m, age == '55-74 years')
Sr15m_75 <- filter(Sr15m, age == '75+ years')
Sr15m_514 <- filter(Sr15m, age == '5-14 years')

#Age groups Female
Sr15f_1524 <- filter(Sr15f, age == '15-24 years')
Sr15f_2534 <- filter(Sr15f, age == '25-34 years')
Sr15f_3554 <- filter(Sr15f, age == '35-54 years')
Sr15f_5574 <- filter(Sr15f, age == '55-74 years')
Sr15f_75 <- filter(Sr15f, age == '75+ years')
Sr15f_514 <- filter(Sr15f, age == '5-14 years')

#Means
mean(Sr15f_514$`suicides/100k pop`)
mean(Sr15f_1524$`suicides/100k pop`)
mean(Sr15f_2534$`suicides/100k pop`)
mean(Sr15f_3554$`suicides/100k pop`)
mean(Sr15f_5574$`suicides/100k pop`)
mean(Sr15f_75$`suicides/100k pop`)

mean(Sr15m_514$`suicides/100k pop`)
mean(Sr15m_1524$`suicides/100k pop`)
mean(Sr15m_2534$`suicides/100k pop`)
mean(Sr15m_3554$`suicides/100k pop`)
mean(Sr15m_5574$`suicides/100k pop`)
mean(Sr15m_75$`suicides/100k pop`)

#QQ norm plots for females
qqnorm(Sr15f_514$`suicides/100k pop`)
qqline(Sr15f_514$`suicides/100k pop`)
qqnorm(Sr15f_1524$`suicides/100k pop`)
qqline(Sr15f_1524$`suicides/100k pop`)
qqnorm(Sr15f_2534$`suicides/100k pop`)
qqline(Sr15f_2534$`suicides/100k pop`)
qqnorm(Sr15f_3554$`suicides/100k pop`)
qqline(Sr15f_3554$`suicides/100k pop`)
qqnorm(Sr15f_5574$`suicides/100k pop`)
qqline(Sr15f_5574$`suicides/100k pop`)
qqnorm(Sr15f_75$`suicides/100k pop`)
qqline(Sr15f_75$`suicides/100k pop`)

#QQ norm Plots for males
qqnorm(Sr15m_514$`suicides/100k pop`)
qqline(Sr15m_514$`suicides/100k pop`)
qqnorm(Sr15m_1524$`suicides/100k pop`)
qqline(Sr15m_1524$`suicides/100k pop`)
qqnorm(Sr15m_2534$`suicides/100k pop`)
qqline(Sr15m_2534$`suicides/100k pop`)
qqnorm(Sr15m_3554$`suicides/100k pop`)
qqline(Sr15m_3554$`suicides/100k pop`)
qqnorm(Sr15m_5574$`suicides/100k pop`)
qqline(Sr15m_5574$`suicides/100k pop`)
qqnorm(Sr15m_75$`suicides/100k pop`)
qqline(Sr15m_75$`suicides/100k pop`)

#Box plots for each age group
boxplot(Sr15m_514$`suicides/100k pop`,Sr15f_514$`suicides/100k pop`)
boxplot(Sr15m_1524$`suicides/100k pop`,Sr15f_1524$`suicides/100k pop`)
boxplot(Sr15m_2534$`suicides/100k pop`,Sr15f_2534$`suicides/100k pop`)
boxplot(Sr15m_3554$`suicides/100k pop`,Sr15f_3554$`suicides/100k pop`)
boxplot(Sr15m_5574$`suicides/100k pop`,Sr15f_5574$`suicides/100k pop`)
boxplot(Sr15m_75$`suicides/100k pop`,Sr15f_1524$`suicides/100k pop`)

#Difference in means for each age group
thetahat514 <- mean(Sr15m_514$`suicides/100k pop`) - mean(Sr15f_514$`suicides/100k pop`)
thetahat1524 <- mean(Sr15m_1524$`suicides/100k pop`) - mean(Sr15f_1524$`suicides/100k pop`)
thetahat2534 <- mean(Sr15m_2534$`suicides/100k pop`) - mean(Sr15f_2534$`suicides/100k pop`)
thetahat3554 <- mean(Sr15m_3554$`suicides/100k pop`) - mean(Sr15f_3554$`suicides/100k pop`)
thetahat5574 <- mean(Sr15m_5574$`suicides/100k pop`) - mean(Sr15f_5574$`suicides/100k pop`)
thetahat75 <- mean(Sr15m_75$`suicides/100k pop`) - mean(Sr15f_75$`suicides/100k pop`)


#Bootstrap resampling for each age group
nx <- length(Sr15m_514$`suicides/100k pop`) 
ny <- length(Sr15f_514$`suicides/100k pop`) 
SE <- sqrt(var(Sr15m_514$`suicides/100k pop`)/nx + var(Sr15f_514$`suicides/100k pop`)/ny)
N <- 10000
Tstar <- numeric(N)
for (i in 1:N)
{
  bootx <- sample(Sr15m_514$`suicides/100k pop`, nx, replace = TRUE)
  booty <- sample(Sr15f_514$`suicides/100k pop`, ny, replace = TRUE)
  Tstar[i] <- (mean(bootx) - mean(booty) - thetahat514) /
    sqrt(var(bootx)/nx + var(booty)/ny)
}
thetahat514 - quantile(Tstar, c(.975, .025)) * SE
SE

t.test(Sr15m_514$`suicides/100k pop`, Sr15f_514$`suicides/100k pop`)$conf


nx <- length(Sr15m_1524$`suicides/100k pop`) 
ny <- length(Sr15f_1524$`suicides/100k pop`) 
SE <- sqrt(var(Sr15m_1524$`suicides/100k pop`)/nx + var(Sr15f_1524$`suicides/100k pop`)/ny)
N <- 10000
Tstar <- numeric(N)
for (i in 1:N)
{
  bootx <- sample(Sr15m_1524$`suicides/100k pop`, nx, replace = TRUE)
  booty <- sample(Sr15f_1524$`suicides/100k pop`, ny, replace = TRUE)
  Tstar[i] <- (mean(bootx) - mean(booty) - thetahat1524) /
    sqrt(var(bootx)/nx + var(booty)/ny)
}
thetahat1524 - quantile(Tstar, c(.975, .025)) * SE
SE


nx <- length(Sr15m_2534$`suicides/100k pop`) 
ny <- length(Sr15f_2534$`suicides/100k pop`) 
SE <- sqrt(var(Sr15m_2534$`suicides/100k pop`)/nx + var(Sr15f_2534$`suicides/100k pop`)/ny)
N <- 10000
Tstar <- numeric(N)
for (i in 1:N)
{
  bootx <- sample(Sr15m_2534$`suicides/100k pop`, nx, replace = TRUE)
  booty <- sample(Sr15f_2534$`suicides/100k pop`, ny, replace = TRUE)
  Tstar[i] <- (mean(bootx) - mean(booty) - thetahat2534) /
    sqrt(var(bootx)/nx + var(booty)/ny)
}
thetahat2534 - quantile(Tstar, c(.975, .025)) * SE
SE

nx <- length(Sr15m_3554$`suicides/100k pop`) 
ny <- length(Sr15f_3554$`suicides/100k pop`) 
SE <- sqrt(var(Sr15m_3554$`suicides/100k pop`)/nx + var(Sr15f_3554$`suicides/100k pop`)/ny)
N <- 10000
Tstar <- numeric(N)
for (i in 1:N)
{
  bootx <- sample(Sr15m_3554$`suicides/100k pop`, nx, replace = TRUE)
  booty <- sample(Sr15f_3554$`suicides/100k pop`, ny, replace = TRUE)
  Tstar[i] <- (mean(bootx) - mean(booty) - thetahat3554) /
    sqrt(var(bootx)/nx + var(booty)/ny)
}
thetahat3554 - quantile(Tstar, c(.975, .025)) * SE
SE

nx <- length(Sr15m_5574$`suicides/100k pop`) 
ny <- length(Sr15f_5574$`suicides/100k pop`) 
SE <- sqrt(var(Sr15m_5574$`suicides/100k pop`)/nx + var(Sr15f_5574$`suicides/100k pop`)/ny)
N <- 10000
Tstar <- numeric(N)
for (i in 1:N)
{
  bootx <- sample(Sr15m_5574$`suicides/100k pop`, nx, replace = TRUE)
  booty <- sample(Sr15f_5574$`suicides/100k pop`, ny, replace = TRUE)
  Tstar[i] <- (mean(bootx) - mean(booty) - thetahat5574) /
    sqrt(var(bootx)/nx + var(booty)/ny)
}
thetahat5574 - quantile(Tstar, c(.975, .025)) * SE
SE

nx <- length(Sr15m_75$`suicides/100k pop`) 
ny <- length(Sr15f_75$`suicides/100k pop`) 
SE <- sqrt(var(Sr15m_75$`suicides/100k pop`)/nx + var(Sr15f_75$`suicides/100k pop`)/ny)
N <- 10000
Tstar <- numeric(N)
for (i in 1:N)
{
  bootx <- sample(Sr15m_75$`suicides/100k pop`, nx, replace = TRUE)
  booty <- sample(Sr15f_75$`suicides/100k pop`, ny, replace = TRUE)
  Tstar[i] <- (mean(bootx) - mean(booty) - thetahat75) /
    sqrt(var(bootx)/nx + var(booty)/ny)
}
thetahat75 - quantile(Tstar, c(.975, .025)) * SE
SE

#Bootstrap t test for each age group
Sr15_514 <- filter(Sr15, age == "5-14 years")
t.test(`suicides/100k pop` ~ sex, data = Sr15_514)

Sr15_1524 <- filter(Sr15, age == "15-24 years")
t.test(`suicides/100k pop` ~ sex, data = Sr15_1524)

Sr15_2534 <- filter(Sr15, age == "25-34 years")
t.test(`suicides/100k pop` ~ sex, data = Sr15_2534)

Sr15_3554 <- filter(Sr15, age == "35-54 years")
t.test(`suicides/100k pop` ~ sex, data = Sr15_3554)

Sr15_5574 <- filter(Sr15, age == "55-74 years")
t.test(`suicides/100k pop` ~ sex, data = Sr15_5574)

Sr15_75 <- filter(Sr15, age == "75+ years")
t.test(`suicides/100k pop` ~ sex, data = Sr15_75)


#Creating a seperate column for difference in means for creating a regression model
agegroup <- list("5-14 years","15-24 years","25-34 years","35-54 years","55-74 years", "75+ years")

diffmeans <- list()

x <- 0
for( i in Sr15f$country){
  x <- x + 1
  if(x == 7){
    x <- 1
    print(agegroup[x])
    tempf <- filter(Sr15f, country == i, age == agegroup[x] )
    tempm <- filter(Sr15m, country == i, age == agegroup[x] )
    diff <- abs(tempm$`suicides/100k pop`-tempf$`suicides/100k pop`)
    diffmeans <- c(diffmeans,diff)
  }
  else if(x != 7){
    print(agegroup[x])
    tempf <- filter(Sr15f, country == i, age == agegroup[x] )
    tempm <- filter(Sr15m, country == i, age == agegroup[x] )
    diff <- abs(tempm$`suicides/100k pop`-tempf$`suicides/100k pop`)
    diffmeans <- c(diffmeans,diff)
  }
}

Sr15m$difference = diffmeans
#Regression model
Sr15m.lm <- lm(Sr15m$`gdp_per_capita ($)` ~ unlist(Sr15m$difference))
Sr15m.lm



unlist(Sr15m$difference)
plot(Sr15m$`gdp_per_capita ($)`,Sr15m$difference)
cor(unlist(Sr15m$difference),Sr15m$`gdp_per_capita ($)`)