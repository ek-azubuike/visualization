# Section 4 Exercises
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
str(titanic)

titanic %>% 
  ggplot(aes(x = Age, group = Sex, fill = Sex)) +
  geom_density(alpha = 0.2) 

titanic %>% 
  ggplot(aes(x = Age, fill = Sex)) +
  geom_histogram() +
  facet_wrap(. ~ Sex)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% 
  filter(!is.na(Age)) %>% 
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()

titanic %>% 
  ggplot(aes(x = Survived, group = Sex, fill = Sex)) +
  geom_bar(position = "stack")

titanic %>% 
  ggplot(aes(x = Age, fill = Survived)) +
  geom_density(alpha = 0.2, bw = 1)

titanic %>% 
  group_by(Age) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  arrange(desc(prop))

titanic %>% 
  filter(Fare != 0) %>% 
  ggplot(aes(x = Survived, y = Fare)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_y_continuous(transform = "log2")

titanic %>% 
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar()

titanic %>% 
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(position = position_fill())

titanic %>% 
  ggplot(aes(x = Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

titanic %>% 
  ggplot(aes(x = Age, fill = Survived)) +
  geom_density(alpha = 0.6, stat = "count") +
  facet_grid(Sex ~ Pclass)

# Properties of Stars
library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)  

str(stars)

stars %>% 
  ggplot(aes(x = magnitude)) +
  geom_density()

stars %>% 
  ggplot(aes(x = temp)) +
  geom_density()

stars %>% 
  ggplot(aes(x = temp,
             y = magnitude)) +
  geom_point()

stars %>% 
  ggplot(aes(x = temp,
             y = magnitude)) +
  geom_point() +
  scale_x_continuous(transform = "log10") +
  scale_y_reverse() +
  scale_x_reverse() 

stars %>% 
  filter(temp > 5000) %>% 
  arrange(desc(magnitude))

stars %>% 
  arrange(temp, magnitude)

stars %>% 
  ggplot(aes(x = temp,
             y = magnitude,
             color = type)) +
  geom_point() +
  scale_x_continuous(transform = c("log10", "reverse")) +
  scale_y_reverse()

# Climate Change 
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>% 
  pull(year) %>% 
  max()

emisssion.max <- temp_carbon[temp_carbon$year == 2014, "carbon_emissions"]
emission.min <- temp_carbon[temp_carbon$year == 1751, "carbon_emissions"]

emisssion.max / emission.min

temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>% 
  pull(year) %>% 
  min()

temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>% 
  pull(year) %>% 
  max()

temp_carbon[temp_carbon$year == 2018, "temp_anomaly"] - temp_carbon[temp_carbon$year == 1880, "temp_anomaly"]

p <- temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = temp_anomaly),
            color = "green") +
  geom_line(aes(y = ocean_anomaly),
            color = "blue") +
  geom_line(aes(y = land_anomaly),
            color = "brown")

p

p + geom_hline(aes(yintercept = 0),
               color = "red")

p + ylab("Temperature anomaly (degrees C)") + 
    ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
    geom_text(aes(x = 2000,
                  y = 0.05,
                  label = "20th century mean"),
              color = "blue") +
    geom_hline(aes(yintercept = 0), color = "red")

library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

greenhouse_gases %>% 
  ggplot(aes(x = year,
             y = concentration,
             color = gas)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(aes(xintercept = 1850), color = "red") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

temp_carbon %>% 
  ggplot(aes(x = year,
             y = carbon_emissions)) +
  geom_line() +
  geom_vline(aes(xintercept = 1960),
             color = "red") +
  geom_vline(aes(xintercept = 2014),
             color = "red") +
  scale_x_continuous(breaks = seq(1751, 2018, 10)) +
  geom_line(data = greenhouse_gases[greenhouse_gases$year>=1751,],
            aes(y = concentration,
                color = gas))

co2.time <- historic_co2 %>% 
  ggplot(aes(x = year,
             y = co2, 
             colour = source)) +
  geom_line()

co2.time + scale_x_continuous(limits = c(-3E3, 2018))
