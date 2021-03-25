library(tidyverse) 
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(ggExtra)
library(cowplot)
library(gtools) # combinations & permutations
library(knitr)
library(rmarkdown)
library(sf)
library(googleway)
library(rnaturalearth)
library(ggspatial)
library(rnaturalearthdata)
library(rgeos)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer) #sequential color palette
library(dslabs)               # datasets
library(nycflights13)         # datasets
library(NHANES)               # datasets
library(titanic)              # datasets


###############################################################################

labels_country <-
  data.frame(country = c("South Korea","Germany", "India", "China", "Pakistan"), 
                     x = c(1976,1965,1980, 2000, 1970), y = c(68,73,52,68, 52))

gapminder %>% 
  filter(country %in% c("South Korea","Germany","India","China","Pakistan")) %>% 
  ggplot(aes(year, life_expectancy, col = country)) + geom_line() + 
  geom_text(data = labels_country, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###############################################################################

gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)
past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day)) + geom_boxplot(aes(fill = continent)) +
  theme_economist() +theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Region") + ylab("Dollars per day") + scale_fill_discrete("Continents") +
  scale_y_continuous(trans = "log2") + geom_point()
view(gapminder)
###############################################################################

murder_rate <- murders %>% summarize(rate = sum(total)/sum(population) * 
                                       10^6) %>% pull(rate)

murders %>% 
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(aes(color = region), size = 3) + geom_text_repel(nudge_x = 0.075) + 
  theme_economist() + scale_x_log10() + scale_y_log10() + 
  xlab("Population per million (Logic Scale)") +
  ylab("Total no. of murders(Logic Scale)") + 
  geom_abline(intercept = log10(murder_rate), lty = 2, color = "darkgrey") +
  ggtitle("Murders in US in 2020") + scale_color_discrete(name = "Region")

## lty = 2 changes line type from solid to dashed
## xlab, ylab, ggtitle <- x axis , y axis, title naming
## scale_color_discrete <- Capitalize legends title for scatterplots

###############################################################################

data(mtcars)
str(mtcars)

mtcars %>% 
  ggplot(aes(hp, mpg)) + geom_point(size = 3, color = "red") + 
  xlab("Horse Power(Log Scale)") + ylab("Miles per Gallon(Log Scale)") +
  ggtitle("MTcars") + scale_x_log10() + scale_y_log10() +
  theme_economist() + geom_line()
 
 
###############################################################################

Males <- 
  heights %>% 
  filter(sex == "Male") %>% ggplot(aes(height,height)) + 
  geom_boxplot() + theme_economist() 

Males

Females <- 
  heights %>% 
      filter(sex == "Female") %>% ggplot(aes(height,height)) + 
      geom_boxplot() + theme_economist()


plot_grid(Males, Females, labels = c('Males', 'Females'), label_size = 12)
###############################################################################

heights %>% 
  group_by(sex) %>% ggplot(aes(height)) + 
  geom_histogram(binwidth = 1, color = "black") + 
  xlab("Height") + ylab("Count")

heights %>% 
  ggplot(aes(height)) + 
  geom_histogram(binwidth = 1, color = "white") + 
  xlab("Height") + ylab("Count")

###############################################################################
str(heights)

gender <- data.frame(sex = c("Male", "Female"), x = c(61,75), y = c(0.12, 0.08))
heights %>% 
  ggplot(aes(height, fill = sex)) + geom_density(alpha = 0.2) +
  theme(legend.position = "none") +
  geom_text(data = gender, aes(x,y, label = sex), size = 5) +
  xlab("Height") + ylab("Density") +
  ggtitle(label = "Height across genders", subtitle = "(Density graph)") +
  theme(plot.title = element_text(color = "Blue", size = 20, 
                                  face = "bold", hjust = 0.5)) +
  theme(plot.subtitle = element_text(color = "red", size = 15, hjust = 0.5)) +
  labs(caption = "By - Abhilash Roy") +
  theme(plot.caption = element_text(color = "black", size = 12)) 

heights %>% 
  ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.4) + 
  labs(x = "Sex", y = "Height", title = "Heights of Genders")

###############################################################################

str(gapminder)

gapminder %>% 
  filter(year %in% c(1960,1970)) %>% ggplot(aes(fertility, 
  life_expectancy, color = continent)) + geom_point() + facet_grid(~year) + 
  theme_economist() + xlab("Fertility") + ylab("Life Expectancy(in years)") + 
  scale_color_discrete("Continents")


gapminder %>% 
  filter(year %in% c(1960,1970,1980,1990,2000,2010)) %>% 
  ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point() + 
  facet_wrap(~year) + theme_economist() + xlab("Fertility") + 
  ylab("Life Expectancy(in years)") + 
  scale_color_discrete("Continents")

###############################################################################

str(iris)
data(iris)

iris %>%  
  ggplot(aes(Sepal.Length, Sepal.Width, color = Species, shape = Species)) + 
  geom_point() +theme_light() + geom_density2d() + ggtitle("IRIS")

###############################################################################

iris %>% 
  mutate(Species = "All") %>% rbind(iris) %>% 
  ggplot(aes(Petal.Length, Petal.Width, color = Species, shape = 
  Species)) + geom_point() + theme_bw() + facet_wrap(~Species, scales = 
  "free") + geom_smooth() + xlab("Petal Length") +ylab("Petal Width")

## rbind = bind_row
###############################################################################

summary(mtcars)
str(mtcars)     
mtcars %>% ggplot(aes(mpg, qsec, color = gear, size = disp)) + 
  geom_point() + xlab("Miles/(US) gallon") + ylab("1/4 mile time") + 
  theme_bw() + scale_size_continuous("Displacement")

###############################################################################

gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter(continent == "Africa", year %in% 
  c(1970,2010), !is.na(dollars_per_day)) %>% 
  ggplot(aes(dollars_per_day)) + geom_density() + facet_grid(~year) + 
  scale_x_continuous(trans = "log2")
  
###############################################################################

gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter(continent == "Africa", year %in% c(1970,2010), 
  !is.na(dollars_per_day)) %>% ggplot(aes(dollars_per_day, fill = region)) + 
  geom_density(bw= 0.5,position = "stack") + facet_grid(~year) + 
  scale_x_continuous(trans = "log2")
###############################################################################

library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)
str(greenhouse_gases)


greenhouse_gases %>%
  ggplot(aes(year,concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") + 
  geom_vline(aes(xintercept = 1850)) +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

str(greenhouse_gases)
greenhouse_gases %>%
  ggplot(aes(year,concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") + 
  geom_hline(aes(yintercept = 200)) +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, col = source)) + geom_line() +
  ggtitle("Atmospheric CO2 concentration, -800,000 BC to today") +
  ylab("co2 (ppmv)") + xlim(-800000, -775000) +
  geom_hline(aes(yintercept = 200)) +
  geom_hline(aes(yintercept = 275))
co2_time

co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, col = source)) + geom_line() +
  ggtitle("Atmospheric CO2 concentration, -800,000 BC to today") +
  ylab("co2 (ppmv)") + xlim(-3000, 2018) +
  geom_hline(aes(yintercept = 275)) +
  geom_hline(aes(yintercept = 400))
co2_time



temp_carbon %>% filter(!is.na(temp_anomaly),!is.na(land_anomaly),
                      !is.na(ocean_anomaly)) %>%
  ggplot(aes(year, carbon_emissions, color = temp_anomaly)) +geom_line() + 
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")




murders %>% 
  ggplot(aes(population/10^6, total, color = region, label = abb)) +
  geom_point() + xlab("State population per million (2010)") + 
  ylab("Number of gun murders in state (2010)") + 
  geom_text_repel(nudge_x = 0.075) +
  scale_x_log10() + scale_y_log10() +
  ggtitle("US gun murders by state for 2010", 
          subtitle = "Gun murder data from FBI reports.") +
  theme(plot.title = element_text(hjust = 0.5, color = "blue", size = 15)) +
  theme(plot.subtitle = element_text(hjust = 0.5, color = "purple")) +
  theme(legend.position = "none")




