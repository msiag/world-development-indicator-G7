#libraries installed
install.packages("tidyverse")
install.packages("DataExplorer")
library(ggplot2)
library(tidyverse)

#Getting data
indicators = read.csv(file.choose())

# filter G7 country data
g7Country <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
g7Indicators <- subset(indicators, indicators$CountryCode %in% g7Country)

#Popluation trend during 1960 to 2010

population <- subset(g7Indicators, g7Indicators$IndicatorCode == "SP.POP.TOTL")

ggplot(data = population, aes(Year, (Value / 1000000))) +
  geom_line(aes(color = CountryCode)) +
  scale_y_continuous(breaks = seq(0, 400, 50)) +
  scale_x_continuous(breaks = seq(1960, 2014, 5)) +
  ggtitle("POPULATION TRENDS") +
  ylab("Population(Million)")


population_growth <- subset(g7Indicators, g7Indicators$IndicatorCode == "SP.POP.GROW")

ggplot(data = population_growth, aes(Year, Value)) +
  geom_line(aes(color = CountryCode)) +
  geom_smooth(stat = "summary", fun.y = mean, size = 2) +
  scale_x_continuous(breaks = seq(1960, 2014, 5)) +
  ggtitle("POPULATION GROWTH RATE") +
  ylab("Population growth rate(%)")

# As shown above in the graph the population growth has been deciline in population with time
#GDP per Capita

gdp_per_capita <- subset(g7Indicators, g7Indicators$IndicatorCode == "NY.GDP.PCAP.CD")

ggplot(data = gdp_per_capita, aes(Year, Value)) +
  geom_line(aes(color = CountryCode)) +
  geom_smooth(stat = "summary", fun.y = mean, linetype = 2, size = 2) +
  scale_x_continuous(breaks = seq(1960, 2014, 5)) +
  ggtitle("GDP PER CAPITA") +
  ylab("GDP per capita(USD)")

# GDP per capita growth

gdp_per_capita_growth <- subset(g7Indicators, g7Indicators$IndicatorCode == "NY.GDP.PCAP.KD.ZG")

ggplot(data = gdp_per_capita_growth, aes(Year, Value)) +
  geom_line(aes(color = CountryCode)) +
  geom_smooth(stat = "summary", fun.y = mean, linetype = 2, size = 2) +
  scale_x_continuous(breaks = seq(1960, 2014, 5)) +
  ggtitle("GDP PER CAPITA GROWTH") +
  ylab("GDP per capita annual growth rate(%)")

# From the above graph we can see there is strong co relation between the economic growth of these countries


#unemployment trendon basis of gender

unemployment <- subset(g7Indicators, IndicatorCode %in% c("SL.UEM.TOTL.ZS", "SL.UEM.TOTL.FE.ZS", "SL.UEM.TOTL.MA.ZS"))%>%
  droplevels(.) %>%
  select(CountryCode, IndicatorCode, Year, Value)
levels(unemployment$IndicatorCode) <- c("FEMALE", "MALE", "TOTAL")

ggplot(data = unemployment, aes(Year, Value)) +
  geom_line(aes(color = IndicatorCode), size = 1) +
  facet_wrap(~CountryCode, ncol = 2) +
  ylab("Unemployment(%)") +
  theme(panel.spacing  = unit(2, "lines"))
