##########################################################################
### COVID-19 ###
## COVID-19 Analysis: a simple approach with R ##

## Author: NIRMAL SAI SWAROOP JANAPANEEDI

# Probably one of the greatest contributions during this time for COVID-19 understanding and analysis has been made by Rami Krispin. This R package is up to date with the most recent information regarding this novel coronavirus.
# It contains the Province/State, the Country/Region, latitude and longitude, date and status (comfirmed/death/recovered).

library(coronavirus)

# Now,I want to inspect how the data looks;

df = coronavirus
summary(df)

unique(df$Country.Region)

library(tidyr)
library(dplyr)
library(janitor)

# Argentina:

argentina = filter(df, (Country.Region == 'Argentina' & cases > 0))

argentina = argentina %>%
  clean_names() %>%
  mutate(date = as.Date(date, "%d/%m/%Y"))


confirmed = argentina %>% filter(type == 'confirmed') %>%
  select(date, cases, type)

recovered = argentina %>% filter(type == 'recovered') %>%
  select(date, cases, type)

death = argentina %>% filter(type == 'death') %>%
  select(date, cases, type)


confirmed[, 2] <- cumsum(confirmed[, 2])
recovered[, 2] <- cumsum(recovered[, 2])
death[, 2] <- cumsum(death[, 2])

library(reshape2)
mergeCols = c('date', 'cases', 'type')

full = merge(confirmed, recovered,  by = mergeCols, all = TRUE)
fill = merge(full, death,  by = mergeCols, all = TRUE)

library(plotly)
library(scales)
library(lubridate)

fill %>%
  group_by(type) %>%
  plot_ly(x = ~ date) %>%
  layout(title = 'Argentina COVID-19 analysis') %>%
  add_lines(y = ~ cases,
            color = ~ factor(type)
  )

# Brazil:

brasil = filter(df, (Country.Region == 'Brazil' & cases > 0))

brasil = brasil %>%
  clean_names() %>%
  mutate(date = as.Date(date, "%d/%m/%Y"))


confirmed_brasil = brasil %>% filter(type == 'confirmed') %>%
  select(date, cases, type)

recovered_brasil = brasil %>% filter(type == 'recovered') %>%
  select(date, cases, type)

death_brasil = brasil %>% filter(type == 'death') %>%
  select(date, cases, type)


confirmed_brasil[, 2] <- cumsum(confirmed_brasil[, 2])
recovered_brasil[, 2] <- cumsum(recovered_brasil[, 2])
death_brasil[, 2] <- cumsum(death_brasil[, 2])

full_brasil = merge(confirmed_brasil, recovered_brasil,  by = mergeCols, all = TRUE)
fill_brasil = merge(full_brasil, death_brasil,  by = mergeCols, all = TRUE)

fill_brasil %>%
  group_by(type) %>%
  plot_ly(x = ~ date) %>%
  layout(title = 'Brazil COVID-19 analysis') %>%
  add_lines(y = ~ cases,
            color = ~ factor(type)
  )

# Finally, let's explore one of the most affected countries: Italy

italy = filter(df, (Country.Region == 'Italy' & cases > 0))

italy = italy %>%
  clean_names() %>%
  mutate(date = as.Date(date, "%d/%m/%Y"))


confirmed_italy = italy %>% filter(type == 'confirmed') %>%
  select(date, cases, type)

recovered_italy = italy %>% filter(type == 'recovered') %>%
  select(date, cases, type)

death_italy = italy %>% filter(type == 'death') %>%
  select(date, cases, type)


confirmed_italy[, 2] <- cumsum(confirmed_italy[, 2])
recovered_italy[, 2] <- cumsum(recovered_italy[, 2])
death_italy[, 2] <- cumsum(death_italy[, 2])

full_italy = merge(confirmed_italy, recovered_italy,  by = mergeCols, all = TRUE)
fill_italy = merge(full_italy, death_italy,  by = mergeCols, all = TRUE)

fill_italy %>%
  group_by(type) %>%
  plot_ly(x = ~ date) %>%
  layout(title = 'Italy COVID-19 analysis') %>%
  add_lines(y = ~ cases,
            color = ~ factor(type)
  )

# South Korea:

sk = filter(df, (Country.Region == 'Korea, South' & cases > 0))

sk = sk %>%
  clean_names() %>%
  mutate(date = as.Date(date, "%d/%m/%Y"))


confirmed_sk = sk %>% filter(type == 'confirmed') %>%
  select(date, cases, type)

recovered_sk = sk %>% filter(type == 'recovered') %>%
  select(date, cases, type)

death_sk = sk %>% filter(type == 'death') %>%
  select(date, cases, type)


confirmed_sk[, 2] <- cumsum(confirmed_sk[, 2])
recovered_sk[, 2] <- cumsum(recovered_sk[, 2])
death_sk[, 2] <- cumsum(death_sk[, 2])

full_sk = merge(confirmed_sk, recovered_sk,  by = mergeCols, all = TRUE)
fill_sk = merge(full_sk, death_sk,  by = mergeCols, all = TRUE)

fill_sk %>%
  group_by(type) %>%
  plot_ly(x = ~ date) %>%
  layout(title = 'South Korea COVID-19 analysis') %>%
  add_lines(y = ~ cases,
            color = ~ factor(type)
  )

# Spain:

spain = filter(df, (Country.Region == 'Spain' & cases > 0))

spain = spain %>%
  clean_names() %>%
  mutate(date = as.Date(date, "%d/%m/%Y"))


confirmed_spain = spain %>% filter(type == 'confirmed') %>%
  select(date, cases, type)

recovered_spain = spain %>% filter(type == 'recovered') %>%
  select(date, cases, type)

death_spain = spain %>% filter(type == 'death') %>%
  select(date, cases, type)


confirmed_spain[, 2] <- cumsum(confirmed_spain[, 2])
recovered_spain[, 2] <- cumsum(recovered_spain[, 2])
death_spain[, 2] <- cumsum(death_spain[, 2])

full_spain = merge(confirmed_spain, recovered_spain,  by = mergeCols, all = TRUE)
fill_spain = merge(full_spain, death_spain,  by = mergeCols, all = TRUE)

fill_spain %>%
  group_by(type) %>%
  plot_ly(x = ~ date) %>%
  layout(title = 'Spain COVID-19 analysis') %>%
  add_lines(y = ~ cases,
            color = ~ factor(type)
  )

# Israel:

israel = filter(df, (Country.Region == 'Israel' & cases > 0))

israel = israel %>%
  clean_names() %>%
  mutate(date = as.Date(date, "%d/%m/%Y"))


confirmed_israel = israel %>% filter(type == 'confirmed') %>%
  select(date, cases, type)

recovered_israel = israel %>% filter(type == 'recovered') %>%
  select(date, cases, type)

death_israel = israel %>% filter(type == 'death') %>%
  select(date, cases, type)


confirmed_israel[, 2] <- cumsum(confirmed_israel[, 2])
recovered_israel[, 2] <- cumsum(recovered_israel[, 2])
death_israel[, 2] <- cumsum(death_israel[, 2])

full_israel = merge(confirmed_israel, recovered_israel,  by = mergeCols, all = TRUE)
fill_israel = merge(full_israel, death_israel,  by = mergeCols, all = TRUE)

fill_israel %>%
  group_by(type) %>%
  plot_ly(x = ~ date) %>%
  layout(title = 'Israel COVID-19 analysis') %>%
  add_lines(y = ~ cases,
            color = ~ factor(type)
  )
## Warning in RColorBrewer::brewer.pal(N, "Set2"): minimal value for n is 3, returning requested palette with 3 different levels

## Warning in RColorBrewer::brewer.pal(N, "Set2"): minimal value for n is 3, returning requested palette with 3 different levels

# germany:

germany = filter(df, (Country.Region == 'Germany' & cases > 0))

germany = germany %>%
  clean_names() %>%
  mutate(date = as.Date(date, "%d/%m/%Y"))


confirmed_germany = germany %>% filter(type == 'confirmed') %>%
  select(date, cases, type)

recovered_germany = germany %>% filter(type == 'recovered') %>%
  select(date, cases, type)

death_germany = germany %>% filter(type == 'death') %>%
  select(date, cases, type)


confirmed_germany[, 2] <- cumsum(confirmed_germany[, 2])
recovered_germany[, 2] <- cumsum(recovered_germany[, 2])
death_germany[, 2] <- cumsum(death_germany[, 2])

full_germany = merge(confirmed_germany, recovered_germany,  by = mergeCols, all = TRUE)
fill_germany = merge(full_germany, death_germany,  by = mergeCols, all = TRUE)

fill_germany %>%
  group_by(type) %>%
  plot_ly(x = ~ date) %>%
  layout(title = 'Germany COVID-19 analysis') %>%
  add_lines(y = ~ cases,
            color = ~ factor(type)
  )
# Diamond princess:

diamond = filter(df, (Country.Region == 'Cruise Ship' & cases > 0))

diamond = diamond %>%
  clean_names() %>%
  mutate(date = as.Date(date, "%d/%m/%Y"))


confirmed_diamond = diamond %>% filter(type == 'confirmed') %>%
  select(date, cases, type)

recovered_diamond = diamond %>% filter(type == 'recovered') %>%
  select(date, cases, type)

death_diamond = diamond %>% filter(type == 'death') %>%
  select(date, cases, type)


confirmed_diamond[, 2] <- cumsum(confirmed_diamond[, 2])
recovered_diamond[, 2] <- cumsum(recovered_diamond[, 2])
death_diamond[, 2] <- cumsum(death_diamond[, 2])

full_diamond = merge(confirmed_diamond, recovered_diamond,  by = mergeCols, all = TRUE)
fill_diamond = merge(full_diamond, death_diamond,  by = mergeCols, all = TRUE)

fill_diamond %>%
  group_by(type) %>%
  plot_ly(x = ~ date) %>%
  layout(title = 'Diamond Princess COVID-19 analysis') %>%
  add_lines(y = ~ cases,
            color = ~ factor(type)
  )

# Singapore:

singapore = filter(df, (Country.Region == 'Singapore' & cases > 0))

singapore = singapore %>%
  clean_names() %>%
  mutate(date = as.Date(date, "%d/%m/%Y"))


confirmed_singapore = singapore %>% filter(type == 'confirmed') %>%
  select(date, cases, type)

recovered_singapore = singapore %>% filter(type == 'recovered') %>%
  select(date, cases, type)

death_singapore = singapore %>% filter(type == 'death') %>%
  select(date, cases, type)


confirmed_singapore[, 2] <- cumsum(confirmed_singapore[, 2])
recovered_singapore[, 2] <- cumsum(recovered_singapore[, 2])
death_singapore[, 2] <- cumsum(death_singapore[, 2])

full_singapore = merge(confirmed_singapore, recovered_singapore,  by = mergeCols, all = TRUE)
fill_singapore = merge(full_singapore, death_singapore,  by = mergeCols, all = TRUE)

fill_singapore %>%
  group_by(type) %>%
  plot_ly(x = ~ date) %>%
  layout(title = 'Singapore COVID-19 analysis') %>%
  add_lines(y = ~ cases,
            color = ~ factor(type)
  )
## Warning in RColorBrewer::brewer.pal(N, "Set2"): minimal value for n is 3, returning requested palette with 3 different levels

## Warning in RColorBrewer::brewer.pal(N, "Set2"): minimal value for n is 3, returning requested palette with 3 different levels


