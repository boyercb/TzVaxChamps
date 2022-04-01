library(haven)
library(tidyverse)

tz <- read_stata("~/Downloads/HBS 2017-18 _Final_Individual_Data.dta")

df <- tz %>%
  filter(calc_age >= 18) %>%
  count(HHID, name = "adults") %>%
  count(adults, name = "count")

ggplot(df, aes(x = adults, y = count)) + 
  geom_col(color = "black", fill = "lightblue", width = 1) +
  scale_x_continuous(breaks = seq(1, 18)) +
  ylim(c(0, 5000)) +
  theme_classic() +
  labs(
    x = "Number of adults",
    y = "Count",
    title = "Distribution of number of adults (>18 years old) in each household",
    caption = "Source: Tanzania Household Budget Survey 2017-2018"
  ) +
  coord_cartesian(expand = F)
  
tz %>%
  filter(calc_age >= 18) %>%
  count(HHID, name = "adults") %>%
  summarise(
    mean_n_adults = mean(adults)
  )
