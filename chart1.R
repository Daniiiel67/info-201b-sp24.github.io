# Load the packages
library(tidyverse)

# Load the data
dat <- read.csv("us-prison-jail-rates-1990.txt")

# Plot the trend 
plot_dat <- dat %>%
  group_by(year) %>%
  summarise(aapi_jail_pop_rate = mean(aapi_jail_pop_rate, na.rm = TRUE),
            black_jail_pop_rate = mean(black_jail_pop_rate, na.rm = TRUE),
            latinx_jail_pop_rate = mean(latinx_jail_pop_rate, na.rm = TRUE),
            native_jail_pop_rate = mean(native_jail_pop_rate, na.rm = TRUE),
            white_jail_pop_rate = mean(white_jail_pop_rate, na.rm = TRUE)) %>%
  ungroup() %>%
  select(year, aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate, native_jail_pop_rate, white_jail_pop_rate) %>%
  rename("Year" = "year", "Asian American / Pacific Islander" = "aapi_jail_pop_rate",
         "Black" = "black_jail_pop_rate", "Latin" = "latinx_jail_pop_rate",
         "Native" = "native_jail_pop_rate", "White" = "white_jail_pop_rate") %>%
  pivot_longer(-Year, names_to = "Ethnicity", values_to = "Jail_Population")


ggplot(data = plot_dat, 
       aes(x = Year, y = Jail_Population, color = Ethnicity, group = Ethnicity)) +
  geom_line() + 
  theme_minimal() + 
  labs(title = "Average Jail Rate by Ethnicity", x = "Year", y = "Avg. Jail Rate Per 100,000 People")
