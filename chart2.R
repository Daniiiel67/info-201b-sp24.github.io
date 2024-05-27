# Load the packages
library(tidyverse)

# Load the data
dat <- read.csv("us-prison-jail-rates-1990.txt")

# Plot the trend 
jail_dat <- dat %>%
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
  pivot_longer(-Year, names_to = "Ethnicity", values_to = "Population") %>%
  mutate(Type = "Jail")

prison_dat <- dat %>%
  group_by(year) %>%
  summarise(aapi_prison_pop_rate = mean(aapi_prison_pop_rate, na.rm = TRUE),
            black_prison_pop_rate = mean(black_prison_pop_rate, na.rm = TRUE),
            latinx_prison_pop_rate = mean(latinx_prison_pop_rate, na.rm = TRUE),
            native_prison_pop_rate = mean(native_prison_pop_rate, na.rm = TRUE),
            white_prison_pop_rate = mean(white_prison_pop_rate, na.rm = TRUE)) %>%
  ungroup() %>%
  select(year, aapi_prison_pop_rate, black_prison_pop_rate, latinx_prison_pop_rate, native_prison_pop_rate, white_prison_pop_rate) %>%
  rename("Year" = "year", "Asian American / Pacific Islander" = "aapi_prison_pop_rate",
         "Black" = "black_prison_pop_rate", "Latin" = "latinx_prison_pop_rate",
         "Native" = "native_prison_pop_rate", "White" = "white_prison_pop_rate") %>%
  pivot_longer(-Year, names_to = "Ethnicity", values_to = "Population") %>%
  mutate(Type = "Prison")

dat <- rbind(jail_dat, prison_dat) %>%
  filter(Year == "1990")

ggplot(data = dat, 
       aes(x = Ethnicity, y = Population, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) + 
  theme_minimal() + 
  labs(title = "Average Jail/Prison Rate by Ethnicity(1990)", x = "Year", y = "Avg. Jail/Prison Rate Per 100,000 People")
