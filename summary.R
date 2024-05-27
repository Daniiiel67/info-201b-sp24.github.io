# Load the package
library(tidyverse)

# Load the data
dat <- read.csv("us-prison-jail-rates-1990.txt")

# Calculate the average of total jail rates for all counties
mean_rate <- mean(dat$total_jail_pop_rate, na.rm = TRUE)

# Calculate the maximum total jail rate for all counties
max_rate <- max(dat$total_jail_pop_rate, na.rm = TRUE)

# Calculate the minimum total jail rate for all counties
min_rate <- min(dat$total_jail_pop_rate, na.rm = TRUE)

# Calculate the change in the total population from 1990 to 2008
dat1 <- dat %>%
  group_by(year) %>%
  summarise(total_pop = mean(total_pop, na.rm = TRUE))

dat1_increase <- dat1$total_pop[dat1$year == 2018] - dat1$total_pop[dat1$year == 1990]

# Calculate the change in the total jail rate from 1990 to 2008
dat2 <- dat %>%
  group_by(year) %>%
  summarise(total_jail_pop_rate = mean(total_jail_pop_rate, na.rm = TRUE))

dat2_increase <- dat2$total_jail_pop_rate[dat2$year == 2018] - dat2$total_jail_pop_rate[dat2$year == 1990]

# Print the result
df <- data.frame(Name = c("Mean jail rates", "Maximum jail rates", "M inimum jail rates",
                          "Pop increase from 1990 to 2008", "Jail rate from 1990 to 2008"), 
                 Values = c(mean_rate, max_rate, min_rate, dat1_increase, dat2_increase))


