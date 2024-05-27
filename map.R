# Load the packages
library(tidyverse)
library(maps)

# Load the data
dat <- read.csv("us-prison-jail-rates-1990.txt")

# Exteract the 2018 data
dat1 <- dat %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarise(total_jail_pop_rate = mean(total_jail_pop_rate, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(data.frame(state = state.abb, state_full = tolower(state.name)))
  
dat2 <- left_join(map_data("state"), dat1, by = c("region" = "state_full"))

# Plot the map
ggplot(dat2) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop_rate),
    color = "white",
    size = .1        
  ) +
  coord_map() +
  scale_fill_continuous(low = "black", high = "Red") +
  labs(fill = "Rate of Jail Pop Rate") +
  labs(title = "The map of Jail Pop Rate, 2018", x = "", y= "") +
  theme_minimal()
  

