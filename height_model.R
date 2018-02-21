library(tidyverse)

firvecs <- read_csv("~/thesis/firingvectors.csv")

max_heights <- firvecs %>%
  group_by(diameter) %>%
  summarise(height = max(z)) %>%
  mutate(d2=diameter^2)

max_heights %>%
  ggplot(aes(x=diameter, y=height)) +
  geom_point() +
  stat_smooth(aes(y = height),method = "lm", formula = y ~ x + I(x^2), size = 1)

evens <- max_heights %>%
  filter((diameter %% 2) == 0)

odds <- max_heights %>%
  filter((diameter %% 2) != 0)

evens %>%
  ggplot(aes(x=diameter, y=height)) +
  geom_point() +
  stat_smooth(aes(y = height),method = "lm", formula = y ~ x + I(x^2), size = 1)

odds %>%
  ggplot(aes(x=diameter, y=height)) +
  geom_point() +
  stat_smooth(aes(y = height),method = "lm", formula = y ~ x + I(x^2), size = 1)

m_all <- lm(height ~ diameter + d2, data = max_heights)
m_evens <- lm(height ~ diameter + d2, data = evens)
m_odds <- lm(height ~ diameter + d2, data = odds)

summary(m_all)$adj.r.squared
summary(m_evens)$adj.r.squared
summary(m_odds)$adj.r.squared


maxes <- firvecs %>%
  group_by(diameter) %>%
  summarise(x_max = max(x), y_max = max(y)) %>%
  mutate(real_diameter = pmax(x_max, y_max) * 2) %>%
  mutate(diff = diameter - real_diameter)

edges <- firvecs %>%
  group_by(diameter) %>%
  filter(x == max(x) | y == max(y)) %>%
  filter((diameter %% 2) != 0) %>%
  summarise(s = mean(z)) %>%
  mutate(d2 = diameter^2)

lm(s ~ diameter + d2, data=edges)

edges %>%
  ggplot(aes(x=diameter, y=s)) +
  geom_point() +
  stat_smooth(aes(y = st),method = "lm", formula = y ~ x + I(x^2), size = 1) + 
  

