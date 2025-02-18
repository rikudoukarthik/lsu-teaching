# explaining mean, SD and SE with a sports example

library(tidyverse)

set.seed(123)

player1 <- data.frame(
  home = c(9:6, 3, 10:7, 2),
  away = c(2:6, 1:5)
)

player2 <- data.frame(
  home = c(10:7, 10:7, 9, 10),
  away = c(1:4, 1:4, 1, 2)
)

data_full <- bind_rows(player1, player2, .id = "player") %>% 
  pivot_longer(cols = c("home", "away"), names_to = "venue", values_to = "points") 

data_means <- data_full %>% 
  group_by(player, venue) %>% 
  reframe(mean = mean(points),
          sd = paste("sd = ", round(sd(points), 3))) %>% 
  mutate(mean_label = paste("mean = ", mean))

ggplot(data_full) +
  geom_point(aes(x = venue, y = points, col = venue), position = position_jitter()) +
  facet_wrap(~ player) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  theme_classic() +
  geom_point(data = data_means, mapping = aes(x = venue, y = mean, fill = venue),
             size = 5, shape = 21, stroke = 1.5) +
  geom_text(data = data_means, mapping = aes(x = venue, y = mean + 1.25, label = mean_label),
            size = 4, fontface = "bold")+
  geom_text(data = data_means, mapping = aes(x = venue, y = mean + 0.75, label = sd),
            size = 4, fontface = "bold")
