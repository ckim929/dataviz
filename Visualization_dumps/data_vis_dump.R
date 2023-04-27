install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

data <- read_csv("database.csv")

ggplot(data, aes(x = Region, fill=Region)) +
  geom_bar(position = "dodge") +
  labs(x = "Region",
       y = "Number of Executions",
       title = "Number of executions in the United States since 1976") +
  scale_fill_manual(values = c("#6F6ECB",
                               "#338FFF",
                               "#F1D076",
                               "#6ECBAF"))

#ggsave("vis1.png")

count <- data %>% count(Method)

ggplot(count, aes(x = "", y = n, fill = Method)) +
  geom_col(color = "black") +
  geom_text(aes(label = n),
            position = position_stack(vjust = 0.5),
            size = 5) +
  coord_polar(theta = "y") +
  labs(x = "", y = "", title = "Method of execution used in the United States since 1976",
       caption = "The number of represents the number of executions that has taken place by specified method cumulative since 1976") +
  scale_fill_manual(values = c("#BE2A3E",
                               "#EC754A",
                               "#EACF65",
                               "#3C8D53",
                               "#6E95CB")) +
  theme_minimal() +
  theme(panel.grid = element_blank())

ggsave("vis2.png")

