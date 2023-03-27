## Exploratory Data Analysis

table(data_without_timeouts$is_iced_kick)

year <- as.list(data_without_timeouts$game_id) %>%
  str_extract("[:digit:]{4}")
year <- data.frame(year)
year$year <- as.numeric(year$year)
data_without_timeouts <- cbind(data_without_timeouts, year)


Figure_1 <- data_without_timeouts %>%
  group_by(year, is_iced_kick) %>%
  dplyr::summarize(count = n(),
                   is_iced_kick,
                   year,
                   .groups = "drop") %>%
  ungroup() %>%
  group_by(year) %>%
  dplyr::summarize(total = n(),
                   count,
                   is_iced_kick,
                   year, 
                   .groups = "drop") %>%
  ungroup() %>%
  dplyr::mutate(prop = count / total) %>%
  filter(is_iced_kick == 1) %>%
  ggplot(aes(x = as.factor(year))) +
  geom_bar(aes(y = prop),
           stat = "identity", position = "dodge", ) +
  theme_bw() + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust = 1, angle = 45)) +
  labs(x = "Year", y = "Proportion", fill = "Is Iced Kick") +
  ggthemes::scale_color_colorblind()


Figure_2 <- data_without_timeouts %>%
  filter(field_goal_attempt == 1, !is.na(prev_def_team_timeout)) %>%
  group_by(prev_def_team_timeout, field_goal_result) %>%
  summarize(count = n(), field_goal_result, prev_def_team_timeout,
            .groups = "drop") %>%
  ungroup() %>%
  group_by(prev_def_team_timeout) %>%
  summarize(total = n(), field_goal_result, prev_def_team_timeout, count, 
            .groups = "drop") %>%
  mutate(prop = count / total) %>%
  ggplot(aes(x = as.factor(prev_def_team_timeout), 
             fill = field_goal_result)) +
  geom_bar(aes(y = prop),
           stat = "identity", position = "dodge") +
  ggthemes::scale_fill_colorblind() +
  labs(x = "", y = "Proportion of Field Goals",
       fill = "Field Goal Result") +
  scale_x_discrete(labels = c("0" = "Non-Iced", "1" = "Iced")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal")

Figure_3 <- data_without_timeouts %>%
  ggplot(aes(x = kick_distance)) +
  geom_density(aes(color = as.factor(is_iced_kick))) +
  labs(x = "Kick Distance", y = "Density") +
  theme_bw() +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind(labels = c("No","Yes")) +
  labs(colour = "Iced Kick?")


Figure_4 <-data_without_timeouts %>%
  ggplot(aes(x = wp)) +
  geom_density(aes(color = as.factor(is_iced_kick))) +
  labs(x = "Win Probability", y = "Density") +
  theme_bw() +
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind(labels = c("No","Yes")) +
  labs(colour = "Iced Kick?") 
