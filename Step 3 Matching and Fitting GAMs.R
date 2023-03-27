## Matching

wp_match <- matchit(is_iced_kick ~ wp,
                    data = data_without_timeouts,
                    method = "nearest",
                    distance = "mahalanobis",
                    replace = FALSE,
                    ratio = 10)

MatchedData <- match.data(wp_match)

table(MatchedData$is_iced_kick)

## Fit GAMs

init_matched_logit <- mgcv::gam(success ~ s(kick_distance) +
                                  as.factor(is_iced_kick),
                                data = MatchedData, 
                                family = "binomial")

summary(init_matched_logit)

Figure_5 <- MatchedData %>%
  mutate(predProb = init_matched_logit$fitted.values) %>%
  ggplot(aes(x = kick_distance)) + 
  geom_line(aes(y = predProb, color = as.factor(is_iced_kick)),
            alpha = .7) +
  theme_bw() +
  scale_x_continuous(breaks = seq(from = 0, to = 80, by = 10))+
  theme(legend.position = "bottom") +
  ggthemes::scale_color_colorblind(labels = c("No","Yes")) +
  labs(color = "Iced Kick?",
       x = "Kick Distance",
       y = "Predicted Probability") 
