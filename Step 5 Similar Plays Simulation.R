# Create Similar Plays

## Filter down to just relevant plays 

##Set the added distance for the FG:
FG_DISTANCE <- 18

similar_plays <- Data %>%
  filter(!is.na(epa), play_type %in% c("qb_kneel", "run"),
         yardline_100 <= 30, (score_differential == -2 | 
                                score_differential == -1 |
                                score_differential == 0),
         qtr == 4, game_seconds_remaining <= 30,
         down <= 3,
         # Remove TD plays:
         touchdown == 0) %>%
  dplyr::select(play_id, game_id, desc, yardline_100,
                yards_gained) %>%
  # create the kick distance variable:
  mutate(pre_kick_distance = yardline_100 + FG_DISTANCE,
         new_kick_distance = yardline_100 - yards_gained + FG_DISTANCE)

# View distribution of yards gained and the kick distance 

similar_plays %>%
  ggplot(aes(x = yards_gained)) +
  geom_histogram(binwidth = 1) +
  theme_bw()

summary(similar_plays$yards_gained)

## Create two versions of the data 
JAG_YARDLINE <- 18
FG_DISTANCE <- 18

post_play_kick_distance <- similar_plays %>%
  mutate(kick_distance = pmax(JAG_YARDLINE + FG_DISTANCE - yards_gained,
                              FG_DISTANCE),
         is_iced_kick = 0)

## Fit the matched data model 

matched_data_model <- gam(success ~ s(kick_distance) + as.factor(is_iced_kick),
                          data = MatchedData, family = "binomial")

# Compute the differences in predicted probabilities 

post_play_pred_probs <- as.numeric(predict(matched_data_model,
                                           newdata = post_play_kick_distance,
                                           type = "response"))

jag_kick_prob <- as.numeric(predict(matched_data_model,
                                    newdata = tibble(kick_distance = 36,
                                                     is_iced_kick = 1),
                                    type = "response"))

result_tbl <- tibble(init_prob = jag_kick_prob,
                     new_prob = post_play_pred_probs) %>%
  mutate(prob_delta = new_prob - init_prob)

# View the summary output - use this to determine the histogram breaks!
summary(result_tbl$prob_delta)

# What's the proportion of plays with where the initial probability is higher?
length(which(result_tbl$prob_delta < 0)) / nrow(result_tbl)

# And now plot the histogram:
Figure_8 <-result_tbl %>% 
  ggplot(aes(x = prob_delta)) + 
  geom_histogram(breaks = seq(-0.06, 0.15, by = 0.01), closed = "left",
                 color = "black", fill = "blue", alpha = 0.75) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(x = "Change in FG success probability (simulated - observed)", 
       y = "Count") +
  theme_bw() +
  theme(axis.title=element_text(size=14))
