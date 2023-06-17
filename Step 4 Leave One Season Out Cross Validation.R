## Leave-One-Season-Out Cross-Validation 

holdout_pred_comparison <- 
  # generate holdout predictions for every row based season
  map_dfr(unique(MatchedData$season), 
          function(S) {
            # Separate the training data (all kicks, not just iced!)
            train_data <- MatchedData %>%
              filter(season != S)
            # Train the model with the kick distance and is_iced_kick indicator:
            train_model <- gam(success ~ s(kick_distance) 
                               + as.factor(is_iced_kick),
                               data = train_data, family = "binomial")
            
            # Get the test data, but only for iced kicks 
            test_data_iced <- MatchedData %>%
              filter(season == S, is_iced_kick == 1)
            
            # Now make a copy of this test data where we change the is_iced_kick
            # variable to be 0s, imagining as if we changed the coaches decision:
            test_data_change_iced <- test_data_iced %>%
              mutate(is_iced_kick = 0) 
            
            # Return tibble of holdout results for both versions of the 
            # test data, where iced_kick == 1, and the same kicks where we change
            # the is_iced_kick to be 0:
            tibble(test_pred_iced = predict(train_model, 
                                            newdata = test_data_iced,
                                            type = "response"),
                   test_pred_change_iced = predict(train_model, 
                                                   newdata = test_data_change_iced,
                                                   type = "response"),
                   # we only need the outcome once since they are the same kicks!
                   test_actual = test_data_iced$success, 
                   test_season = S,
                   test_kickdistance = test_data_iced$kick_distance)
            
          }
  )

holdout_pred_comparison$PredProbIcedDiff <-
  as.numeric(holdout_pred_comparison$test_pred_iced) -
  as.numeric(holdout_pred_comparison$test_pred_change_iced)

Difference <- as.data.frame(holdout_pred_comparison)

summary(Difference$PredProbIcedDiff)

Figure_7 <- Difference %>%
  ggplot(aes(y=PredProbIcedDiff, x=as.factor(test_season))) + 
  geom_boxplot(fill = "grey") +
  theme_bw() + 
  theme(axis.title.x = element_text(vjust = +2),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(hjust = 1, angle = 45)) +
  labs(y = "Difference in Predicted Probability",
       x = "Season")

## Decrease in Probability

Iced_vs_NonIced_36 <- 
  map_dfr(unique(MatchedData$season), 
          function(S) {
            
   #Train model on the entire MatchedData
   train_model <- gam(success ~ s(kick_distance) 
                               + as.factor(is_iced_kick),
                               data = MatchedData, family = "binomial")
            
   #Create predicted probability where input is of the form        
   #data.frame(is_iced_kick = c(0, 0), kick_distance = c(36, 38))
   tibble(predicted_probability = predict(train_model, 
                                          newdata = data.frame(is_iced_kick = c(0, 1),
                                                               kick_distance = c(36, 36)),
                                          type = "response"))
            
                     }
        )

Decrease_in_Probability_2 = Iced_vs_NonIced_36[1,]-Iced_vs_NonIced_36[2,]

Non_Iced_36_vs_38 <- 
  map_dfr(unique(MatchedData$season), 
          function(S) {
            
   #Train model on the entire MatchedData
   train_model <- gam(success ~ s(kick_distance) 
                               + as.factor(is_iced_kick),
                               data = MatchedData, family = "binomial")
            
   #Create predicted probability where input is of the form          			       
   #data.frame(is_iced_kick = c(0, 0), kick_distance = c(36, 38))
   tibble(predicted_probability = predict(train_model, 
                                          newdata = data.frame(is_iced_kick = c(0, 0),
                                                               kick_distance = c(36, 38)),
                                          type = "response"))
            
                    }
         )

Decrease_in_Probability_3 = Non_Iced_36_vs_38[1,] - Non_Iced_36_vs_38[2,]
