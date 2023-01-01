library(caret)
library(xgboost)
library(pROC)
library(Ckmeans.1d.dp)
library(glue)
library(ggimage)
library(ggrepel)
library(ggthemes)
library(gt)
library(gtExtras)
options(scipen = 999)

defenders_prior_to_snap_model <- defenders_prior_to_snap %>%
  select(is_pass_rusher,
         absolute_y_dist_from_ball,
         distFromLOS,
         dir,
         crept_dis,
         primary_position) %>%
  mutate(primary_position = as.factor(primary_position))

#make empty df to store results in
names <- colnames(defenders_prior_to_snap_model)

names <- append(names, "xPassRush", 1)
names <- append(names, "PROE", 6)

df <- as.data.frame(matrix(ncol = 8, nrow = 1))
colnames(df) <- names

table(defenders_prior_to_snap_model$primary_position)
num_folds <- 8
fold_nums <- seq(1, num_folds, 1)
set.seed(123)
#xgboosting
for (position in c("CB", "EDGE", "IDL", "Off-Ball LB", "Safety")) {
  folds <-
    vfold_cv(
      defenders_prior_to_snap_model %>% filter(primary_position == position),
      v = num_folds,
      repeats = 1,
      strata = is_pass_rusher
    )
  
  for (i in fold_nums) {
    test_data <- folds$splits[[i]] %>% assessment() %>%
      select(is_pass_rusher, absolute_y_dist_from_ball, distFromLOS, dir, crept_dis)
    
    train_data <- folds$splits[[i]] %>% analysis() %>%
      select(is_pass_rusher, absolute_y_dist_from_ball, distFromLOS, dir, crept_dis)
    
    test_data <- as.matrix(test_data)
    train_data <- as.matrix(train_data)
    
    xpr <- xgboost::xgboost(
      data = train_data[, 2:5],
      label = train_data[, 1],
      nrounds = 500,
      objective = "binary:logistic",
      early_stopping_rounds = 3,
      max_depth = 4,
      eta = 0.1,
      gamma = 0,
      colsample_bytree = 1,
      print_every_n = 50,
      subsample = 1,
      min_child_weight = 1,
      monotone_constraints = c(0, -1, 0, 1) #decreasing on absolute_dis_from_los and increasing on crep_dis
    )
    
    # #predict values on the test data
    predict <- predict(xpr, test_data[, 2:5])
    
    # #join to test data
    model_test_results <- predict %>%
      as_tibble() %>%
      bind_cols(test_data)
    #
    # #rename
    model_test_results <- as.data.frame(model_test_results)
    #
    model_test_results <- model_test_results %>%
      rename(xPassRush = value) %>%
      mutate(PROE = is_pass_rusher - xPassRush, primary_position = position)
    
    #
    df <- rbind(model_test_results, df)
  }
}

#model results
xpr_results <- df %>%
  filter(!is.na(is_pass_rusher))

#variable importance plot
importance <- xgboost::xgb.importance(feature_names = colnames(xpr),
                                      model = xpr)

print(xgboost::xgb.ggplot.importance(importance_matrix = importance))

#results look good!
xpr_results %>%
  group_by(is_pass_rusher) %>%
  summarise(xPassRush = mean(xPassRush))

#builds roc for area under the curve
rocRFall = multiclass.roc(xpr_results$is_pass_rusher, xpr_results$xPassRush)
#produce area under the curve
aucRF = pROC::auc(rocRFall)
#0.988 area under the curve!
print(aucRF)

#join back to defenders_prior_to_snap to see probability by position
defenders_prior_to_snap_joined <-
  inner_join(
    defenders_prior_to_snap,
    xpr_results,
    by = c(
      "is_pass_rusher",
      "distFromLOS",
      "absolute_y_dist_from_ball",
      "dir",
      "crept_dis",
      "primary_position"
    )
  )

#probability by each pff position // tracks
defenders_prior_to_snap_joined %>%
  group_by(pff_positionLinedUp) %>%
  summarise(
    xPassRush = mean(xPassRush),
    isPassRusher = mean(is_pass_rusher),
    PROE = mean(PROE)
  ) %>%
  arrange(desc(xPassRush)) %>%
  View()

defenders_prior_to_snap_joined <- defenders_prior_to_snap_joined %>% 
  mutate(mse = (is_pass_rusher - xPassRush) ** 2)

#player level stats summed
xpr_player_stats <- defenders_prior_to_snap_joined %>%
  group_by(team, nflId, displayName, officialPosition) %>%
  mutate(mse = (is_pass_rusher - xPassRush) ** 2) %>%
  summarize(
    rushes = sum(is_pass_rusher),
    exp_rushes = sum(xPassRush),
    passrushed_oe_sum = sum(PROE),
    passrushed_oe_mean = mean(PROE),
    mse = mean(mse),
    plays = n()
  )

# team stats summed
xpr_team_stats <- defenders_prior_to_snap_joined %>%
  group_by(defensiveTeam, gameId, playId) %>%
  summarise(rushes = sum(is_pass_rusher),
            exp_rushes = sum(xPassRush),
            passrushed_oe_sum = sum(PROE),
            passrushed_oe_mean = mean(PROE),
            mse = mean(mse),
            pressure = max(pressure_caused),
            epa = mean(epa),
            pass_rush_pct = mean(is_pass_rusher),
            expect_passrush = mean(xPassRush)) %>% 
  group_by(defensiveTeam) %>%
  summarise(rushes = sum(rushes),
            exp_rushes = sum(exp_rushes),
            passrushed_oe_sum = sum(passrushed_oe_sum),
            passrushed_oe_mean = mean(passrushed_oe_mean) * 100,
            mse = mean(mse),
            pressure = mean(pressure) * 100,
            epa = mean(epa),
            pass_rush_pct = mean(pass_rush_pct),
            expect_passrush = mean(expect_passrush),
            n_plays = n())

# plot for model validation
# credit: https://gist.github.com/guga31bb/2e827358b9957c727282b843685e524f#cp-model-calibration-results
binning_summarized <- defenders_prior_to_snap_joined %>% 
  mutate(bin_pred_prob = round(xPassRush / 0.05) * .05,
         correct = ifelse(xPassRush >= .5 & is_pass_rusher == 1 | xPassRush <= .5 & is_pass_rusher == 0, 1, 0))

actual_prob <- binning_summarized %>% 
  group_by(bin_pred_prob) %>% 
  summarise(n_plays = n(),
            n_is_pass_rusher = length(which(is_pass_rusher == 1)),
            bin_actual_prob = n_is_pass_rusher / n_plays)

binning_summarized <- binning_summarized %>% 
  left_join(actual_prob, by = c("bin_pred_prob"))

binning_summarized %>% 
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(size = "Number of plays",
       x = "Expected Pass Rush",
       y = "Observed Pass Rush") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16, face = "bold", margin = margin(5, 0, 20, 0)),
        axis.text.y = element_text(size = 16, face = "bold", margin = margin(0, 5, 0, 20)),
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

# plot faceted by primary position
binning_summarized %>% 
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(size = "Number of plays",
       x = "Expected Pass Rush",
       y = "Observed Pass Rush",
       title = 'Pass Rush Probability Validation',
       subtitle = "By Position") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 9, face = "bold", margin = margin(5, 0, 20, 0)),
        axis.text.y = element_text(size = 9, face = "bold", margin = margin(0, 5, 0, 20)),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  facet_wrap(~ primary_position, nrow = 2)
