##### Step one: Calculate metric averages by pitcher and pitch type
# Minimum 5% frequency for each pitch type
pitcher_ptypes <- 
  statcast %>%
  group_by(pitcher, game_year, pitch_category) %>%
  summarise(N = n()) %>% 
  left_join(statcast %>% group_by(pitcher, game_year) %>% summarise(TP = n()), by = c("pitcher", "game_year")) %>%
  mutate(Pct = N / TP) %>%
  filter(Pct >= .05)


##### Step two: Calculate unpredictability by measuring deviation between a pitcher's mix overall versus based on the count
# Calculate frequency of each count in MLB
count_freq <-
  statcast %>%
  filter(!is.na(count)) %>%
  group_by(count) %>%
  summarise(
    count_freq = n() / nrow(statcast)
  )

# Each pitcher's overall pitch mix regardless of count
pitcher_mix_overall <- 
  statcast %>%
  group_by(pitcher, game_year) %>%
  summarise(
    ff = sum(case_when(pitch_type == 'FF' ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    si = sum(case_when(pitch_type %in% c('FT','SI') ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    ct = sum(case_when(pitch_type == 'FC' ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    sl = sum(case_when(pitch_type == 'SL' ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    cu = sum(case_when(pitch_type %in% c('CU','KC','SC') ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    ch = sum(case_when(pitch_type == 'CH' ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    sp = sum(case_when(pitch_type %in% c('FS','FO') ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    kn = sum(case_when(pitch_type == 'KN' ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    ot = sum(case_when(pitch_type %in% c('EP','CS','','PO') ~ 1, TRUE ~ 0), na.rm = TRUE) / n()
  ) %>% melt(id = c('pitcher', 'game_year'), value.name = 'ovr_pct', variable.name = 'pitch_type') %>%
  arrange(pitcher)

# Each pitcher's pitch mix by count - calculate difference between count mix and overall mix
pitcher_mix_count <-
  statcast %>%
  group_by(pitcher, game_year, count) %>%
  summarise(
    ff = sum(case_when(pitch_type == 'FF' ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    si = sum(case_when(pitch_type %in% c('FT','SI') ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    ct = sum(case_when(pitch_type == 'FC' ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    sl = sum(case_when(pitch_type == 'SL' ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    cu = sum(case_when(pitch_type %in% c('CU','KC','SC') ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    ch = sum(case_when(pitch_type == 'CH' ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    sp = sum(case_when(pitch_type %in% c('FS','FO') ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    kn = sum(case_when(pitch_type == 'KN' ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    ot = sum(case_when(pitch_type %in% c('EP','CS','','PO') ~ 1, TRUE ~ 0), na.rm = TRUE) / n()
  ) %>% melt(id = c('pitcher','game_year','count'), value.name = 'count_pct', variable.name = 'pitch_type') %>%
  left_join(pitcher_mix_overall, by = c('pitcher','game_year','pitch_type')) %>%
  left_join(count_freq, by = 'count') %>%
  mutate(
    diff = abs(count_pct - ovr_pct)
  )

# Weight the differences by the overall frequencies of each count and take the sum
pitcher_mix <-
  pitcher_mix_count %>%
  group_by(pitcher, game_year) %>%
  summarise(
    mix = sum(count_freq * diff)
  )

# Case studies: Trevor Bauer (predictable) vs. Aaron Nola (unpredictable)
bauer_mix <- 
  pitcher_mix_count %>%
  filter(pitcher == 545333)

nola_mix <-
  pitcher_mix_count %>%
  filter(pitcher == 605400)

#################################################################
######### Step three: Measure release point consistency
# Calculate the Euclidean distance between release point and the release point of the previous pitch in the AB
# since it doesn't really matter what his release point was 50 games ago, it just matters in the context of an AB
pitcher_rel_points <-
  statcast %>%
  filter(!pitch_type %in% c('','EP','PO','CS')) %>%
  arrange(pitcher, game_year, game_date, inning, inning_topbot, outs_when_up, pitch_number) %>%
  group_by(pitcher, game_year, game_date, at_bat_number) %>%
  mutate(prev_rel_pos_x = lag(release_pos_x),
         prev_rel_pos_z = lag(release_pos_z),
         euclidean_dist = sqrt( (release_pos_x - prev_rel_pos_x)^2 + (release_pos_z - prev_rel_pos_z)^2 )) %>% #+ (release_extension - prev_rel_ext)^2 )) %>%
  dplyr::select(player_name, pitcher, game_year, game_date, inning, inning_topbot, outs_when_up, pitch_number, 
                at_bat_number, pitch_type, batter, release_pos_x, prev_rel_pos_x, release_pos_z, prev_rel_pos_z, euclidean_dist, index) %>%
  filter(euclidean_dist < 0.75)


# Plot distrbution of euclidean distances
# There are instances of bad data, and outlier pitchers who intentionally change arm slots
euclid_mean = mean(pitcher_rel_points$euclidean_dist, na.rm = TRUE)
euclid_sd = sd(pitcher_rel_points$euclidean_dist, na.rm = TRUE)

ggplot(pitcher_rel_points, aes(x=euclidean_dist)) + 
  geom_density() + 
  geom_vline(aes(xintercept = euclid_mean), color = "green") +
  geom_text(mapping = aes(x = euclid_mean, y = 0, label = "mean", hjust = -0.25, vjust = -1)) +
  geom_vline(aes(xintercept = euclid_mean + euclid_sd), color = "red") +
  geom_text(mapping = aes(x = euclid_mean + euclid_sd, y = 0, label = "1 SD", hjust = -0.25, vjust = -1)) +
  geom_vline(aes(xintercept = euclid_mean - euclid_sd), color = "red") +
  geom_text(mapping = aes(x = euclid_mean - euclid_sd, y = 0, label = "1 SD", hjust = -0.25, vjust = -1)) +
  geom_vline(aes(xintercept = euclid_mean + 2 * euclid_sd), color = "orange") +
  geom_text(mapping = aes(x = euclid_mean + 2 * euclid_sd, y = 0, label = "2 SD", hjust = -0.25, vjust = -1)) +
  geom_vline(aes(xintercept = euclid_mean + 3 * euclid_sd), color = "yellow") +
  geom_text(mapping = aes(x = euclid_mean + 3 * euclid_sd, y = 0, label = "3 SD", hjust = -0.25, vjust = -1)) +
  xlim(0,1) + 
  ggtitle("Euclidean Distance Distribution") +
  theme_bw()


# Calculate each pitcher-season's new mean euclidean distance
pitcher_rel_consistency <- 
  pitcher_rel_points %>%
  group_by(player_name, pitcher, game_year) %>%
  summarise(
    N = n(),
    consistency = mean(euclidean_dist, na.rm = TRUE)
  )


#################################################################
######### Step four: Find expected pitch movement based on release point
# K-nearest neighbors regression to get average of results based on most similar release points

# Calculate weighted average of Euclidean distance per pitcher
pitcher_num_pitches <- 
  statcast %>%
  filter(!pitch_type %in% c('','EP','PO','CS')) %>%
  group_by(pitcher, game_year) %>%
  summarise(
    tot_pitches = n()
  )

# Use pitchers' averages rather than one row per data to avoid nearest neighbor being the same pitcher
# Must have thrown at least 100 total pitches, and at least 25 pitches of the specific pitch type
pitcher_rel_move <- 
  statcast %>%
  filter(!pitch_type %in% c('','EP','PO','CS')) %>%
  dplyr::select(pitcher, game_year, pitch_type, p_throws, release_pos_x, release_pos_z, pfx_x, pfx_z) %>%
  mutate(
    release_pos_x = ifelse(p_throws == 'L', release_pos_x * -1, release_pos_x),
    pfx_x = ifelse(p_throws == 'L', pfx_x * -1, pfx_x)
  ) %>%
  na.omit() %>%
  group_by(pitcher, game_year, pitch_type) %>%
  summarise(
    pitches = n(),
    release_pos_x = mean(release_pos_x, na.rm = TRUE),
    release_pos_z = mean(release_pos_z, na.rm = TRUE),
    pfx_x = mean(pfx_x, na.rm = TRUE),
    pfx_z = mean(pfx_z, na.rm = TRUE)
  ) %>%
  left_join(pitcher_num_pitches, by = c('pitcher','game_year')) %>%
  filter(tot_pitches >= 100 & pitches >= 25)


############# Data exploration
# Correlations between release point and pitch movement by pitch type
by(data = pitcher_rel_move, INDICES = pitcher_rel_move$pitch_type, 
   FUN = function(x) {cor(x$release_pos_x, x$pfx_x)})

by(data = pitcher_rel_move, INDICES = pitcher_rel_move$pitch_type, 
   FUN = function(x) {cor(x$release_pos_z, x$pfx_z)})

# Horizontal release point vs. horizontal movement faceted by pitch type
ggplot(pitcher_rel_move, aes(x = release_pos_x, y = pfx_x)) +
  geom_point(size = .5, alpha = .2) +
  facet_wrap(~pitch_type) +
  ggtitle("Horz Rel Point vs. Horz Movement by Pitch Type") +
  theme_bw()

# Vertical release point vs. vertical movement faceted by pitch type
ggplot(pitcher_rel_move, aes(x = release_pos_z, y = pfx_z)) +
  geom_point(size = .5, alpha = .2) +
  facet_wrap(~pitch_type) +
  ggtitle("Vert Rel Point vs. Vert Movement by Pitch Type") +
  theme_bw()

# Clear that we need a separate model for each pitch type  

###### Build KNN regression models
# Split the data into training and testing sets
set.seed(42)
knn_sample <- sample.split(pitcher_rel_move$pitch_type, SplitRatio = .7)
knn_train <- subset(pitcher_rel_move, knn_sample == TRUE)
knn_test <- subset(pitcher_rel_move, knn_sample == FALSE)


# Choose best value for k
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

knn_pred_x <- function(k = 1, training, testing) {
  pred = FNN::knn.reg(train = training %>% ungroup %>% dplyr::select(release_pos_x, release_pos_z), 
                      test = testing %>% ungroup %>% dplyr::select(release_pos_x, release_pos_z), 
                      y = training$pfx_x, k = k)$pred
  act  = testing$pfx_x
  rmse(predicted = pred, actual = act)
}

# Possible values of k
k <- c(1, 5, 10, 15, 20, 25, 30, 50, 100, 250, 500)

knn_train_rmse <- sapply(k, knn_pred_x, 
                         training = knn_train, 
                         testing = knn_train)

knn_test_rmse <- sapply(k, knn_pred_x, 
                        training = knn_train, 
                        testing = knn_test)

# Find optimal value of k
opt_k <- k[which.min(knn_test_rmse)]

# find overfitting, underfitting, and "best"" k
fit_status <- ifelse(k < opt_k, "Over", ifelse(k == opt_k, "Best", "Under"))

knn_results <- data.frame(
  k,
  round(knn_train_rmse, 2),
  round(knn_test_rmse, 2),
  fit_status
)
colnames(knn_results) <- c("k", "Train RMSE", "Test RMSE", "Fit?")

# Normalize variables in pitcher_rel_move for knn regression
max_release_pos_x <- max(pitcher_rel_move$release_pos_x)
min_release_pos_x <- min(pitcher_rel_move$release_pos_x)
max_release_pos_z <- max(pitcher_rel_move$release_pos_z)
min_release_pos_z <- min(pitcher_rel_move$release_pos_z)
max_pfx_x <- max(pitcher_rel_move$pfx_x)
min_pfx_x <- min(pitcher_rel_move$pfx_x)
max_pfx_z <- max(pitcher_rel_move$pfx_z)
min_pfx_z <- min(pitcher_rel_move$pfx_z)

pitcher_rel_move_norm <- 
  pitcher_rel_move %>%
  mutate(release_pos_x = (release_pos_x - min_release_pos_x) / (max_release_pos_x - min_release_pos_x),
         release_pos_z = (release_pos_z - min_release_pos_z) / (max_release_pos_z - min_release_pos_z),
         pfx_x = (pfx_x - min_pfx_x) / (max_pfx_x - min_pfx_x),
         pfx_z = (pfx_z - min_pfx_z) / (max_pfx_z - min_pfx_z))


# Four-seam fastball models
ff_model_x <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FF') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FF') %>% pull(pfx_x), 
          k = 10)

ff_model_z <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FF') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FF') %>% pull(pfx_z), 
          k = 10)

ff_model_results <- pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FF') %>% dplyr::select(pitcher, game_year, pitch_type)
ff_model_results$pred_x <- ff_model_x$pred
ff_model_results$pred_z <- ff_model_z$pred

# Cutter models
fc_model_x <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FC') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FC') %>% pull(pfx_x), 
          k = 10)

fc_model_z <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FC') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FC') %>% pull(pfx_z), 
          k = 10)

fc_model_results <- pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FC') %>% dplyr::select(pitcher, game_year, pitch_type)
fc_model_results$pred_x <- fc_model_x$pred
fc_model_results$pred_z <- fc_model_z$pred


# Two-seam models
ft_model_x <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FT') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FT') %>% pull(pfx_x), 
          k = 10)

ft_model_z <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FT') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FT') %>% pull(pfx_z), 
          k = 10)

ft_model_results <- pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FT') %>% dplyr::select(pitcher, game_year, pitch_type)
ft_model_results$pred_x <- ft_model_x$pred
ft_model_results$pred_z <- ft_model_z$pred

# Sinker models
si_model_x <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'SI') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'SI') %>% pull(pfx_x), 
          k = 10)

si_model_z <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'SI') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'SI') %>% pull(pfx_z), 
          k = 10)

si_model_results <- pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'SI') %>% dplyr::select(pitcher, game_year, pitch_type)
si_model_results$pred_x <- si_model_x$pred
si_model_results$pred_z <- si_model_z$pred

# Slider models
sl_model_x <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'SL') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'SL') %>% pull(pfx_x), 
          k = 10)

sl_model_z <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'SL') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'SL') %>% pull(pfx_z), 
          k = 10)

sl_model_results <- pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'SL') %>% dplyr::select(pitcher, game_year, pitch_type)
sl_model_results$pred_x <- sl_model_x$pred
sl_model_results$pred_z <- sl_model_z$pred

# Curveball and knuckle-curve models
cu_model_x <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'CU' | pitch_type == 'KC') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'CU' | pitch_type == 'KC') %>% pull(pfx_x), 
          k = 10)

cu_model_z <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'CU' | pitch_type == 'KC') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'CU' | pitch_type == 'KC') %>% pull(pfx_z), 
          k = 10)

cu_model_results <- pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'CU' | pitch_type == 'KC') %>% dplyr::select(pitcher, game_year, pitch_type)
cu_model_results$pred_x <- cu_model_x$pred
cu_model_results$pred_z <- cu_model_z$pred

# Changeup models
ch_model_x <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'CH') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'CH') %>% pull(pfx_x), 
          k = 10)

ch_model_z <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'CH') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'CH') %>% pull(pfx_z), 
          k = 10)

ch_model_results <- pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'CH') %>% dplyr::select(pitcher, game_year, pitch_type)
ch_model_results$pred_x <- ch_model_x$pred
ch_model_results$pred_z <- ch_model_z$pred

# Splitter and forkball models
fs_model_x <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FS' | pitch_type == 'FO') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FS' | pitch_type == 'FO') %>% pull(pfx_x), 
          k = 10)

fs_model_z <- 
  knn.reg(train = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FS' | pitch_type == 'FO') %>% dplyr::select(release_pos_x, release_pos_z), 
          y = pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FS' | pitch_type == 'FO') %>% pull(pfx_z), 
          k = 10)

fs_model_results <- pitcher_rel_move_norm %>% ungroup() %>% filter(pitch_type == 'FS' | pitch_type == 'FO') %>% dplyr::select(pitcher, game_year, pitch_type)
fs_model_results$pred_x <- fs_model_x$pred
fs_model_results$pred_z <- fs_model_z$pred


#### Make predictions on Statcast data
knn_predictions <- bind_rows(
  ff_model_results,
  fc_model_results,
  ft_model_results,
  si_model_results,
  sl_model_results,
  cu_model_results,
  ch_model_results,
  fs_model_results
)

# Add predictions to original dataframe
pitcher_rel_move_norm <-
  left_join(pitcher_rel_move_norm, knn_predictions, by = c('pitcher','game_year','pitch_type'))

pitcher_rel_move_norm$euclidean_dist <- 
  sqrt( (pitcher_rel_move_norm$pfx_x - pitcher_rel_move_norm$pred_x)^2 + (pitcher_rel_move_norm$pfx_z - pitcher_rel_move_norm$pred_z)^2 )


pitcher_unexpected <- 
  pitcher_rel_move_norm %>%
  group_by(pitcher, game_year) %>%
  summarise(
    unexpected = sum(euclidean_dist * (pitches / tot_pitches), na.rm = TRUE)
  )



#################################################################
######### Step five: Combine the three metrics and take a weighted average to calculate Deception
# Combine into one dataframe, filter by > 100 pitches thrown, and apply scaler
pitcher_deception <- 
  pitcher_unexpected %>%
  left_join(pitcher_rel_consistency, by = c('pitcher','player_name','game_year')) %>%
  left_join(pitcher_mix, by = c('pitcher','game_year')) %>%
  filter(N > 100) %>%
  na.omit()

# Min-max scaling
max_consistency <- max(pitcher_deception$consistency)
min_consistency <- min(pitcher_deception$consistency)
max_unexpected <- max(pitcher_deception$unexpected)
min_unexpected <- min(pitcher_deception$unexpected)
max_mix <- max(pitcher_deception$mix)
min_mix <- min(pitcher_deception$mix)

pitcher_deception <-
  pitcher_deception %>%
  mutate(
    consistency_scaled = -1 * (((consistency - min_consistency) / (max_consistency - min_consistency)) - 1),
    unexpected_scaled = (unexpected - min_unexpected) / (max_unexpected - min_unexpected),
    mix_scaled = -1 * (((mix - min_mix) / (max_mix - min_mix)) - 1)
  ) %>%
  inner_join(statcast %>% dplyr::select(pitcher, game_year, player_name) %>% unique(), by = c('pitcher','game_year'))

# Calculate each pitcher's CSW (called-strike plus whiff) %
pitcher_csw <-
  statcast %>%
  group_by(pitcher, game_year) %>%
  summarise(
    csw = sum(case_when(description %in% c('swinging_strike','called_strike','swinging_strike_blocked') ~ 1, TRUE ~ 0), na.rm = TRUE) / n(),
    woba = mean(woba_value, na.rm = TRUE),
    xwoba = mean(estimated_woba_using_speedangle, na.rm = TRUE)
  ) %>%
  inner_join(pitcher_deception %>% dplyr::select(pitcher, game_year, consistency_scaled, unexpected_scaled, mix_scaled), by = c('pitcher','game_year'))

# Linear regression between each of the three metrics and CSW to decide on weights
cor(pitcher_csw$csw, pitcher_csw$consistency_scaled)
cor(pitcher_csw$csw, pitcher_csw$unexpected_scaled)
cor(pitcher_csw$csw, pitcher_csw$mix_scaled)

cor(pitcher_csw$woba, pitcher_csw$consistency_scaled)
cor(pitcher_csw$woba, pitcher_csw$unexpected_scaled)
cor(pitcher_csw$woba, pitcher_csw$mix_scaled)

cor(pitcher_csw$xwoba, pitcher_csw$consistency_scaled)
cor(pitcher_csw$xwoba, pitcher_csw$unexpected_scaled)
cor(pitcher_csw$xwoba, pitcher_csw$mix_scaled)

ggplot(pitcher_csw, aes(x = consistency_scaled, y = csw)) +
  geom_point()

ggplot(pitcher_csw, aes(x = unexpected_scaled, y = csw)) +
  geom_point()

ggplot(pitcher_csw, aes(x = mix_scaled, y = csw)) +
  geom_point()

csw_lm <- lm(data = pitcher_csw,
             formula = csw ~ consistency_scaled + unexpected_scaled + mix_scaled)

# Take abs value of t-values and use them for weights
abs(summary(csw_lm)[["coefficients"]][, "t value"][2:4])

# Calculate deception using weighted sum of three metrics
pitcher_deception <-
  pitcher_deception %>%
  mutate(
    deception = 2.191464 * consistency_scaled + 6.876388 * unexpected_scaled + 5.665022 * mix_scaled
  )

max_deception <- max(pitcher_deception$deception)
min_deception <- min(pitcher_deception$deception)

pitcher_deception <-
  pitcher_deception %>%
  mutate(deception_scaled = ((deception - min_deception) / (max_deception - min_deception)) * 100)

pitcher_csw <- 
  pitcher_csw %>%
  inner_join(pitcher_deception, by = c("pitcher","game_year"))

cor(pitcher_csw$csw, pitcher_csw$deception)

# Create deception leaderboard and write to CSV
deception_leaderboard <-
  pitcher_deception %>%
  dplyr::select(player_name, pitcher, game_year, N, mix_scaled, consistency_scaled, unexpected_scaled, deception_scaled) %>%
  rename('unpredictability' = mix_scaled, 
         'indistinguishability' = consistency_scaled, 
         'unexpectedness' = unexpected_scaled,
         'deception' = deception_scaled) %>%
  mutate(unpredictability = unpredictability * 100,
         indistinguishability = indistinguishability * 100,
         unexpectedness = unexpectedness * 100) %>%
  arrange(-deception)

write.csv(deception_leaderboard, 
          "C:\\Users\\jakes\\OneDrive\\Desktop\\Jobs\\Rangers Analytics\\deception_leaderboard.csv",
          row.names = FALSE)


# Plot deception distribution
ggplot(deception_leaderboard %>% mutate(pitcher_season = paste(game_year, pitcher)),
       aes(x = deception)) +
  geom_density()


############# Step six: Validate by calculating YoY correlations
# Pivot deception leaderboard to make years as columns
deception_pivot <-
  deception_leaderboard %>%
  dplyr::select(player_name, pitcher, game_year, deception) %>%
  pivot_wider(names_from = game_year, values_from = deception)

# Correlation between 2019 and 2020
cor(deception_pivot[,3], deception_pivot[,4], use="complete.obs")

ggplot(deception_pivot, aes(x = deception_pivot$`2019`, y = deception_pivot$`2020`)) +
  geom_point(aes(color = deception_pivot$`2020` - deception_pivot$`2019`)) +
  scale_color_gradient2(low = "blue", mid = "gray", high = "red") +
  xlab("2019 Deception") + ylab("2020 Deception") +
  ggtitle("2019 to 2020 Deception Correlation",
          subtitle = paste0("Correlation = ", 
                            round(cor(deception_pivot[,3], deception_pivot[,4], use="complete.obs"), 3))) +
  theme_bw() +
  theme(legend.position = "none")

# Correlation between 2018 and 2019
cor(deception_pivot[,4], deception_pivot[,5], use="complete.obs")

ggplot(deception_pivot, aes(x = deception_pivot$`2018`, y = deception_pivot$`2019`)) +
  geom_point(aes(color = deception_pivot$`2019` - deception_pivot$`2018`)) +
  scale_color_gradient2(low = "blue", mid = "gray", high = "red") +
  xlab("2018 Deception") + ylab("2019 Deception") +
  ggtitle("2018 to 2019 Deception Correlation",
          subtitle = paste0("Correlation = ", 
                            round(cor(deception_pivot[,4], deception_pivot[,5], use="complete.obs"), 3))) +
  theme_bw() +
  theme(legend.position = "none")