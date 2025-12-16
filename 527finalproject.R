####### Setup ##############

library(tidyverse)
library(corrplot)
library(tidymodels)

set.seed(123)

# Import dataset
#dir <- "~/Documents/STAT 527/archive (3)/"
dir <- "C:/Users/thong/Downloads/archive/"
files <- c("constructor_standings",
           "constructors",
           "drivers",
           "lap_times",
           "pit_stops",
           "qualifying",
           "races",
           "results")
for (f in files) {
  file_path <- file.path(paste0(dir, f, ".csv"))
  temp_df <- read.csv(file_path)
  assign(paste0(f,"_ori"), temp_df)
}

####### Preprocessing ##############

# Selecting and filtering relevant information
races <- races_ori |> 
  filter(year >= 2020) |> 
  select(raceId, year, round)
constructor_standings <- constructor_standings_ori |> 
  select(raceId, constructorId, position) |> 
  rename(cs_position = position)
constructors <- constructors_ori |> 
  select(constructorId, constructorRef) |>
  rename(constructor = constructorRef)
drivers <- drivers_ori |> 
  select(driverId, code) |>
  rename(driver = code)
lap_times <- lap_times_ori |> 
  group_by(raceId, driverId) |>
  summarise(lap_time = mean(milliseconds), .groups = "drop") |> # we use mean lap time instead of sum to account for DNFs
  select(raceId, driverId, lap_time)
pit_stops <- pit_stops_ori |> 
  group_by(raceId, driverId) |>
  summarise(stop_time = mean(milliseconds), .groups = "drop") |>
  select(raceId, driverId, stop_time)
qualifying <- qualifying_ori |> 
  select(raceId, driverId, constructorId, position) |> 
  rename(qual_position = position)
results <- results_ori |> 
  select(raceId, driverId, constructorId, grid, positionOrder) |>
  rename(final_grid = grid, final_position = positionOrder)

# Merge data
merged_df <- constructors |> 
  inner_join(results, by="constructorId") |> 
  inner_join(races, by="raceId") |>
  left_join(drivers, by = "driverId") |>
  left_join(lap_times, by = c("raceId", "driverId")) |>
  left_join(pit_stops, by = c("raceId", "driverId")) |>
  left_join(constructor_standings, by=c("constructorId", "raceId")) |>
  left_join(qualifying, by=c("constructorId", "driverId", "raceId"))

# Calculate average of predictors (since each team has two drivers) and reorder columns
avg_df <- merged_df |> 
  group_by(constructorId, constructor, raceId, year, round) |> 
  summarise(avg_qual_position = mean(qual_position, na.rm=TRUE), 
            avg_final_grid = mean(final_grid, na.rm=TRUE),
            avg_final_position = mean(final_position, na.rm=TRUE),
            avg_lap_time = mean(lap_time, na.rm=TRUE),
            avg_stop_time = mean(stop_time, na.rm=TRUE),
            cs_position = first(cs_position), # cs_position is unique per constructor, just take the first one instead of averaging
            .groups = "drop") |>
  arrange(raceId, constructorId) |>
  select(raceId, year, round, constructorId, constructor, avg_qual_position, avg_lap_time, avg_stop_time, avg_final_grid, avg_final_position, cs_position)

# Final cleaning
df <- avg_df |> 
  drop_na() |> # drop all rows with NA (at this point any NA would mean double DNF, or Belgian 2021)
  group_by(constructorId) |> filter(n_distinct(year) >= 2) |> # remove teams with less than 2 years history
  ungroup()

####### Visualizations ##############
f1_colors <- c(
  "mercedes"      = "#00D2BE",
  "ferrari"       = "#DC0000",
  "red_bull"      = "#0600EF",
  "mclaren"       = "#FF8700",
  "alpine"        = "#0090FF",
  "aston_martin"  = "#006F62",
  "haas"          = "#787878",
  "alphatauri"    = "#2B4562",
  "alfa"          = "#900000",
  "williams"      = "#005AFF"
)

### Line plots

# Team's avg finishing position over time 
plot_df <- df %>%
  group_by(year, constructor) %>%
  summarise(mean_finish = mean(avg_final_position, na.rm = TRUE), .groups = "drop")

ggplot(plot_df, aes(x = year, y = mean_finish, color = constructor, group = constructor)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_reverse() +   # because 1st place is better
  scale_color_manual(values = f1_colors) + 
  labs(
    title = "Constructor Performance Over Time (Average Final Position)",
    x = "Year",
    y = "Average Final Position",
    color = "Constructor"
  ) +
  theme_minimal(base_size = 14)

# Team's championship rankings over the years
df %>%
  group_by(year, constructor) %>%
  summarise(mean_cs = mean(cs_position, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_cs, color = constructor, group = constructor)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_reverse() +  # Lower ranks (1st place) at the top
  scale_color_manual(values = f1_colors) + 
  labs(title = "Constructor Championship Rankings Over Years",
       x = "Year",
       y = "Average Championship Position",
       color = "Constructor") +
  theme_minimal(base_size = 14)



#Team's qualifying performance over time
plot_df <- df %>%
  group_by(year, constructor) %>%
  summarise(mean_qual = mean(avg_qual_position, na.rm = TRUE), .groups = "drop")

ggplot(plot_df, aes(x = year, y = mean_qual, color = constructor)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_reverse() +
  scale_color_manual(values = f1_colors) + 
  labs(
    title = "Constructor Qualifying Performance Over Time",
    x = "Year",
    y = "Average Qualifying Position",
    color = "Constructor"
  ) +
  theme_minimal(base_size = 14)



### Bar plot of mean lap time by team
mean_lap_df <- df %>%
  group_by(constructor) %>%
  summarise(mean_lap_time = mean(avg_lap_time, na.rm = TRUE)) %>%
  arrange(mean_lap_time)  # fastest team first

ggplot(mean_lap_df, aes(x = reorder(constructor, mean_lap_time), y = mean_lap_time, fill=constructor)) +
  geom_col() +
  labs(
    title = "Mean Average Lap Time per Constructor",
    x = "Constructor",
    y = "Mean Lap Time (ms)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  coord_flip()


### heat map of team championship ranking by year
df_heat <- df %>%
  group_by(constructor, year) %>%
  summarise(mean_cs = mean(cs_position, na.rm = TRUE), .groups = "drop")

ggplot(df_heat, aes(x = factor(year), y = constructor, fill = mean_cs)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(direction = -1) +  # Darker = better rank
  labs(title = "Heatmap of Constructor Championship Rankings",
       x = "Year",
       y = "Constructor",
       fill = "CS Position") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### scatterplot of lap time vs. qualifying time across teams
ggplot(df, aes(x = avg_qual_position, y = avg_lap_time, color = constructor)) +
  geom_point() +
  scale_color_manual(values = f1_colors) + 
  facet_wrap(~ constructor, scales = "free") +
  labs(title = "Qualifying vs Lap Time Across Teams",
       x = "Average Qualifying Position",
       y = "Average Lap Time") +
  theme_minimal() +
  theme(legend.position = "none")


### heat map of correlation matrix
cor_data <- df %>%
  ungroup() %>%
  select(avg_qual_position, avg_lap_time, avg_stop_time, avg_final_grid, avg_final_position)

corrplot(cor(cor_data),
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Correlation Matrix of Predictors",
         mar = c(0,0,2,0))


####### Modeling and Evaluation ##############

# Train-test split
split <- initial_split(df, prop = 0.8, strata = cs_position)
train_data <- training(split)
test_data  <- testing(split)
cv_folds <- vfold_cv(train_data, v = 5, strata = cs_position)

# Recipe (preprocessing)
base_rec <- recipe(cs_position ~ ., data = train_data) |>
  # ID Variables: Not used for prediction
  update_role(raceId, year, round, constructorId, constructor, new_role = "ID") |> 
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())



library(kknn)
##### KNN code 

# recipe
f1_rec <- recipe(cs_position ~ ., data = train_data) |>
  update_role(raceId, year, round, constructorId, constructor, new_role = "ID") |>
  step_impute_median(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors())

# prep and bake
f1_prep <- prep(f1_rec, train_data)
f1_baked <- bake(f1_prep, train_data)
summary(f1_baked)

# KNN model (regression) - treating cs_position as numeric here since it's ranked (we then compare with the ordinal regression model)
knn_model <- nearest_neighbor(
  neighbors = tune()
) |>
  set_engine("kknn") |>
  set_mode("regression")

# fit
knn_wf <- workflow() |>
  add_recipe(f1_rec) |>
  add_model(knn_model)

# predict
predict(knn_fit, f1_baked) |>
  mutate(truth = f1_baked$cs_position) |>
  head()

# evaluate on test data
f1_test_baked <- bake(f1_prep, test_data)

f1_preds <- predict(knn_fit, f1_test_baked) |>
  mutate(truth = f1_test_baked$cs_position)

metrics(f1_preds,
        truth = truth,
        estimate = .pred)


# validate KNN with Cross-Validation
set.seed(123)
cv_folds <- vfold_cv(df, v = 5)

knn_res <- tune_grid(
  knn_wf,
  resamples = cv_folds,
  grid = tibble(neighbors = seq(3, 25, by = 2)),
  metrics = metric_set(rmse, mae)
)

collect_metrics(knn_res)

### Final model with best k
best_k <- select_best(knn_res, metric="rmse")
best_k

final_knn <- finalize_workflow(knn_wf, best_k) |>
  fit(train_data)

# Evaluate on test dataset
knn_preds <- predict(final_knn, test_data) |>
  bind_cols(test_data)

# RMSE of 1.61 which means on average, the KNN model predicts a team's 
#rank within 1.61 of the true value
metrics(knn_preds,
        truth = cs_position,
        estimate = .pred)







##### Random Forest 

library(randomForest)

# recipe for random forest
rf_rec <- recipe(cs_position ~ ., data = train_data) |>
  update_role(raceId, constructorId, constructor, new_role = "ID") |>
  step_impute_median(all_numeric_predictors())


# model specification
rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) |>
  set_engine("randomForest") |>
  set_mode("regression")

# workflow
rf_wf <- workflow() |>
  add_recipe(rf_rec) |>
  add_model(rf_spec)


# cross validation
set.seed(123)
cv_folds <- vfold_cv(train_data, v = 5, strata = cs_position)

rf_grid <- grid_regular(
  mtry(range = c(1, 3)),   # we have 3 predictors
  min_n(range = c(2, 10)),
  levels = 5
)

rf_res <- tune_grid(
  rf_wf,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = metric_set(rmse, mae, rsq)
)

collect_metrics(rf_res)

# select best hyperparameters
best_rf <- select_best(rf_res, metric = "rmse")
best_rf

# fit final random forest model 
final_rf <- finalize_workflow(rf_wf, best_rf) |>
  fit(train_data)

# predict on test set
rf_preds <- predict(final_rf, test_data) |>
  bind_cols(test_data)

#Performance - RMSE of 1.59, MAE of 1.27
metrics(rf_preds, truth = cs_position, estimate = .pred)



