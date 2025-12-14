####### Setup ##############

library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidymodels)
library(xgboost)
library(ranger)
library(kknn)

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
  step_normalize(all_numeric_predictors()) |>
  step_zv(all_predictors())

