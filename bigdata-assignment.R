## HEADER ####
## Florence Galliers
## Big Data Assignment C7084
## 2021-03-16

## CONTENTS ####
# 1.0 Set Up
# 2.0 Data Imports
# 3.0 EDA
# 4.0 Modelling

## 1.0 SET UP ####
# Install packages
system('java -version')
library(sparklyr)
packageVersion('sparklyr')
library(dplyr)
library(geospark)
library(ggplot2)
library(corrr)
library(dbplot)
library(maps)
library(viridis)

# Set up spark connection
sc <- spark_connect(master = "local", version = "2.3")

## 2.0 DATA IMPORTS ####
# Load in Data set for Analysis (final_data.csv)
# Load raw GBIF dataset (Tree Sparrow) (passer-montanus.csv)
# Load world map data for mapping observation locations

data <- spark_read_csv(sc, "final_data.csv")
pm_data <- spark_read_csv(sc, "passer-montanus.csv")
world <- map_data('world')

## 3.0 EDA ####

# Look at the observation data locations

pm_plot <- pm_data %>%
  select(decimalLongitude, decimalLatitude, year) %>%
  filter(year >= 2001) %>%
  collect() %>% 
  ggplot() + 
  geom_polygon(data = world, 
                        aes(x = long, 
                            y = lat, 
                            group = group),
                        fill = "grey95",
                        color = "grey20") + 
  coord_fixed(ratio = 1.2,
              xlim = c(-10,3), 
              ylim = c(50.3, 59)) +
  theme_minimal() +
  geom_point(pm_data,
             mapping = aes(x = decimalLongitude,
                 y = decimalLatitude),
             color = "seagreen4",
             alpha = 0.5) +
  labs(title = "",
       x = "",
       y = "") +
  theme(
    plot.title = element_text(family = "Avenir",
                              size = 12,
                              hjust = 0.5),
    axis.text = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
  )

ggsave("pm_plot.png", plot = pm_plot, width = 20, height = 20, units = "cm")

# Look at frequency over time of observation data
pm_data$year <- as.factor(pm_data$year)

time_plot <- pm_data %>%
  group_by(year) %>%
  filter(year >= 2001) %>%
  summarise(sum = n()) %>%
  collect() %>%
  ggplot(aes(x = as.character(year), y = sum)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Tree Sparrow Observations per year",
       x = "Year",
       y = "Total no of observations") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Avenir",
                              size = 12,
                              hjust = 0.5),
    axis.title = element_text(family = "Avenir",
                              size = 12),
    axis.text = element_text(family = "Avenir",
                             size = 8),
    axis.text.x = element_text(angle = 90, hjust = 1, 
                               size = 10, vjust = 0.5, 
                               margin=margin(-10,0,0,0)),
    panel.grid = element_blank())
time_plot

ggsave("time_plot.png", time_plot, width = 20, height = 10, unit = "cm")

# Look at observations per month of the year
months <- pm_data %>%
  select(year, month) %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = as.character(month), y = count)) +
  geom_bar(stat = "identity")

# look at average values of all data
summarise_all(data, mean) 

# Selected needed columns
data2 <- data %>%
  select(Count, tas, tasmin, tasmax, rainfall, hurs, land, year)

# Look at correlation between variables
ml_corr(data2)

correlate(data2, use = "pairwise.complete.obs", method = "pearson") %>%
  shave() %>%
  rplot()

# Look at distributions of temperature variables
temp_plot <- data2 %>%
  select(tas, tasmin, tasmax) %>%
  pivot_longer(names_to = "type", 
               values_to = "temperature",
               cols = 1:3) %>%
  ggplot(aes(x = type, y = temperature)) +
  geom_boxplot() +
  geom_point(position = "jitter", alpha = 0.3)

## 4.0 MODELLING ####

## 4.1 Feature Engineering ####
# Look at the data in the table
sdf_describe(data2, cols = c("Count", "tas", "tasmin", "tasmax", "rainfall", "hurs", "year", "land"))

# Plot count data histogram
count_plot <- data2 %>%
  select(Count) %>%
  ggplot(aes(x = Count)) +
  geom_histogram(bins = 300)

# A large number of the count data is for '1' observation.

# Scale the variables to have a mean of 0 as they are all in different units
scaled_values <- data2 %>%
  summarise(
    # tas
    mean_tas = mean(tas),
    sd_tas = sd(tas),
    # tasmin
    mean_tasmin = mean(tasmin),
    sd_tasmin = sd(tasmin),
    # tasmax
    mean_tasmax = mean(tasmax),
    sd_tasmax = sd(tasmax),
    # rainfall
    mean_rainfall = mean(rainfall),
    sd_rainfall = sd(rainfall),
    # hurs
    mean_hurs = mean(hurs),
    sd_hurs = sd(hurs)
  ) %>%
  collect()

data3 <- data2 %>%
  mutate(scaled_tas = (tas - !!scaled_values$mean_tas) / !!scaled_values$sd_tas) %>%
  mutate(scaled_tasmin = (tasmin - !!scaled_values$mean_tasmin) / !!scaled_values$sd_tasmin) %>%
  mutate(scaled_tasmax = (tasmax - !!scaled_values$mean_tasmax) / !!scaled_values$sd_tasmax) %>%
  mutate(scaled_rainfall = (rainfall - !!scaled_values$mean_rainfall) / !!scaled_values$sd_rainfall) %>%
  mutate(scaled_hurs = (hurs - !!scaled_values$mean_hurs) / !!scaled_values$sd_hurs)
  
# One hot encode the land variables as it is categorical with each number representing one land type
data4 <- ft_one_hot_encoder(data3, input_cols = 'land', output_cols = 'type')
data4[14]
glimpse(data4[14])

# Create new variable to convert the count data into binary categorical data of 'one' observation or more than 
# one observation ('many')
data5 <-data4 %>% 
  mutate(pmcount = case_when(Count == 1 ~ "one",
                             Count >=2 ~ "many"))

analysis_data <- data5 %>%
  select(pmcount, year, type, scaled_tas, scaled_tasmin, scaled_tasmax, scaled_rainfall, scaled_hurs)

## 4.2 Data Partitioning ####
# split data into testing and training sets
data_splits <- sdf_random_split(analysis_data, train = 80, test = 20, seed = 10)
data_train <- data_splits$train
data_test <- data_splits$test

ml_formula <- formula(pmcount ~ scaled_tas + scaled_tasmin + scaled_tasmax + scaled_rainfall + scaled_hurs + type + year)

## 4.3 Logistic Regression ####
ml_log <- ml_logistic_regression(data_train, ml_formula)
log_evaluate <- ml_evaluate(ml_log, data_test)
log_evaluate$area_under_roc()

## 4.4 Decision Tree ####
ml_dt <- ml_decision_tree(data_train, ml_formula)
# Confusion matrix
dt_predict <- ml_predict(ml_dt, data_test) %>%
  ft_string_indexer("pmcount", "pmcount_index") %>%
  collect()
dt_predict_table <- table(dt_predict$pmcount_index, dt_predict$prediction)

## 4.5 Random Forest ####
ml_rf <- ml_random_forest(data_train, ml_formula)
# Create a confusion matrix
rf_predict <- ml_predict(ml_rf, data_test) %>%
  ft_string_indexer("pmcount", "pmcount_index") %>%
  collect()
rf_predict_table <- table(rf_predict$pmcount_index, rf_predict$prediction)

## 4.6 Gradient Boosted Tree ####
ml_gbt <- ml_gradient_boosted_trees(data_train, ml_formula)
# Tuning the Boosted Tree Model, using 50 iterations, rather than 20
ml_gbt2 <- ml_gradient_boosted_trees(data_train, ml_formula, max_iter = 50)
ml_gbt3 <- ml_gradient_boosted_trees(data_train, ml_formula, max_iter = 100)
# Increase depth of trees to try and further improve accuracy
ml_gbt4 <- ml_gradient_boosted_trees(data_train, ml_formula, max_iter = 100, max_depth = 6)
# Confusion matrix
gbt_predict <- ml_predict(ml_gbt4, data_test) %>%
  ft_string_indexer("pmcount", "pmcount_index") %>%
  collect()
gbt_predict_table <- table(gbt_predict$pmcount_index, gbt_predict$prediction)
print(gbt_predict_table)

## 5.0 Results ####

# Accuracy of the models
log_acc <- ml_evaluate(ml_log, data_test)$accuracy()
dt_acc <- ml_evaluate(ml_dt, data_test)
rf_acc <- ml_evaluate(ml_rf, data_test)
gbt_acc <- ml_evaluate(ml_gbt, data_test)
gbt2_acc <- ml_evaluate(ml_gbt2, data_test)
gbt3_acc <- ml_evaluate(ml_gbt3, data_test)
# increasing the number of iterations improves accuracy 
gbt4_acc <- ml_evaluate(ml_gbt4, data_test)
# increasing depth of trees improves accuracy

accuracies <- c(log_acc, dt_acc, rf_acc, gbt_acc, gbt2_acc, gbt3_acc, gbt4_acc)
acc <- as.data.frame.numeric(accuracies)
colnames(acc) <- "accuracy"
acc$model <- c("Logistic", "Decision Tree", "Random Forest", "Gradient Boosted Model",
               "GBM (50 Iterations)", "GBM (100 Iterations)", "GBM (100 Iterations, depth of 6)")
acc$model <- factor(acc$model, levels = c("Logistic", "Decision Tree", "Random Forest", "Gradient Boosted Model",
                                          "GBM (50 Iterations)", "GBM (100 Iterations)", "GBM (100 Iterations, depth of 6)"))
acc$accuracy <- as.numeric(acc$accuracy)
acc_plot <- ggplot(data = acc, aes(x = model, y = accuracy, fill = accuracy)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(accuracy, 3)), hjust = 1.5, color = "white", size = 3.5) +
  labs(y = "Accuracy", x = "Model", title = "Comparison of Model Accuracy") +
  coord_flip() +
  scale_fill_viridis(option = "D", limits = c(0.5, 0.9)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(family = "Avenir", hjust = 0.5),
    axis.title = element_text(family = "Avenir"),
    axis.text = element_text(family = "Avenir"),
    legend.position = "none"
  ) 

ggsave("acc_plot.png", acc_plot, width = 15, height = 15, unit = "cm")

# Plot the feature importance for the three tree based models
dt <- ml_tree_feature_importance(ml_dt) 
dt$model <- rep("Decision Tree")
gbt <- ml_tree_feature_importance(ml_gbt4)
gbt$model <- rep("Gradient Boosted Tree")
rf <- ml_tree_feature_importance(ml_rf)
rf$model <- rep("Random Forest")

importances <- rbind(dt, rf, gbt)

importances$feature <- as.factor(importances$feature)
importances$model <- as.factor(importances$model)

# Plot results

importances_plot <- importances %>%
  ggplot(aes(reorder(feature, importance), importance, fill = model)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~model) +
  coord_flip() +
  labs(title = "Feature Importance of Environmental Variables",
       x = "",
       y = "Importance") +
  theme(plot.title = element_text(family = "Avenir"),
        axis.title = element_text(family = "Avenir"),
        axis.text = element_text(family = "Avenir"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "grey70", 
                                    fill = "#FFFFFF00"),
        strip.text = element_text(family = "Avenir"),
        strip.background = element_rect(fill = "white"))
importances_plot

ggsave("importances_plot.png", plot = importances_plot, 
       width = 15, height = 12, unit = "cm")

# Disconnect spark connection
spark_disconnect(sc)



