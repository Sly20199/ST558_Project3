# API.R
library(plumber)
library(tidyverse)
library(tidymodels)
library(ranger)

# Load data and fit best model (Random Forest) on entire dataset
water <- read_csv("~/ST558_Project3/water_potability.csv", show_col_types = FALSE) |>
  rename_with(tolower) |>
  mutate(potability = factor(potability, levels = c(0,1), labels = c("Not Potable", "Potable")))

# Compute column means for default parameter values (ignoring NAs)
ph_mean <- round(mean(water$ph, na.rm = TRUE), 2)
hardness_mean <- round(mean(water$hardness, na.rm = TRUE), 2)
solids_mean <- round(mean(water$solids, na.rm = TRUE), 2)
chloramines_mean <- round(mean(water$chloramines, na.rm = TRUE), 2)
sulfate_mean <- round(mean(water$sulfate, na.rm = TRUE), 2)
conductivity_mean <- round(mean(water$conductivity, na.rm = TRUE), 2)
organic_carbon_mean <- round(mean(water$organic_carbon, na.rm = TRUE), 2)
trihalomethanes_mean <- round(mean(water$trihalomethanes, na.rm = TRUE), 2)
turbidity_mean <- round(mean(water$turbidity, na.rm = TRUE), 2)

# Load the finalize best model workflow, then refit it on the entire dataset.
best_model <- readRDS( "~/ST558_Project3/best_model.rds")
full_fit <- best_model |>
  fit(data = water)

#* Predict water potability from physicochemical measurements
#* @param ph pH value of the water (0 to 14)
#* @param hardness Capacity of water to precipitate soap in mg/L
#* @param solids Total dissolved solids in ppm
#* @param chloramines Amount of Chloramines in ppm
#* @param sulfate Amount of Sulfates dissolved in mg/L
#* @param conductivity Electrical conductivity of water in μS/cm
#* @param organic_carbon Amount of organic carbon in ppm
#* @param trihalomethanes Amount of Trihalomethanes in μg/L
#* @param turbidity Measure of light emitting property of water in NTU
#* @get /pred 
function( ph              = ph_mean,
          hardness        = hardness_mean,
          solids          = solids_mean,
          chloramines     = chloramines_mean,
          sulfate         = sulfate_mean,
          conductivity    = conductivity_mean,
          organic_carbon  = organic_carbon_mean,
          trihalomethanes = trihalomethanes_mean,
          turbidity       = turbidity_mean) {
  
  new_data <- tibble(
    ph              = as.numeric(ph),
    hardness        = as.numeric(hardness),
    solids          = as.numeric(solids),
    chloramines     = as.numeric(chloramines),
    sulfate         = as.numeric(sulfate),
    conductivity    = as.numeric(conductivity),
    organic_carbon  = as.numeric(organic_carbon),
    trihalomethanes = as.numeric(trihalomethanes),
    turbidity      = as.numeric(turbidity)
  )
  
  pred_class <- predict(full_fit, new_data = new_data, type = "class")
  pred_prob <- predict(full_fit, new_data = new_data, type = "prob")
  
  list(
    predicted_class  = as.character(pred_class$.pred_class),
    prob_not_potable = round(pred_prob[[".pred_Not Potable"]], 4),
    prob_potable = round(pred_prob[[".pred_Potable"]], 4)
  )
}

# Query with http://localhost:8000/pred
# Query with http://localhost:8000/pred?ph=3.5&hardness=150&solids=25000&chloramines=5&sulfate=450&conductivity=500&organic_carbon=18&trihalomethanes=80&turbidity=4.5
# Query with http://localhost:8000/pred?ph=7.5&hardness=200&solids=10000&chloramines=7&sulfate=250&conductivity=380&organic_carbon=10&trihalomethanes=55&turbidity=3.2


#* Return author info and GitHub Pages URL
#* @get /info
function() {
  list(
    author = "Stephanie Shuai",
    github_pages = ""
  )
}

# Query with http:localhost:8000/info


#* Confusion matrix plot for the Random Forest fit on full dataset
#* @serializer png list(width = 600, height = 500)
#* @get /confusion
function(){
  preds <- augment(full_fit, new_data = water)
  
  cm <- conf_mat(preds, truth = potability, estimate = .pred_class)
  
  p <- autoplot(cm, type = "heatmap") +
    scale_fill_gradient(low = "#EAF4FB", high = "#2980B9") +
    labs(
      title = "Confusion Matrix: Random Forest",
      subtitle = "Predicted vs. Actual Water Potability",
      x = "Predicted Class",
      y = "True Class"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray40")
    )
  print(p)
}

# Query with http://localhost:8000/confusion