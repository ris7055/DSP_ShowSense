library(tidyverse)
library(caret)
library(randomForest)

# Load data and model
tv <- read_csv("C:/Users/S2180/Downloads/DS_Entertainment/data/tv_shows_clean.csv") |>
  mutate(outcome = as.factor(outcome))

rf_model <- readRDS("C:/Users/S2180/Downloads/DS_Entertainment/model/rf_model.rds")

# Create output folder
dir.create("C:/Users/S2180/Downloads/DS_Entertainment/plots", showWarnings = FALSE)

# ── EDA Plot 1: IMDb Score vs Outcome (Boxplot) ──
p1 <- ggplot(tv, aes(x = outcome, y = imdb_score, fill = outcome)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Cancelled" = "#c0392b", "Renewed" = "#1d9e75")) +
  labs(title = "IMDb Score vs Show Outcome",
       x = "Outcome", y = "IMDb Score") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

ggsave("C:/Users/S2180/Downloads/DS_Entertainment/plots/EDA_imdb_score_vs_outcome.png",
       p1, width = 6, height = 4, dpi = 300)

# ── EDA Plot 2: IMDb Votes vs Outcome ──
p2 <- ggplot(tv, aes(x = outcome, y = imdb_votes, fill = outcome)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Cancelled" = "#c0392b", "Renewed" = "#1d9e75")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "IMDb Votes vs Show Outcome",
       x = "Outcome", y = "IMDb Votes") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

ggsave("C:/Users/S2180/Downloads/DS_Entertainment/plots/EDA_imdb_votes_vs_outcome.png",
       p2, width = 6, height = 4, dpi = 300)

# ── Confusion Matrix Heatmap ──
model_data <- tv |>
  select(imdb_score, imdb_votes, runtime, release_year.x, outcome) |>
  rename(release_year = release_year.x) |>
  drop_na()

set.seed(42)
train_idx <- createDataPartition(model_data$outcome, p = 0.8, list = FALSE)
test_df   <- model_data[-train_idx, ]
predictions <- predict(rf_model, test_df)

cm <- confusionMatrix(predictions, test_df$outcome)
cm_df <- as.data.frame(cm$table)

p3 <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = Freq), size = 10, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#1a1a2e", high = "#534ab7") +
  labs(title = "Confusion Matrix — Random Forest",
       subtitle = "Test set predictions (282 shows)",
       x = "Actual Outcome", y = "Predicted Outcome") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(face = "bold"))

ggsave("C:/Users/S2180/Downloads/DS_Entertainment/plots/Confusion_Matrix.png",
       p3, width = 6, height = 5, dpi = 300)

cat("All 3 plots saved to DS_Entertainment/plots/ folder!\n")