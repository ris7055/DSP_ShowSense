unzip("C:/Users/S2180/Downloads/DS_Entertainment/data/Netflix Movies and TV Shows.zip", list = TRUE)
unzip("C:/Users/S2180/Downloads/DS_Entertainment/data/Netflix IMDB Scores.zip", list = TRUE)
unzip("C:/Users/S2180/Downloads/DS_Entertainment/data/IMDb Dataset (2024) updated.zip", list = TRUE)



# Extract only the files we need
unzip("C:/Users/S2180/Downloads/DS_Entertainment/data/Netflix Movies and TV Shows.zip",
      files = "netflix_titles.csv",
      exdir = "C:/Users/S2180/Downloads/DS_Entertainment/data/")

unzip("C:/Users/S2180/Downloads/DS_Entertainment/data/Netflix IMDB Scores.zip",
      files = "Netflix TV Shows and Movies.csv",
      exdir = "C:/Users/S2180/Downloads/DS_Entertainment/data/")

unzip("C:/Users/S2180/Downloads/DS_Entertainment/data/IMDb Dataset (2024) updated.zip",
      files = "IMDb_Dataset_2.csv",
      exdir = "C:/Users/S2180/Downloads/DS_Entertainment/data/")


install.packages(c("tidyverse", "janitor", "skimr", "caret", 
                   "randomForest", "tidytext", "shiny", 
                   "shinydashboard", "plotly", "rsconnect", "DT"))


# Load all 3
library(tidyverse)

titles  <- read_csv("C:/Users/S2180/Downloads/DS_Entertainment/data/netflix_titles.csv")
scores  <- read_csv("C:/Users/S2180/Downloads/DS_Entertainment/data/Netflix TV Shows and Movies.csv")
imdb    <- read_csv("C:/Users/S2180/Downloads/DS_Entertainment/data/IMDb_Dataset_2.csv")

# Check columns
cat("=== netflix_titles columns ===\n")
colnames(titles)

cat("\n=== Netflix TV Shows and Movies columns ===\n")
colnames(scores)

cat("\n=== IMDb_Dataset_2 columns ===\n")
colnames(imdb)



# Cleaning the Data:

library(tidyverse)
library(janitor)

# ── Load data ──────────────────────────────────────────────
titles <- read_csv("C:/Users/S2180/Downloads/DS_Entertainment/data/netflix_titles.csv")
scores <- read_csv("C:/Users/S2180/Downloads/DS_Entertainment/data/Netflix TV Shows and Movies.csv")

# ── Keep only TV Shows from titles ────────────────────────
tv_titles <- titles |>
  filter(type == "TV Show") |>
  select(title, duration, listed_in, release_year) |>
  mutate(
    seasons = as.numeric(str_extract(duration, "\\d+")),
    genre   = str_extract(listed_in, "^[^,]+")
  ) |>
  select(title, seasons, genre, release_year)

# ── Keep only TV Shows from scores ────────────────────────
tv_scores <- scores |>
  filter(type == "SHOW") |>
  select(title, imdb_score, imdb_votes, runtime, release_year)

# ── Join both datasets by title ───────────────────────────
tv_merged <- tv_titles |>
  inner_join(tv_scores, by = "title") |>
  drop_na(imdb_score, seasons, imdb_votes) |>
  mutate(
    outcome = ifelse(seasons >= 2, "Renewed", "Cancelled"),
    outcome = as.factor(outcome)
  )

# ── Check result ───────────────────────────────────────────
cat("Total shows after merge:", nrow(tv_merged), "\n")
cat("Outcome breakdown:\n")
print(table(tv_merged$outcome))

cat("\nSample data:\n")
print(head(tv_merged, 5))

# ── Save cleaned data ──────────────────────────────────────
write_csv(tv_merged, 
          "C:/Users/S2180/Downloads/DS_Entertainment/data/tv_shows_clean.csv")
cat("\nCleaned data saved!\n")

# Model Training:
library(tidyverse)
library(caret)
library(randomForest)

# ── Load cleaned data ──────────────────────────────────────
tv <- read_csv("C:/Users/S2180/Downloads/DS_Entertainment/data/tv_shows_clean.csv") |>
  mutate(outcome = as.factor(outcome))

# ── Select features for model ──────────────────────────────
model_data <- tv |>
  select(imdb_score, imdb_votes, runtime, release_year.x, outcome) |>
  rename(release_year = release_year.x) |>
  drop_na()

cat("Rows used for modelling:", nrow(model_data), "\n")

# ── Train/test split 80/20 ─────────────────────────────────
set.seed(42)
train_idx <- createDataPartition(model_data$outcome, p = 0.8, list = FALSE)
train_df  <- model_data[train_idx, ]
test_df   <- model_data[-train_idx, ]

cat("Training rows:", nrow(train_df), "\n")
cat("Testing rows:", nrow(test_df), "\n")

# ── Train Random Forest ────────────────────────────────────
set.seed(42)
rf_model <- randomForest(
  outcome ~ .,
  data       = train_df,
  ntree      = 100,
  mtry       = 2,
  importance = TRUE
)

print(rf_model)

# ── Evaluate on test set ───────────────────────────────────
predictions <- predict(rf_model, test_df)
conf_matrix <- confusionMatrix(predictions, test_df$outcome)
print(conf_matrix)

cat("\nModel Accuracy:", 
    round(conf_matrix$overall["Accuracy"] * 100, 1), "%\n")

# ── Feature importance ─────────────────────────────────────
cat("\nFeature Importance:\n")
importance_df <- as.data.frame(importance(rf_model)) |>
  rownames_to_column("feature") |>
  arrange(desc(MeanDecreaseGini))
print(importance_df)

# ── Save model ─────────────────────────────────────────────
dir.create("C:/Users/S2180/Downloads/DS_Entertainment/model", 
           showWarnings = FALSE)
saveRDS(rf_model, 
        "C:/Users/S2180/Downloads/DS_Entertainment/model/rf_model.rds")
cat("\nModel saved!\n")