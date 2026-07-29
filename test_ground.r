library(tidyverse)
library(ggplot2)
library(modeldata)

# Make sure a folder exists for exported figures
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Load the healthcare dataset if it is available in your R environment
if (!exists("cms_patient_experience")) {
  data(cms_patient_experience, package = "modeldata")
}

# If your data object has a different name, replace cms_patient_experience below
# with that name.
df <- cms_patient_experience

# View a compact preview of the dataset in the console
print(head(df, 10))

# Find a numeric value column automatically if the name is not exactly "score"
value_col <- NULL
for (col in c("score", "value", "measure_value", "score_value")) {
  if (col %in% names(df)) {
    value_col <- col
    break
  }
}

if (is.null(value_col)) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  if (length(numeric_cols) == 0) {
    stop("No numeric value column was found to average.")
  }
  value_col <- numeric_cols[1]
}

if (!"measure_title" %in% names(df)) {
  stop("The dataset must contain a measure_title column.")
}

# 1) Variation across measures: average and spread by measure_title
measure_summary <- df %>%
  mutate(measure_title = as.character(measure_title)) %>%
  group_by(measure_title) %>%
  summarise(
    mean_score = mean(.data[[value_col]], na.rm = TRUE),
    sd_score = sd(.data[[value_col]], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_score))

print(measure_summary)

# Plot measure-level averages
plot_measure <- measure_summary %>%
  mutate(measure_title = str_wrap(measure_title, width = 28)) %>%
  ggplot(aes(x = reorder(measure_title, mean_score), y = mean_score)) +
  geom_col(fill = "steelblue", alpha = 0.9, width = 0.6) +
  coord_flip() +
  labs(
    title = "Average score by measure",
    x = "Measure",
    y = paste("Average", value_col)
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  )

print(plot_measure)
ggsave("plots/measure_plot.png", plot_measure, width = 10, height = 7, dpi = 300)

# 2) Variation across organizations: average and spread by organization name or ID
org_label_col <- NULL
for (col in c("org_name", "organization_name", "org_nm", "provider_name", "hospital_name", "org_pac_id", "org_id", "organization_id", "provider_id", "hospital_id")) {
  if (col %in% names(df)) {
    org_label_col <- col
    break
  }
}

if (!is.null(org_label_col)) {
  org_summary <- df %>%
    group_by(.data[[org_label_col]]) %>%
    summarise(
      mean_score = mean(.data[[value_col]], na.rm = TRUE),
      sd_score = sd(.data[[value_col]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_score))

  names(org_summary)[1] <- "org_label"
  print(org_summary)

  # Top-performing organizations
  top_orgs <- org_summary %>%
    slice_max(order_by = mean_score, n = 10)

  print(top_orgs)

  # Plot top organizations
  plot_org <- top_orgs %>%
    mutate(org_label = str_wrap(as.character(org_label), width = 20)) %>%
    ggplot(aes(x = reorder(org_label, mean_score), y = mean_score)) +
    geom_col(fill = "darkgreen", alpha = 0.9, width = 0.6) +
    coord_flip() +
    labs(
      title = "Top-performing organizations",
      x = "Organization",
      y = paste("Average", value_col)
    ) +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 13),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 18, face = "bold")
    )

  print(plot_org)
  ggsave("plots/organization_plot.png", plot_org, width = 10, height = 7, dpi = 300)
} else {
  message("The dataset does not contain an organization-name or organization-ID column, so organization-level ranking was skipped.")
}

# 3) Optional: compare organizations across measures if you have a repeated measure structure
if (!is.null(org_label_col) && "measure_title" %in% names(df)) {
  org_measure_summary <- df %>%
    group_by(.data[[org_label_col]], measure_title) %>%
    summarise(
      mean_score = mean(.data[[value_col]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  print(head(org_measure_summary))
}