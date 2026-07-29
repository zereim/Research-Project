library(tidyverse)
library(ggplot2)
library(modeldata)
library(car)  # For Levene's test
library(stringr)  # For string manipulation

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
  mutate(
    measure_label = str_remove(measure_title, "^CAHPS for MIPS SSM: "),
    measure_label = str_wrap(measure_label, width = 24)
  ) %>%
  ggplot(aes(x = reorder(measure_label, mean_score), y = mean_score)) +
  geom_col(fill = "steelblue", alpha = 0.9, width = 0.6) +
  labs(
    title = "Average score by measure",
    x = "Measure",
    y = paste("Average", value_col)
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 13),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    panel.background = element_rect(fill = "#d9d9d9", colour = NA),
    plot.background = element_rect(fill = "#d9d9d9", colour = NA)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5))

print(plot_measure)
ggsave(
    "plots/measure_plot.png",
    plot_measure,
    width = 10,
    height = 7,
    dpi = 300,
    bg = "#d9d9d9"
  )

# 2) Variation across organizations: average and spread by organization name or ID
make_short_label <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_replace_all(x, " MEDICAL GROUP", " MG")
  x <- str_replace_all(x, " HEALTH", " H")
  x <- str_replace_all(x, " HOSPITAL", " H")
  x <- str_replace_all(x, " PHYSICIANS", " P")
  x <- str_replace_all(x, " UNIVERSITY", " U")
  x <- str_replace_all(x, " INC", "")
  x <- str_replace_all(x, " LLC", "")
  x <- str_replace_all(x, " PLLC", "")
  x <- str_replace_all(x, " CORPORATION", " CO")
  x <- str_replace_all(x, " ASSOCIATION", " ASSN")
  x <- str_replace_all(x, " SYSTEM", " SYS")
  x <- str_replace_all(x, " OF THE", "")
  x <- str_replace_all(x, " THE ", "")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_wrap(x, width = 18)
  x
}

org_label_col <- NULL
for (col in c(
  "org_name", "organization_name", "org_nm", "provider_name",
  "hospital_name", "org_pac_id", "org_id", "organization_id",
  "provider_id", "hospital_id"
)) {
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
    mutate(
      org_label = make_short_label(org_label)
    ) %>%
    ggplot(aes(x = reorder(org_label, mean_score), y = mean_score)) +
    geom_col(fill = "darkgreen", alpha = 0.9, width = 0.6) +
    labs(
      title = "Top-performing organizations",
      x = "Organization",
      y = paste("Average", value_col)
    ) +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.y = element_text(size = 11),
      axis.text.x = element_text(size = 13),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 18, face = "bold"),
      panel.background = element_rect(fill = "#d9d9d9", colour = NA),
      plot.background = element_rect(fill = "#d9d9d9", colour = NA)
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(breaks = seq(0, 100, by = 5))

  print(plot_org)
  ggsave(
    "plots/organization_plot.png",
    plot_org,
    width = 10,
    height = 7,
    dpi = 300,
    bg = "#d9d9d9"
  )
} else {
  message(
    "The dataset does not contain an organization-name or organization-ID ",
    "column, so organization-level ranking was skipped."
  )
}

# 3) Variance summary table, Levene's test, Welch's ANOVA, and a violin plot by measure
if ("measure_title" %in% names(df)) {
  variance_summary <- df %>%
    mutate(measure_title = as.character(measure_title)) %>%
    group_by(measure_title) %>%
    summarise(
      mean_score = mean(.data[[value_col]], na.rm = TRUE),
      sd_score = sd(.data[[value_col]], na.rm = TRUE),
      var_score = var(.data[[value_col]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(sd_score))

  print(variance_summary)

  # Levene's test for equality of variances across measures
  levene_test <- car::leveneTest(
    df[[value_col]] ~ df$measure_title
  )

  print(levene_test)

  # Prepare a version of the data without missing values for ANOVA and plotting
  analysis_df <- df %>%
    mutate(measure_title = as.character(measure_title)) %>%
    filter(!is.na(.data[[value_col]]), !is.na(measure_title))

  analysis_df$measure_title <- factor(analysis_df$measure_title)

  # Welch's ANOVA for comparing means when variances are unequal
  welch_test <- stats::oneway.test(
    as.formula(paste0(value_col, " ~ measure_title")),
    data = analysis_df
  )

  print(welch_test)

  analysis_df <- analysis_df %>%
    mutate(
      measure_label = str_remove(measure_title, "^CAHPS for MIPS SSM: "),
      measure_label = str_replace_all(measure_label, "[\r\n]+", " "),
      measure_label = str_replace_all(measure_label, "\\s+", " "),
      measure_label = str_squish(measure_label),
      measure_label = str_wrap(measure_label, width = 28)
    )

  # Violin plot to show the distribution of scores by measure
  plot_violin <- ggplot(analysis_df, aes(x = measure_label, y = .data[[value_col]])) +
    geom_violin(fill = "skyblue", alpha = 0.85, trim = FALSE, width = 0.9, scale = "width") +
    geom_boxplot(width = 0.12, fill = "white", outlier.alpha = 0.4, position = position_dodge(width = 0.9)) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 2.5, color = "darkred", position = position_dodge(width = 0.9)) +
    labs(
      title = paste("Score distribution by measure for", value_col),
      subtitle = "Each violin shows the distribution of scores within a measure",
      x = "Measure",
      y = paste("Score", value_col)
    ) +
    scale_y_continuous(breaks = seq(0, 100, by = 5), limits = c(0, 100)) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14),
      panel.background = element_rect(fill = "#f0f0f0", colour = NA),
      plot.background = element_rect(fill = "#f0f0f0", colour = NA),
      panel.grid.major.y = element_line(color = "#d0d0d0"),
      panel.grid.minor.y = element_blank()
    )

  print(plot_violin)
  ggsave(
    "plots/violin_plot.png",
    plot_violin,
    width = 12,
    height = 8,
    dpi = 300,
    bg = "#f0f0f0"
  )
} else {
  message("The dataset does not contain a measure_title column, so variance testing was skipped.")
}
