knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  out.width = "95%"
)

library(tidyverse)
library(AmesHousing)
library(broom)
library(knitr)
library(patchwork)
library(gridExtra)
library(grid)
library(car)

# load data
ames <- make_ames()
dim(ames)
head(ames)
write.csv(ames, "ames.csv", row.names = FALSE)

# clean data
clean_ames<- ames %>%
  # create house age
  mutate(
    House_Age = (Year_Sold - Year_Built) + Mo_Sold/12
  ) %>%
  
  # Convert fireplaces into categorical variable
   mutate(
  Fireplace_Category = case_when(
    Fireplaces == 0 ~ "0 fireplaces",
    Fireplaces == 1 ~ "1 fireplace",
    Fireplaces >= 2 ~ "2 or more fireplaces"
  ),
  
  Overall_Qual_Category = case_when(
    Overall_Qual %in% c("Very_Poor", "Poor", "Fair", "Below_Average") ~ "Bad",
    Overall_Qual %in% c("Average", "Above_Average") ~ "Average",
    Overall_Qual %in% c("Good", "Very_Good", "Excellent", "Very_Excellent") ~ "Good"
  )
)%>%
   # convert to factor
  mutate(
    Fireplace_Category = factor(
      Fireplace_Category,
      levels = c("0 fireplaces", "1 fireplace", "2 or more fireplaces")
    ),
    
    Overall_Qual_Category = factor(
      Overall_Qual_Category,
      levels = c("Bad", "Average", "Good")
    )
  ) %>%
  # select variables
  dplyr::select(
    Sale_Price,
    Gr_Liv_Area,
    Total_Bsmt_SF,
    House_Age,
    Overall_Qual_Category,
    Fireplace_Category,
    Full_Bath,
  ) %>%
  na.omit() %>%
  filter(
    Sale_Price > 0,
    Gr_Liv_Area >= 0,
    Total_Bsmt_SF >=0,
    House_Age >= 0,
    Full_Bath >= 0
  ) %>% mutate(
    log_sale_price = log(Sale_Price)
  )


write.csv(clean_ames, "clean_ames.csv", row.names = FALSE)



sale_price_summary <- tibble(
  Statistic = c(
    "Minimum", "First quartile", "Median", "Mean",
    "Third quartile", "Maximum", "Standard deviation",
    "Interquartile range"
  ),
  Value = c(
    min(clean_ames$Sale_Price, na.rm = TRUE),
    quantile(clean_ames$Sale_Price, 0.25, na.rm = TRUE),
    median(clean_ames$Sale_Price, na.rm = TRUE),
    mean(clean_ames$Sale_Price, na.rm = TRUE),
    quantile(clean_ames$Sale_Price, 0.75, na.rm = TRUE),
    max(clean_ames$Sale_Price, na.rm = TRUE),
    sd(clean_ames$Sale_Price, na.rm = TRUE),
    IQR(clean_ames$Sale_Price, na.rm = TRUE)
  )
) %>%
  mutate(Value = scales::dollar(round(Value, 0)))

log_sale_price_summary <- tibble(
  Statistic = c(
    "Minimum", "First quartile", "Median", "Mean",
    "Third quartile", "Maximum", "Standard deviation",
    "Interquartile range"
  ),
  Value = c(
    min(clean_ames$log_sale_price, na.rm = TRUE),
    quantile(clean_ames$log_sale_price, 0.25, na.rm = TRUE),
    median(clean_ames$log_sale_price, na.rm = TRUE),
    mean(clean_ames$log_sale_price, na.rm = TRUE),
    quantile(clean_ames$log_sale_price, 0.75, na.rm = TRUE),
    max(clean_ames$log_sale_price, na.rm = TRUE),
    sd(clean_ames$log_sale_price, na.rm = TRUE),
    IQR(clean_ames$log_sale_price, na.rm = TRUE)
  )
) %>%
  mutate(Value = round(Value, 3))

table_theme <- ttheme_default(
  core = list(
    fg_params = list(fontsize = 15),
    bg_params = list(
      fill = c("#F7F9FC", "white"),
      col = "#C7CED8",
      lwd = 0.8
    )
  ),
  colhead = list(
    fg_params = list(
      fontsize = 13,
      fontface = "bold",
      col = "white"
    ),
    bg_params = list(
      fill = "#3F5F8A",
      col = "#3F5F8A",
      lwd = 0.8
    )
  )
)

sale_price_table_body <- tableGrob(
  sale_price_summary,
  rows = NULL,
  theme = table_theme
)

log_sale_price_table_body <- tableGrob(
  log_sale_price_summary,
  rows = NULL,
  theme = table_theme
)

# Helper function: keeps title above the table properly
make_titled_table <- function(table_body, title_text) {
  title_grob <- textGrob(
    title_text,
    gp = gpar(fontface = "bold", fontsize = 13)
  )
  
  arrangeGrob(
    title_grob,
    table_body,
    ncol = 1,
    heights = unit.c(
      unit(0.35, "in"),
      sum(table_body$heights)
    )
  )
}

sale_price_table <- make_titled_table(
  sale_price_table_body,
  "Table 1.1. Summary Statistics for Sale Price"
)

log_sale_price_table <- make_titled_table(
  log_sale_price_table_body,
  "Table 1.2. Summary Statistics for Log Sale Price"
)

sale_price_histogram <- ggplot(clean_ames, aes(x = Sale_Price)) +
  geom_histogram(
    bins = 30,
    fill = "cornflowerblue",
    color = "white",
    linewidth = 0.2
  ) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(
    x = "Sale Price",
    y = "Number of Properties",
    caption = "Figure 1.1 Distribution of Sale Price"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray35"),
    plot.caption = element_text(
      hjust = 0.5,
      face = "bold",
      size = 13,
      margin = margin(t = 10)
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 28, r = 5, b = 20, l = 5)
  )

log_sale_price_histogram <- ggplot(clean_ames, aes(x = log_sale_price)) +
  geom_histogram(
    bins = 30,
    fill = "darksalmon",
    color = "white",
    linewidth = 0.2
  ) +
  labs(
    x = "Log Sale Price",
    y = "Number of Properties",
    caption = "Figure 1.2 Distribution of Log Sale Price",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray35"),
    plot.caption = element_text(
      hjust = 0.5,
      face = "bold",
      size = 13,
      margin = margin(t = 10)
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 28, r = 5, b = 20, l = 5)
  )

sale_price_column <- wrap_elements(sale_price_table) / sale_price_histogram +
  plot_layout(heights = c(2.2, 2.6))

log_sale_price_column <- wrap_elements(log_sale_price_table) / log_sale_price_histogram +
  plot_layout(heights = c(2.2, 2.6))

sale_price_column | log_sale_price_column


hist_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    plot.margin = margin(8, 8, 8, 8)
  )

p1 <- ggplot(clean_ames, aes(x = Gr_Liv_Area)) +
  geom_histogram(
    bins = 30,
    fill = "cornflowerblue",
    color = "white",
    linewidth = 0.2
  ) +
  labs(
    title = "Above-ground Living Area",
    x = "Above-ground living area",
    y = "Count"
  ) +
  hist_theme

p2 <- ggplot(clean_ames, aes(x = Total_Bsmt_SF)) +
  geom_histogram(
    bins = 30,
    fill = "darkseagreen3",
    color = "white",
    linewidth = 0.2
  ) +
  labs(
    title = "Total Basement Area",
    x = "Total basement area",
    y = "Count"
  ) +
  hist_theme

p3 <- ggplot(clean_ames, aes(x = House_Age)) +
  geom_histogram(
    bins = 30,
    fill = "darksalmon",
    color = "white",
    linewidth = 0.2
  ) +
  labs(
    title = "House Age",
    x = "House age",
    y = "Count"
  ) +
  hist_theme

p4 <- ggplot(clean_ames, aes(x = Full_Bath)) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5,
    fill = "plum3",
    color = "white",
    linewidth = 0.2
  ) +
  scale_x_continuous(breaks = 0:4) +
  labs(
    title = "Full Bathrooms",
    x = "Number of full bathrooms",
    y = "Count"
  ) +
  hist_theme

histogram_plots <- arrangeGrob(
  p1, p2,
  p3, p4,
  ncol = 2
)

hist_caption <- textGrob(
  "Figure 1.3 Histograms of Numerical Predictors",
  x = 0.5,
  hjust = 0.5,
  gp = gpar(fontface = "bold", fontsize = 13)
)

grid.arrange(
  histogram_plots,
  hist_caption,
  ncol = 1,
  heights = c(1, 0.05)
)

# Numerical predictor summary table
numeric_predictor_summary <- tibble(
  Variable = c(
    "Above-ground living area",
    "Total basement area",
    "House age",
    "Full bathrooms"
  ),
  Mean = c(
    mean(clean_ames$Gr_Liv_Area, na.rm = TRUE),
    mean(clean_ames$Total_Bsmt_SF, na.rm = TRUE),
    mean(clean_ames$House_Age, na.rm = TRUE),
    mean(clean_ames$Full_Bath, na.rm = TRUE)
  ),
  Median = c(
    median(clean_ames$Gr_Liv_Area, na.rm = TRUE),
    median(clean_ames$Total_Bsmt_SF, na.rm = TRUE),
    median(clean_ames$House_Age, na.rm = TRUE),
    median(clean_ames$Full_Bath, na.rm = TRUE)
  ),
  SD = c(
    sd(clean_ames$Gr_Liv_Area, na.rm = TRUE),
    sd(clean_ames$Total_Bsmt_SF, na.rm = TRUE),
    sd(clean_ames$House_Age, na.rm = TRUE),
    sd(clean_ames$Full_Bath, na.rm = TRUE)
  ),
  Minimum = c(
    min(clean_ames$Gr_Liv_Area, na.rm = TRUE),
    min(clean_ames$Total_Bsmt_SF, na.rm = TRUE),
    min(clean_ames$House_Age, na.rm = TRUE),
    min(clean_ames$Full_Bath, na.rm = TRUE)
  ),
  Maximum = c(
    max(clean_ames$Gr_Liv_Area, na.rm = TRUE),
    max(clean_ames$Total_Bsmt_SF, na.rm = TRUE),
    max(clean_ames$House_Age, na.rm = TRUE),
    max(clean_ames$Full_Bath, na.rm = TRUE)
  )
) %>%
  mutate(
    Mean = round(Mean, 2),
    Median = round(Median, 2),
    SD = round(SD, 2),
    Minimum = round(Minimum, 2),
    Maximum = round(Maximum, 2)
  )

# Categorical predictor summary table
categorical_predictor_summary <- bind_rows(
  clean_ames %>%
    count(Overall_Qual_Category) %>%
    mutate(
      Variable = "Overall quality",
      Category = as.character(Overall_Qual_Category),
      Percent = paste0(round(n / sum(n) * 100, 1), "%")
    ) %>%
    dplyr::select(Variable, Category, Count = n, Percent),

  clean_ames %>%
    count(Fireplace_Category) %>%
    mutate(
      Variable = "Fireplace",
      Category = as.character(Fireplace_Category),
      Percent = paste0(round(n / sum(n) * 100, 1), "%")
    ) %>%
    dplyr::select(Variable, Category, Count = n, Percent)
)

# Shared table style
predictor_table_theme <- ttheme_default(
  core = list(
    fg_params = list(fontsize = 10),
    bg_params = list(
      fill = c("#F7F9FC", "white"),
      col = "#C7CED8",
      lwd = 0.8
    ),
    padding = unit(c(4, 4), "mm")
  ),
  colhead = list(
    fg_params = list(
      fontsize = 10,
      fontface = "bold",
      col = "white"
    ),
    bg_params = list(
      fill = "#3F5F8A",
      col = "#3F5F8A",
      lwd = 0.8
    ),
    padding = unit(c(4, 4), "mm")
  )
)

numeric_predictor_table <- tableGrob(
  numeric_predictor_summary,
  rows = NULL,
  theme = predictor_table_theme
)

categorical_predictor_table <- tableGrob(
  categorical_predictor_summary,
  rows = NULL,
  theme = predictor_table_theme
)

numeric_predictor_table$widths <- unit(
  c(2.7, 1, 1, 1, 1, 1),
  "null"
)

categorical_predictor_table$widths <- unit(
  c(2, 2.4, 1, 1),
  "null"
)

numeric_title <- textGrob(
  "Table 1.3 Summary Statistics for Numerical Predictors",
  x = 0.5,
  hjust = 0.5,
  gp = gpar(fontface = "bold", fontsize = 10)
)

categorical_title <- textGrob(
  "Table 1.4 Summary Statistics for Categorical Predictors",
  x = 0.5,
  hjust = 0.5,
  gp = gpar(fontface = "bold", fontsize = 10)
)

numeric_block <- arrangeGrob(
  numeric_title,
  numeric_predictor_table,
  heights = unit.c(unit(0.4, "in"), unit(1, "null"))
)

categorical_block <- arrangeGrob(
  categorical_title,
  categorical_predictor_table,
  heights = unit.c(unit(0.4, "in"), unit(1, "null"))
)

grid.arrange(
  numeric_block,
  categorical_block,
  ncol = 1,
  heights = c(1, 1.15),
  padding = unit(0.05, "in")
)

# Fit linear regression model
model_pre <- lm(
  log_sale_price ~
    Gr_Liv_Area +
    Total_Bsmt_SF +
    House_Age +
    Overall_Qual_Category +
    Fireplace_Category +
    Full_Bath,
  data = clean_ames
)

# Create regression coefficient table
coef_table <- broom::tidy(model_pre) %>%
  mutate(
    Term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "Gr_Liv_Area" ~ "Above-ground living area",
      term == "Total_Bsmt_SF" ~ "Total basement area",
      term == "House_Age" ~ "House age",
      term == "Overall_Qual_CategoryAverage" ~ "Quality: Average vs Bad",
      term == "Overall_Qual_CategoryGood" ~ "Quality: Good vs Bad",
      term == "Fireplace_Category1 fireplace" ~ "Fireplace: 1 vs 0",
      term == "Fireplace_Category2 or more fireplaces" ~ "Fireplace: 2+ vs 0",
      term == "Full_Bath" ~ "Full bathrooms",
      TRUE ~ term
    ),
    Estimate = round(estimate, 6),
    `Std. Error` = round(std.error, 6),
    `t value` = round(statistic, 3),
    `p value` = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))
  ) %>%
  dplyr::select(Term, Estimate, `Std. Error`, `t value`, `p value`)

# Same style as previous tables
regression_table_theme <- ttheme_default(
  core = list(
    fg_params = list(fontsize = 12),
    bg_params = list(
      fill = c("#F7F9FC", "white"),
      col = "#C7CED8",
      lwd = 0.8
    ),
    padding = unit(c(4, 4), "mm")
  ),
  colhead = list(
    fg_params = list(
      fontsize = 12,
      fontface = "bold",
      col = "white"
    ),
    bg_params = list(
      fill = "#3F5F8A",
      col = "#3F5F8A",
      lwd = 0.8
    ),
    padding = unit(c(4, 4), "mm")
  )
)

regression_table_body <- tableGrob(
  coef_table,
  rows = NULL,
  theme = regression_table_theme
)

# Adjust column widths
regression_table_body$widths <- unit(
  c(3.2, 1.2, 1.2, 1.1, 1.1),
  "null"
)

# Add title above table
regression_title <- textGrob(
  "Table 1.5 Linear Regression Coefficients",
  gp = gpar(fontface = "bold", fontsize = 10)
)

regression_table <- arrangeGrob(
  regression_title,
  regression_table_body,
  ncol = 1,
  heights = unit.c(
    unit(0.4, "in"),
    sum(regression_table_body$heights)
  )
)

grid.arrange(regression_table)

# Residual diagnostic plots
old_par <- par(no.readonly = TRUE)

par(
  mfrow = c(2, 2),
  pin = c(2.8, 2.8),
  mar = c(4, 4, 3, 1),
  oma = c(4, 0, 0, 0)   # extra outside space at bottom for whole figure caption
)

# Residuals vs fitted
plot(
  model_pre,
  which = 1,
  id.n = 0,
  cex = 0.7,
  cex.lab = 1.1,
  cex.axis = 1.0,
  cex.main = 1.2
)

# QQ plot
plot(
  model_pre,
  which = 2,
  id.n = 0,
  cex = 0.7,
  cex.lab = 1.1,
  cex.axis = 1.0,
  cex.main = 1.2
)

# Standardized residuals vs fitted
fitted_values <- fitted(model_pre)
std_residuals <- rstandard(model_pre)

plot(
  fitted_values,
  std_residuals,
  main = "Fitted vs Standardized Residuals",
  xlab = "Fitted Values",
  ylab = "Standardized Residuals",
  pch = 16,
  col = "black"
)

abline(h = 0, col = "red")

# Histogram of standardized residuals
hist(
  std_residuals,
  breaks = 30,
  main = "Histogram of Standardized Residuals",
  xlab = "Standardized Residuals",
  col = "lightgray",
  xlim = c(-4, 4)
)

# Whole figure caption
mtext(
  "Figure 1.3 Residual diagnostic plots ",
  side = 1,
  outer = TRUE,
  line = 1.5,
  font = 0.8,
  cex = 1
)

par(old_par)

# function for presenting a table contains the regression coefs
make_regression_table <- function(model, title_text = "Linear Regression Coefficients") {
  
  coef_table <- broom::tidy(model) %>%
    mutate(
      Term = case_when(
        term == "(Intercept)" ~ "Intercept",
        term == "Gr_Liv_Area" ~ "Above-ground living area",
        term == "Total_Bsmt_SF" ~ "Total basement area",
        term == "House_Age" ~ "House age",
        term == "I(House_Age^2)" ~ "House age squared",
        term == "Overall_Qual_CategoryAverage" ~ "Quality: Average vs Bad",
        term == "Overall_Qual_CategoryGood" ~ "Quality: Good vs Bad",
        term == "Fireplace_Category1 fireplace" ~ "Fireplace: 1 vs 0",
        term == "Fireplace_Category2 or more fireplaces" ~ "Fireplace: 2+ vs 0",
        term == "Full_Bath" ~ "Full bathrooms",
        term == "Gr_Liv_Area:Overall_Qual_CategoryAverage" ~
          "Living area x Quality: Average",
        term == "Gr_Liv_Area:Overall_Qual_CategoryGood" ~
          "Living area x Quality: Good",
        TRUE ~ term
      ),
      Estimate = round(estimate, 6),
      `Std. Error` = round(std.error, 6),
      `t value` = round(statistic, 3),
      `p value` = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))
    ) %>%
    dplyr::select(Term, Estimate, `Std. Error`, `t value`, `p value`)
  
  regression_table_theme <- ttheme_default(
    core = list(
      fg_params = list(fontsize = 12),
      bg_params = list(
        fill = c("#F7F9FC", "white"),
        col = "#C7CED8",
        lwd = 0.8
      ),
      padding = unit(c(4, 4), "mm")
    ),
    colhead = list(
      fg_params = list(
        fontsize = 12,
        fontface = "bold",
        col = "white"
      ),
      bg_params = list(
        fill = "#3F5F8A",
        col = "#3F5F8A",
        lwd = 0.8
      ),
      padding = unit(c(4, 4), "mm")
    )
  )
  
  regression_table_body <- tableGrob(
    coef_table,
    rows = NULL,
    theme = regression_table_theme
  )
  
  regression_table_body$widths <- unit(
    c(3.4, 1.2, 1.2, 1.1, 1.1),
    "null"
  )
  
  regression_title <- textGrob(
    title_text,
    gp = gpar(fontface = "bold", fontsize = 10)
  )
  
  regression_table <- arrangeGrob(
    regression_title,
    regression_table_body,
    ncol = 1,
    heights = unit.c(
      unit(0.4, "in"),
      sum(regression_table_body$heights)
    )
  )
  
  grid.arrange(regression_table)
}

# make a comparison table for all models in the list
make_model_comparison_table <- function(..., model_names = NULL,
                                        title_text = "Model Comparison") {
  
  models <- list(...)
  
  if (is.null(model_names)) {
    model_names <- paste0("Model ", seq_along(models))
  }
  
  get_aicc <- function(model) {
    n <- nobs(model)
    k <- length(coef(model)) + 1  # coefficients + sigma
    
    AIC(model) + (2 * k * (k + 1)) / (n - k - 1)
  }
  
  comparison_table <- tibble(
    Model = model_names,
    `Adjusted R-squared` = sapply(models, function(m) summary(m)$adj.r.squared),
    AIC = sapply(models, AIC),
    BIC = sapply(models, BIC),
    AICc = sapply(models, get_aicc),
    all_significant = c("yes","yes","yes","yes","no","no")
  ) %>%
    mutate(
      `Adjusted R-squared` = round(`Adjusted R-squared`, 4),
      AIC = round(AIC, 2),
      BIC = round(BIC, 2),
      AICc = round(AICc, 2)
    )
  
  comparison_theme <- ttheme_default(
    core = list(
      fg_params = list(fontsize = 12),
      bg_params = list(
        fill = c("#F7F9FC", "white"),
        col = "#C7CED8",
        lwd = 0.8
      ),
      padding = unit(c(4, 4), "mm")
    ),
    colhead = list(
      fg_params = list(
        fontsize = 12,
        fontface = "bold",
        col = "white"
      ),
      bg_params = list(
        fill = "#3F5F8A",
        col = "#3F5F8A",
        lwd = 0.8
      ),
      padding = unit(c(4, 4), "mm")
    )
  )
  
  comparison_table_body <- tableGrob(
    comparison_table,
    rows = NULL,
    theme = comparison_theme
  )
  
  comparison_title <- textGrob(
    title_text,
    gp = gpar(fontface = "bold", fontsize = 10)
  )
  
  final_table <- arrangeGrob(
    comparison_title,
    comparison_table_body,
    ncol = 1,
    heights = unit.c(
      unit(0.4, "in"),
      sum(comparison_table_body$heights)
    )
  )
  
  grid.arrange(final_table)
}

# present all the statistics and number of extreme points for each model
single_model_stats_table <- function(model, title_text = "Model Statistics and Diagnostics",
                                          outlier_cutoff = 4) {
  
  get_aicc <- function(model) {
    n <- nobs(model)
    k <- length(coef(model)) + 1
    
    AIC(model) + (2 * k * (k + 1)) / (n - k - 1)
  }
  
  n <- nobs(model)
  p <- length(coef(model))
  
  std_resid <- rstandard(model)
  cooks_d <- cooks.distance(model)
  leverage <- hatvalues(model)
  
  cook_cutoff <- 4 / n
  leverage_cutoff <- 2 * (p + 1) / n
  
  is_outlier <- abs(std_resid) > outlier_cutoff
  is_influential <- cooks_d > cook_cutoff
  is_leverage <- leverage > leverage_cutoff
  
  model_stats <- tibble(
  Statistic = c(
    "R-squared",
    "Adjusted R-squared",
    "AIC",
    "BIC",
    "AICc",
    "Number of outliers",
    "Number of influential points",
    "Number of leverage points",
    "Number of any flagged points"
  ),
  Value = c(
    sprintf("%.4f", summary(model)$r.squared),
    sprintf("%.4f", summary(model)$adj.r.squared),
    sprintf("%.4f", AIC(model)),
    sprintf("%.4f", BIC(model)),
    sprintf("%.4f", get_aicc(model)),
    as.character(sum(is_outlier, na.rm = TRUE)),
    as.character(sum(is_influential, na.rm = TRUE)),
    as.character(sum(is_leverage, na.rm = TRUE)),
    as.character(sum(is_outlier | is_influential | is_leverage, na.rm = TRUE))
  )
)
  
  model_stats_theme <- ttheme_default(
    core = list(
      fg_params = list(fontsize = 12),
      bg_params = list(
        fill = c("#F7F9FC", "white"),
        col = "#C7CED8",
        lwd = 0.8
      ),
      padding = unit(c(4, 4), "mm")
    ),
    colhead = list(
      fg_params = list(
        fontsize = 12,
        fontface = "bold",
        col = "white"
      ),
      bg_params = list(
        fill = "#3F5F8A",
        col = "#3F5F8A",
        lwd = 0.8
      ),
      padding = unit(c(4, 4), "mm")
    )
  )
  
  model_stats_body <- tableGrob(
    model_stats,
    rows = NULL,
    theme = model_stats_theme
  )
  
  model_stats_title <- textGrob(
    title_text,
    gp = gpar(fontface = "bold", fontsize = 10)
  )
  
  model_stats_final <- arrangeGrob(
    model_stats_title,
    model_stats_body,
    ncol = 1,
    heights = unit.c(
      unit(0.4, "in"),
      sum(model_stats_body$heights)
    )
  )
  
  grid.arrange(model_stats_final)
}

## plot residual plots
plot_main_diagnostics <- function(model,
                                  caption = "Main residual diagnostic plots",
                                  ncol = 2) {
  library(ggplot2)
  library(dplyr)
  library(gridExtra)
  library(grid)

  diag_data <- model.frame(model) %>%
    mutate(
      .fitted = fitted(model),
      .resid = resid(model),
      .std_resid = rstandard(model)
    )

  plot_theme <- theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      plot.margin = margin(6, 6, 6, 6)
    )

  p1 <- ggplot(diag_data, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE, formula = y ~ x) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Residuals vs Fitted",
      x = "Fitted values",
      y = "Residuals"
    ) +
    plot_theme

  p2 <- ggplot(diag_data, aes(sample = .std_resid)) +
    stat_qq(alpha = 0.4) +
    stat_qq_line() +
    labs(
      title = "Normal Q-Q Plot",
      x = "Theoretical quantiles",
      y = "Standardized residuals"
    ) +
    plot_theme

  p3 <- ggplot(diag_data, aes(x = .fitted, y = .std_resid)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE, formula = y ~ x) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Fitted vs Standardized Residuals",
      x = "Fitted values",
      y = "Standardized residuals"
    ) +
    plot_theme

  p4 <- ggplot(diag_data, aes(x = .std_resid)) +
    geom_histogram(bins = 30, fill = "lightgray", color = "white") +
    coord_cartesian(xlim = c(-4, 4)) +
    labs(
      title = "Histogram of Standardized Residuals",
      x = "Standardized residuals",
      y = "Count"
    ) +
    plot_theme

  plot_grid <- arrangeGrob(
    p1, p2,
    p3, p4,
    ncol = ncol
  )

  caption_grob <- textGrob(
    caption,
    x = 0.5,
    hjust = 0.5,
    gp = gpar(fontface = "bold", fontsize = 12)
  )

  grid.arrange(
    plot_grid,
    caption_grob,
    ncol = 1,
    heights = c(1, 0.06)
  )
}

plot_residuals_vs_predictors <- function(model,
                                         data = NULL,
                                         caption = "Residuals versus predictors",
                                         ncol = 2,
                                         interaction_pairs = NULL,
                                         discrete_cutoff = 10,
                                         y_zoom = c(-4, 4),
                                         point_alpha = 0.25,
                                         point_size = 0.7) {
  library(ggplot2)
  library(dplyr)
  library(gridExtra)
  library(grid)

  mf <- model.frame(model)

  if (is.null(data)) {
    diag_data <- mf
  } else {
    diag_data <- data[rownames(mf), , drop = FALSE]
  }

  diag_data <- diag_data %>%
    mutate(
      .std_resid = rstandard(model)
    )

  response_vars <- all.vars(formula(model)[[2]])
  predictor_names <- setdiff(all.vars(formula(model)[[3]]), response_vars)

  plot_theme <- theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 9),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7),
      panel.grid.minor = element_blank(),
      plot.margin = margin(4, 4, 4, 4)
    )

  predictor_plots <- list()

  for (var in predictor_names) {
    x <- diag_data[[var]]
    x_label <- gsub("_", " ", var)

    is_discrete <- is.factor(x) ||
      is.character(x) ||
      is.logical(x) ||
      (is.numeric(x) && length(unique(x)) <= discrete_cutoff)

    if (is_discrete) {
      p <- ggplot(diag_data, aes(x = factor(.data[[var]]), y = .std_resid)) +
        geom_boxplot(outlier.shape = NA, linewidth = 0.3) +
        geom_jitter(
          width = 0.12,
          alpha = point_alpha,
          size = point_size
        ) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        coord_cartesian(ylim = y_zoom) +
        labs(
          title = paste("Residuals vs", x_label),
          x = x_label,
          y = "Standardized residuals"
        ) +
        plot_theme +
        theme(axis.text.x = element_text(angle = 15, hjust = 1))
    } else {
      p <- ggplot(diag_data, aes(x = .data[[var]], y = .std_resid)) +
        geom_point(
          alpha = point_alpha,
          size = point_size
        ) +
        geom_smooth(method = "loess", se = FALSE, formula = y ~ x, linewidth = 0.7) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        coord_cartesian(ylim = y_zoom) +
        labs(
          title = paste("Residuals vs", x_label),
          x = x_label,
          y = "Standardized residuals"
        ) +
        plot_theme
    }

    predictor_plots[[var]] <- p
  }

  interaction_plots <- list()

  if (!is.null(interaction_pairs)) {
    for (pair in interaction_pairs) {
      x_var <- pair[1]
      group_var <- pair[2]

      x_label <- gsub("_", " ", x_var)
      group_label <- gsub("_", " ", group_var)

      interaction_plots[[paste(x_var, group_var, sep = "_by_")]] <-
        ggplot(
          diag_data,
          aes(
            x = .data[[x_var]],
            y = .std_resid,
            color = factor(.data[[group_var]])
          )
        ) +
        geom_point(alpha = 0.22, size = point_size) +
        geom_smooth(method = "loess", se = FALSE, formula = y ~ x, linewidth = 0.7) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        coord_cartesian(ylim = y_zoom) +
        labs(
          title = paste("Residuals vs", x_label, "by", group_label),
          x = x_label,
          y = "Standardized residuals",
          color = group_label
        ) +
        plot_theme +
        theme(
          legend.position = "bottom",
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 7)
        )
    }
  }

  all_plots <- c(predictor_plots, interaction_plots)

  plot_grid <- arrangeGrob(
    grobs = all_plots,
    ncol = ncol
  )

  caption_grob <- textGrob(
    caption,
    x = 0.5,
    hjust = 0.5,
    gp = gpar(fontface = "bold", fontsize = 11)
  )

  grid.arrange(
    plot_grid,
    caption_grob,
    ncol = 1,
    heights = c(1, 0.05)
  )
}

plot_interaction_residuals <- function(model,
                                       data,
                                       x_var,
                                       group_var,
                                       caption = "Residuals by interaction term",
                                       y_zoom = c(-4, 4),
                                       x_zoom = NULL,
                                       smooth_method = "lm") {
  library(ggplot2)
  library(dplyr)
  library(gridExtra)
  library(grid)

  mf <- model.frame(model)

  diag_data <- data[rownames(mf), , drop = FALSE] %>%
    mutate(
      .std_resid = rstandard(model)
    )

  # Optional automatic x-axis zoom using 1st and 99th percentiles
  if (is.null(x_zoom)) {
    x_zoom <- quantile(
      diag_data[[x_var]],
      probs = c(0.01, 0.99),
      na.rm = TRUE
    )
  }

  x_label <- gsub("_", " ", x_var)
  group_label <- gsub("_", " ", group_var)

  interaction_plot <- ggplot(
    diag_data,
    aes(x = .data[[x_var]], y = .std_resid)
  ) +
    geom_point(alpha = 0.25, size = 0.7) +
    geom_smooth(
      method = smooth_method,
      se = FALSE,
      linewidth = 0.8
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_cartesian(
      xlim = x_zoom,
      ylim = y_zoom
    ) +
    facet_wrap(vars(.data[[group_var]]), nrow = 1) +
    labs(
      title = paste("Residuals vs", x_label, "by", group_label),
      x = x_label,
      y = "Standardized residuals"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      strip.text = element_text(face = "bold", size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      plot.margin = margin(6, 6, 6, 6)
    )

  caption_grob <- textGrob(
    caption,
    x = 0.5,
    hjust = 0.5,
    gp = gpar(fontface = "bold", fontsize = 11)
  )

  grid.arrange(
    interaction_plot,
    caption_grob,
    ncol = 1,
    heights = c(1, 0.06)
  )
}

boxcox_transform <- function(y, lambda) {
  gm_y <- exp(mean(log(y)))
  
  if (abs(lambda) < 0.001) {
    gm_y * log(y)
  } else {
    (gm_y^(1 - lambda)) * ((y^lambda - 1) / lambda)
  }
}


##split the data into 80% train, 20% test
set.seed(302)
n <- nrow(clean_ames)
train_index <- sample(1:n, size = 0.8*n)
clean_ames_train <- clean_ames[train_index,]
clean_ames_test <- clean_ames[-train_index,]
numeric_vars <- c(
  "Sale_Price",
  "log_sale_price",
  "Gr_Liv_Area",
  "Total_Bsmt_SF",
  "House_Age",
  "Full_Bath"
)

numeric_comparison <- bind_rows(
  clean_ames_train %>%
    dplyr::select(all_of(numeric_vars)) %>%
    mutate(Dataset = "Train"),

  clean_ames_test %>%
    dplyr::select(all_of(numeric_vars)) %>%
    mutate(Dataset = "Test")
) %>%
  pivot_longer(
    cols = all_of(numeric_vars),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  group_by(Dataset, Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Dataset,
    values_from = c(Mean, SD),
    names_glue = "{Dataset}_{.value}"
  ) %>%
  mutate(
    Pooled_SD = sqrt((Train_SD^2 + Test_SD^2) / 2),
    SMD = (Train_Mean - Test_Mean) / Pooled_SD,
    Variable = case_when(
      Variable == "Sale_Price" ~ "Sale price",
      Variable == "log_sale_price" ~ "Log price",
      Variable == "Gr_Liv_Area" ~ "Living area",
      Variable == "Total_Bsmt_SF" ~ "Basement area",
      Variable == "House_Age" ~ "House age",
      Variable == "Full_Bath" ~ "Full bath",
      TRUE ~ Variable
    )
  ) %>%
  dplyr::select(
    Variable,
    `Train Mean` = Train_Mean,
    `Test Mean` = Test_Mean,
    `Train SD` = Train_SD,
    `Test SD` = Test_SD,
    SMD
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 3))
  )

numeric_comparison_theme <- ttheme_default(
  core = list(
    fg_params = list(fontsize = 8),
    bg_params = list(
      fill = c("#F7F9FC", "white"),
      col = "#C7CED8",
      lwd = 0.6
    ),
    padding = unit(c(2, 2), "mm")
  ),
  colhead = list(
    fg_params = list(
      fontsize = 8,
      fontface = "bold",
      col = "white"
    ),
    bg_params = list(
      fill = "#3F5F8A",
      col = "#3F5F8A",
      lwd = 0.6
    ),
    padding = unit(c(2, 2), "mm")
  )
)

numeric_comparison_body <- tableGrob(
  numeric_comparison,
  rows = NULL,
  theme = numeric_comparison_theme
)

numeric_comparison_body$widths <- unit(
  c(1.5, 1, 1, 1, 1, 0.8),
  "null"
)

numeric_comparison_title <- textGrob(
  "Table 1.5 Comparison of Numerical Variables Between Training and Test Sets",
  gp = gpar(fontface = "bold", fontsize = 10)
)

numeric_comparison_final <- arrangeGrob(
  numeric_comparison_title,
  numeric_comparison_body,
  ncol = 1,
  heights = unit.c(
    unit(0.35, "in"),
    sum(numeric_comparison_body$heights)
  )
)

grid.arrange(numeric_comparison_final)

categorical_vars <- c(
  "Overall_Qual_Category",
  "Fireplace_Category"
)

categorical_comparison <- bind_rows(
  clean_ames_train %>%
    dplyr::select(all_of(categorical_vars)) %>%
    mutate(Dataset = "Train"),

  clean_ames_test %>%
    dplyr::select(all_of(categorical_vars)) %>%
    mutate(Dataset = "Test")
) %>%
  pivot_longer(
    cols = all_of(categorical_vars),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  group_by(Dataset, Variable, Category) %>%
  summarise(
    Count = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    Percent = Count / sum(Count) * 100
  ) %>%
  ungroup() %>%
  dplyr::select(Dataset, Variable, Category, Count, Percent) %>%
  pivot_wider(
    names_from = Dataset,
    values_from = c(Count, Percent),
    names_glue = "{Dataset}_{.value}",
    values_fill = 0
  ) %>%
  mutate(
    `Diff (%)` = Train_Percent - Test_Percent,

    Variable = case_when(
      Variable == "Overall_Qual_Category" ~ "Quality",
      Variable == "Fireplace_Category" ~ "Fireplace",
      TRUE ~ Variable
    ),

    Category = case_when(
      Category == "0 fireplaces" ~ "0",
      Category == "1 fireplace" ~ "1",
      Category == "2 or more fireplaces" ~ "2+",
      TRUE ~ as.character(Category)
    ),

    `Train n (%)` = paste0(
      Train_Count, " (", round(Train_Percent, 1), "%)"
    ),

    `Test n (%)` = paste0(
      Test_Count, " (", round(Test_Percent, 1), "%)"
    ),

    `Diff (%)` = round(`Diff (%)`, 1)
  ) %>%
  dplyr::select(
    Variable,
    Category,
    `Train n (%)`,
    `Test n (%)`,
    `Diff (%)`
  )

categorical_comparison_theme <- ttheme_default(
  core = list(
    fg_params = list(fontsize = 8),
    bg_params = list(
      fill = c("#F7F9FC", "white"),
      col = "#C7CED8",
      lwd = 0.6
    ),
    padding = unit(c(2, 2), "mm")
  ),
  colhead = list(
    fg_params = list(
      fontsize = 8,
      fontface = "bold",
      col = "white"
    ),
    bg_params = list(
      fill = "#3F5F8A",
      col = "#3F5F8A",
      lwd = 0.6
    ),
    padding = unit(c(2, 2), "mm")
  )
)

categorical_comparison_body <- tableGrob(
  categorical_comparison,
  rows = NULL,
  theme = categorical_comparison_theme
)

categorical_comparison_body$widths <- unit(
  c(1.1, 1.2, 1.4, 1.4, 0.9),
  "null"
)

categorical_comparison_title <- textGrob(
  "Table 1.6 Comparison of Categorical Variables Between Training and Test Sets",
  gp = gpar(fontface = "bold", fontsize = 10)
)

categorical_comparison_final <- arrangeGrob(
  categorical_comparison_title,
  categorical_comparison_body,
  ncol = 1,
  heights = unit.c(
    unit(0.35, "in"),
    sum(categorical_comparison_body$heights)
  )
)

grid.arrange(categorical_comparison_final)

# Fit linear regression model
model1 <- lm(
  log_sale_price ~
    Gr_Liv_Area +
    Total_Bsmt_SF +
    House_Age +
    Overall_Qual_Category +
    Fireplace_Category +
    Full_Bath,
  data = clean_ames_train
)

# Create regression coefficient table
make_regression_table(model1, "Table 1.7 Baseline Model Coefficients")

single_model_stats_table(model1, "Table 1.7.1 Model 1 Fit Statistics")

n <- nrow(clean_ames_train)
p <- length(coef(model1))

outlier_cutoff <- 4
cook_cutoff <- 4 / n
leverage_cutoff <- 2 * (p + 1) / n

diagnostic_points <- clean_ames_train %>%
  mutate(
    row_id = row_number(),
    std_resid = rstandard(model1),
    cooks_d = cooks.distance(model1),
    leverage = hatvalues(model1),
    
    is_outlier = abs(std_resid) > outlier_cutoff,
    is_influential = cooks_d > cook_cutoff,
    is_leverage = leverage > leverage_cutoff
  )

diagnostic_counts <- diagnostic_points %>%
  summarise(
    number_of_outliers = sum(is_outlier, na.rm = TRUE),
    number_of_influential_points = sum(is_influential, na.rm = TRUE),
    number_of_leverage_points = sum(is_leverage, na.rm = TRUE),
    number_of_any_problem_points = sum(
      is_outlier | is_influential | is_leverage,
      na.rm = TRUE
    )
  )


clean_ames_train$std_resid <- rstandard(model1)
clean_ames_train$cooks_d <- cooks.distance(model1)
clean_ames_train$leverage <- hatvalues(model1)

problem_points1 <- clean_ames_train %>%
  mutate(row_id = row_number()) %>%
  filter(abs(std_resid) > 4 | cooks_d > 4 / nrow(clean_ames_train))


plot_main_diagnostics(model1, caption = "Figure 1.5 Residual plots and QQ plot for model 1")

plot_residuals_vs_predictors(model1 ,caption ="Figure 1.6 Residuals V.S predictors for model 1")

rows_to_remove1 <- problem_points1$row_id

clean_ames_train_removed1 <- clean_ames_train %>%
  mutate(row_id = row_number()) %>%
  filter(!row_id %in% rows_to_remove1)

model_removed1 <- lm(
  log_sale_price ~
    Gr_Liv_Area +
    Total_Bsmt_SF +
    House_Age +
    Overall_Qual_Category +
    Fireplace_Category+
    Full_Bath,
  data = clean_ames_train_removed1
)
make_regression_table(model_removed1,"Table 1.8 Coefficients of Baseline Model without extreme points")

single_model_stats_table(model_removed1, "Table 1.8.1 Model 1 without extreme points fit Statistics")


plot_main_diagnostics(model_removed1, caption = "Figure 1.7  Residual plots and QQ plot for model 1 without extreme points")

plot_residuals_vs_predictors(model_removed1 ,caption ="Figure 1.8 Residuals V.S predictors for model 1 without extreme points")

vif_values <- car::vif(model_removed1)

vif_table <- as.data.frame(vif_values) %>%
  rownames_to_column(var = "Predictor")

# If model has categorical predictors, car::vif() returns GVIF
if ("GVIF" %in% names(vif_table)) {
  vif_table <- vif_table %>%
    mutate(
      `GVIF^(1/(2*Df))` = round(`GVIF^(1/(2*Df))`, 3),
      GVIF = round(GVIF, 3)
    )
} else {
  vif_table <- vif_table %>%
    rename(VIF = vif_values) %>%
    mutate(VIF = round(VIF, 3))
}

vif_theme <- ttheme_default(
  core = list(
    fg_params = list(fontsize = 12),
    bg_params = list(
      fill = c("#F7F9FC", "white"),
      col = "#C7CED8",
      lwd = 0.8
    ),
    padding = unit(c(4, 4), "mm")
  ),
  colhead = list(
    fg_params = list(
      fontsize = 12,
      fontface = "bold",
      col = "white"
    ),
    bg_params = list(
      fill = "#3F5F8A",
      col = "#3F5F8A",
      lwd = 0.8
    ),
    padding = unit(c(4, 4), "mm")
  )
)

vif_table_body <- tableGrob(
  vif_table,
  rows = NULL,
  theme = vif_theme
)

vif_table_title <- textGrob(
  "Table 1.9 VIF Values for Model 1 without extreme points",
  gp = gpar(fontface = "bold", fontsize = 10)
)

vif_table_final <- arrangeGrob(
  vif_table_title,
  vif_table_body,
  ncol = 1,
  heights = unit.c(
    unit(0.4, "in"),
    sum(vif_table_body$heights)
  )
)

grid.arrange(vif_table_final)

model2 <- lm(
  log_sale_price ~
    Gr_Liv_Area +
    Total_Bsmt_SF +
    House_Age +
    Overall_Qual_Category +
    Fireplace_Category,
  data = clean_ames_train_removed1
)
make_regression_table(model2,"Table 2.0 Model 2 Coefficients")

single_model_stats_table(model2, "Table 2.0.1 Model 2 Fit Statistics")

plot_main_diagnostics(model2, caption = "Figure 1.9 Residual plots and QQ plot for model 2")

plot_residuals_vs_predictors(model2 ,caption ="Figure 2.0 Residuals V.S predictors for model 1")

library(ggplot2)
library(patchwork)
library(stringr)

plot_data <- clean_ames_train_removed1

# Zoom x-axis for numerical plots using 1st and 99th percentiles
gr_xlim <- quantile(plot_data$Gr_Liv_Area, probs = c(0, 1), na.rm = TRUE)
bsmt_xlim <- quantile(plot_data$Total_Bsmt_SF, probs = c(0, 1), na.rm = TRUE)
age_xlim <- quantile(plot_data$House_Age, probs = c(0, 1), na.rm = TRUE)

scatter_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    plot.margin = margin(6, 6, 6, 6)
  )

# 1. log_sale_price vs Gr_Liv_Area
p1 <- ggplot(plot_data, aes(x = Gr_Liv_Area, y = log_sale_price)) +
  geom_point(alpha = 0.5, size = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  coord_cartesian(xlim = gr_xlim) +
  labs(
    x = "Above-ground living area",
    y = "Log sale price",
    title = "Living Area"
  ) +
  scatter_theme

# 2. log_sale_price vs Total_Bsmt_SF
p2 <- ggplot(plot_data, aes(x = Total_Bsmt_SF, y = log_sale_price)) +
  geom_point(alpha = 0.5, size = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  coord_cartesian(xlim = bsmt_xlim) +
  labs(
    x = "Total basement area",
    y = "Log sale price",
    title = "Basement Area"
  ) +
  scatter_theme

# 3. log_sale_price vs House_Age
p3 <- ggplot(plot_data, aes(x = House_Age, y = log_sale_price)) +
  geom_point(alpha = 0.5, size = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  coord_cartesian(xlim = age_xlim) +
  labs(
    x = "House age",
    y = "Log sale price",
    title = "House Age"
  ) +
  scatter_theme

# 4. log_sale_price vs Overall_Qual_Category
p4 <- ggplot(plot_data, aes(x = Overall_Qual_Category, y = log_sale_price)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.4) +
  geom_jitter(width = 0.15, alpha = 5, size = 0.6) +
  labs(
    x = "Overall quality category",
    y = "Log sale price",
    title = "Overall Quality"
  ) +
  scatter_theme

# 5. log_sale_price vs Fireplace_Category
p5 <- ggplot(plot_data, aes(x = Fireplace_Category, y = log_sale_price)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.4) +
  geom_jitter(width = 0.15, alpha = 5, size = 0.6) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(
    x = "Fireplace category",
    y = "Log sale price",
    title = "Fireplace Category"
  ) +
  scatter_theme

# 6. log_sale_price vs Full_Bath
p6 <- ggplot(plot_data, aes(x = factor(Full_Bath), y = log_sale_price)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.4) +
  geom_jitter(width = 0.15, alpha = 5, size = 0.6) +
  labs(
    x = "Number of full bathrooms",
    y = "Log sale price",
    title = "Full Bathrooms"
  ) +
  scatter_theme

# Combine plots
(p1 + p2) / (p3 + p4) / (p5 + p6) +
  plot_annotation(
    caption = "Figure 2.1 Log Sale Price versus Predictors",
    theme = theme(
      plot.caption = element_text(
        hjust = 0.5,
        face = "bold",
        size = 12,
        margin = margin(t = 8)
      )
    )
  )

## transformation on House Age due to the residual plot and the scatter plot of y VS house age
model3 <- lm(
  log_sale_price ~
    Gr_Liv_Area +
    Total_Bsmt_SF +
    House_Age +
    I(House_Age^2)+
    Overall_Qual_Category +
    Fireplace_Category,
  data = clean_ames_train_removed1
)

make_regression_table(model3, "Table 2.1 Model 3 Coefficients")

single_model_stats_table(model3, "Table 2.1.1 Model 3 Fit Statistics")

plot_main_diagnostics(model3, caption = "Figure 2.2 Residual plots and QQ plot for model 3")

plot_residuals_vs_predictors(model3,
                             caption ="Figure 2.3 Residuals V.S predictors for model 3"
                            )


# get lambda value of Box-Cox transformation
boxcox_base_model <- lm(
  Sale_Price ~
    Gr_Liv_Area +
    Total_Bsmt_SF +
    House_Age +
    I(House_Age^2) +
    Overall_Qual_Category +
    Fireplace_Category,
  data = clean_ames_train_removed1
)

boxcox_result <- MASS::boxcox(
  boxcox_base_model,
  lambda = seq(-2, 2, by = 0.01),
  xlab = expression(lambda)
)

lambda_hat <- boxcox_result$x[which.max(boxcox_result$y)]

# add vertical line
abline(v = lambda_hat, col = "red", lty = 2, lwd = 2)

# add label
text(
  x = lambda_hat,
  y = max(boxcox_result$y) - 20,
  labels = bquote(lambda == .(round(lambda_hat, 2))),
  col = "red",
  pos = 4,
  cex = 1
)

# add box-cox response in the training dataset
clean_ames_train_removed1 <- clean_ames_train_removed1 %>%
  mutate(
    bc_sale_price = boxcox_transform(Sale_Price, lambda_hat)
  )

clean_ames_test <- clean_ames_test %>%
  mutate(
    bc_sale_price = boxcox_transform(Sale_Price, lambda_hat)
  )

# fit the model with box-coxed response
model3_boxcox_y <- lm(
  bc_sale_price ~
    Gr_Liv_Area +
    Total_Bsmt_SF +
    House_Age +
    I(House_Age^2) +
    Overall_Qual_Category +
    Fireplace_Category,
  data = clean_ames_train_removed1
)
make_regression_table(model3_boxcox_y,"Table 2.2 Model 4 Coefficients")

single_model_stats_table(model3_boxcox_y, "Table 2.2.1 Model 4 Fit Statistics")

plot_main_diagnostics(
  model3_boxcox_y,
  caption = "Figure 2.4 Residual plots and QQ plot for model 2"
)

plot_residuals_vs_predictors(model3_boxcox_y, caption = "Figure 2.5 Box-Cox transformed model 3 vs predictors")

model5 <- lm(
  log_sale_price ~
    Gr_Liv_Area +
    Total_Bsmt_SF +
    House_Age +
    I(House_Age^2) +
    Overall_Qual_Category +
    Fireplace_Category +
    Gr_Liv_Area*Overall_Qual_Category,
  data = clean_ames_train_removed1
)

make_regression_table(model5, "Table 2.3 model 5 Coefficients")

## Back ward BIC with all the added predictors so far in the baseline model
baseline_model <- lm(
  log_sale_price ~
    Gr_Liv_Area +
    Total_Bsmt_SF +
    House_Age +
    I(House_Age^2) +
    Overall_Qual_Category +
    Fireplace_Category +
    Full_Bath +
    Gr_Liv_Area*Overall_Qual_Category,
  data = clean_ames_train_removed1
)

backward_model <- step(
  baseline_model,
  direction = "backward",
  k = log(nrow(clean_ames_train_removed1)),
  trace = 0
)
make_regression_table(backward_model, "Table 2.4 model 6 Coefficients")

single_model_stats_table(model5,"Table 2.4.1 Model 5 & 6 Fit Statistics")

plot_main_diagnostics(model5, caption = "Figure 2.6 Residual plots and QQ plot for model 5 & 6")

plot_residuals_vs_predictors(model5,
                             caption ="Figure 2.7 Residuals V.S predictors for model 5 & 6"
                            )

plot_interaction_residuals(
  model5,
  data = clean_ames_train_removed1,
  x_var = "Gr_Liv_Area",
  group_var = "Overall_Qual_Category",
  caption = "Figure 2.8 Standardized Residuals versus Living Area by Overall Quality for Model 5 & 6"
)

# Summary of all the criteria 
make_model_comparison_table(
  model_removed1,
  model2,
  model3,
  model3_boxcox_y,
  model5,
  backward_model,
  model_names = c(
    "Model 1",
    "Model 2",
    "Model 3",
    "Model 4",
    "Model 5",
    "Model 6"
  ),
  title_text = "Table 2.5 Model Comparison Statistics"
)

# Summary table of all the flagged points for each model
model_list <- list(
  "Model 1" = model_removed1,
  "Model 2" = model2,
  "Model 3" = model3,
  "Model 4" = model3_boxcox_y,
  "Model 5" = model5,
  "Model 6" = backward_model
)
point_diagnostic_table <- bind_rows(
  lapply(names(model_list), function(model_name) {
    model <- model_list[[model_name]]
    
    n <- nobs(model)
    p <- length(coef(model))
    
    std_resid <- rstandard(model)
    cooks_d <- cooks.distance(model)
    leverage <- hatvalues(model)
    
    outlier_cutoff <- 4
    cook_cutoff <- 4 / n
    leverage_cutoff <- 2 * (p + 1) / n
    
    is_outlier <- abs(std_resid) > outlier_cutoff
    is_influential <- cooks_d > cook_cutoff
    is_leverage <- leverage > leverage_cutoff
    
    tibble(
      Model = model_name,
      Outliers = sum(is_outlier, na.rm = TRUE),
      `Influential points` = sum(is_influential, na.rm = TRUE),
      `Leverage points` = sum(is_leverage, na.rm = TRUE),
      `Any flagged points` = sum(
        is_outlier | is_influential | is_leverage,
        na.rm = TRUE
      )
    )
  })
)

point_diagnostic_theme <- ttheme_default(
  core = list(
    fg_params = list(fontsize = 11),
    bg_params = list(
      fill = c("#F7F9FC", "white"),
      col = "#C7CED8",
      lwd = 0.8
    ),
    padding = unit(c(4, 4), "mm")
  ),
  colhead = list(
    fg_params = list(
      fontsize = 11,
      fontface = "bold",
      col = "white"
    ),
    bg_params = list(
      fill = "#3F5F8A",
      col = "#3F5F8A",
      lwd = 0.8
    ),
    padding = unit(c(4, 4), "mm")
  )
)

point_diagnostic_table_body <- tableGrob(
  point_diagnostic_table,
  rows = NULL,
  theme = point_diagnostic_theme
)

point_diagnostic_title <- textGrob(
  "Table 2.6 Number of Flagged Observations by Model",
  gp = gpar(fontface = "bold", fontsize = 10)
)

point_diagnostic_table_final <- arrangeGrob(
  point_diagnostic_title,
  point_diagnostic_table_body,
  ncol = 1,
  heights = unit.c(
    unit(0.4, "in"),
    sum(point_diagnostic_table_body$heights)
  )
)

grid.arrange(point_diagnostic_table_final)

library(broom)
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

coef_table <- tidy(
  model3,
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  dplyr::select(
    term,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value
  ) %>%
  mutate(
    term = c(
      "Intercept",
      "Above-ground living area",
      "Total basement area",
      "House age",
      "House age squared",
      "Quality: Average vs Bad",
      "Quality: Good vs Bad",
      "Fireplace: 1 vs 0",
      "Fireplace: 2+ vs 0"
    ),
    estimate = round(estimate, 6),
    std.error = round(std.error, 6),
    conf.low = round(conf.low, 6),
    conf.high = round(conf.high, 6),
    p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  rename(
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `p value` = p.value
  )

ttheme <- ttheme_minimal(
  core = list(
    fg_params = list(cex = 0.9),
    bg_params = list(fill = "white",
                     col = "#C8CDD5")
  ),
  colhead = list(
    fg_params = list(col = "white",
                     fontface = "bold",
                     cex = 0.9),
    bg_params = list(fill = "#3F5F8A",
                     col = "#3F5F8A")
  )
)

tbl <- tableGrob(
  coef_table,
  rows = NULL,
  theme = ttheme
)

caption <- textGrob(
  "Table 2.7 Final Model Coefficients",
  gp = gpar(fontface = "bold", fontsize = 12),
  hjust = 0.5,
  x = 0.5
)

# Add one small row above the table for caption
tbl_with_caption <- gtable_add_rows(
  tbl,
  heights = grobHeight(caption) + unit(4, "mm"),
  pos = 0
)

# Put caption in that row, spanning the full table width
tbl_with_caption <- gtable_add_grob(
  tbl_with_caption,
  caption,
  t = 1,
  l = 1,
  r = ncol(tbl_with_caption)
)

grid.newpage()
grid.draw(tbl_with_caption)

# Predict test-set log sale prices using model3 fitted on training data
model3_test_pred <- predict(model3, newdata = clean_ames_test)

# Create test results
model3_test_results <- clean_ames_test %>%
  mutate(
    predicted_log_sale_price = model3_test_pred,
    test_residual = log_sale_price - predicted_log_sale_price
  )

# Test-set performance metrics
model3_test_rmse <- sqrt(mean(model3_test_results$test_residual^2))
model3_test_mae <- mean(abs(model3_test_results$test_residual))

model3_test_r2 <- 1 -
  sum(model3_test_results$test_residual^2) /
  sum((model3_test_results$log_sale_price - mean(model3_test_results$log_sale_price))^2)


validation_results <- tibble(
  Metric = c(
    "Test RMSE",
    "Test MAE",
    "Test R-squared"
  ),
  Value = c(
    model3_test_rmse,
    model3_test_mae,
    model3_test_r2
  )
) %>%
  mutate(Value = sprintf("%.4f", Value))

validation_theme <- ttheme_default(
  core = list(
    fg_params = list(fontsize = 10),
    bg_params = list(
      fill = c("#F7F9FC", "white"),
      col = "#C7CED8",
      lwd = 0.8
    ),
    padding = unit(c(3, 3), "mm")
  ),
  colhead = list(
    fg_params = list(
      fontsize = 10,
      fontface = "bold",
      col = "white"
    ),
    bg_params = list(
      fill = "#3F5F8A",
      col = "#3F5F8A",
      lwd = 0.8
    ),
    padding = unit(c(3, 3), "mm")
  )
)

validation_table_body <- tableGrob(
  validation_results,
  rows = NULL,
  theme = validation_theme
)

validation_title <- textGrob(
  "Table 2.6 Test Set Validation Results for Model 3",
  gp = gpar(fontface = "bold", fontsize = 10)
)

validation_table_final <- arrangeGrob(
  validation_title,
  validation_table_body,
  ncol = 1,
  heights = unit.c(
    unit(0.35, "in"),
    sum(validation_table_body$heights)
  )
)

grid.arrange(validation_table_final)

library(patchwork)

p1 <- ggplot(model3_test_results, aes(x = predicted_log_sale_price, y = log_sale_price)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  theme_minimal() +
  labs(
    x = "Predicted log sale price",
    y = "Observed log sale price",
    title = "Observed vs Predicted"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11)
  )

p2 <- ggplot(model3_test_results, aes(x = predicted_log_sale_price, y = test_residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE) +
  theme_minimal() +
  labs(
    x = "Predicted log sale price",
    y = "Test residual",
    title = "Test Residuals"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11)
  )

p1 + p2 +
  plot_annotation(
    caption = "Figure 2.9 Model 3 Test Set Prediction and Residual Plots",
    theme = theme(
      plot.caption = element_text(
        hjust = 0.5,
        face = "bold",
        size = 10,
        margin = margin(t = 8)
      )
    )
  )
