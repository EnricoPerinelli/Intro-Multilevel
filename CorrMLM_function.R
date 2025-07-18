CorrMLM <- function(data, cluster_var) {
  
  if (!requireNamespace("psych", quietly = TRUE)) stop("Package 'psych' is required.")
  library(psych)
  
  # Make sure the cluster variable is the first column
  merged_data <- data[, c(cluster_var, setdiff(names(data), cluster_var))]
  
  # Compute multilevel correlations
  my_corr <- psych::statsBy(merged_data, group = merged_data[[cluster_var]])
  
  # Extract correlation matrices and p-values
  corr_btw_group    <- round(my_corr$rbg, 2)
  corr_within_group <- round(my_corr$rwg, 2)
  p_btw_group       <- round(my_corr$pbg, 3)
  p_within_group    <- round(my_corr$pwg, 3)
  
  # Convert p-values into APA-style significance stars
  stars_p_btw_group <- ifelse(
    p_btw_group < .001, "***",
    ifelse(
      p_btw_group < .01, "** ",
      ifelse(p_btw_group < .05, "*   ", "   ")))
  
  stars_p_within_group <- ifelse(
    p_within_group < .001, "***",
    ifelse(
      p_within_group < .01, "**  ",
      ifelse(p_within_group < .05, "*   ", "   ")))
  
  # Hide upper triangle for between-group correlations
  upper_corr_btw_group <- corr_btw_group
  upper_corr_btw_group[upper.tri(upper_corr_btw_group)] <- ""
  
  # Hide lower triangle (incl. diagonal) for within-group correlations
  lower_corr_within_group <- corr_within_group
  lower_corr_within_group[lower.tri(lower_corr_within_group, diag = TRUE)] <- ""
  
  # Mask p-value triangles accordingly
  upper_stars_p_btw_group <- stars_p_btw_group
  upper_stars_p_btw_group[upper.tri(upper_stars_p_btw_group, diag = TRUE)] <- ""
  
  lower_stars_p_within_group <- stars_p_within_group
  lower_stars_p_within_group[lower.tri(lower_stars_p_within_group, diag = TRUE)] <- ""
  
  # Combine correlation values with APA stars
  Rlow <- matrix(paste(upper_corr_btw_group, upper_stars_p_btw_group, sep = ""), 
                 ncol = ncol(corr_btw_group))
  Rup  <- matrix(paste(lower_corr_within_group, lower_stars_p_within_group, sep = ""), 
                 ncol = ncol(corr_within_group))
  
  # Merge lower (between) and upper (within)
  R_final <- matrix(paste(Rlow, Rup, sep = ""), ncol = ncol(corr_btw_group))
  R_final <- as.data.frame(R_final)
  
  # Assign numbered column names
  n_vars <- ncol(corr_btw_group)
  colnames(R_final) <- as.character(1:n_vars)
  
  # Build the first column: "1 variable_name"
  var_names <- rownames(corr_btw_group)
  row_labels <- paste0(seq_along(var_names), " ", var_names)
  
  # Add first column to the data frame
  R_final <- cbind(Variable = row_labels, R_final)
  
  # Replace artifacts
  R_final[R_final == "NANA"] <- "-"
  
  cat("\nNOTE:\n",
      "  Between-group correlations are shown in the LOWER triangle\n",
      "  Within-group correlations are shown in the UPPER triangle\n",
      "  '-' indicates no within-group variation (e.g., SD = 0)\n\n")
  
  return(R_final)
}


# Example usage:
CorrMLM(
  data = my_data[, c("var1", "var2", "var3")],
  cluster_var = "id"
)




# library(tidyverse)
# 
# set.seed(123)
# 
# # Parameters
# n_teams <- 30
# n_individuals_per_team <- 10
# n_individuals <- n_teams * n_individuals_per_team
# 
# # Create team-level data (Level 2)
# team_data <- data.frame(
#   team_id = 1:n_teams,
#   team_climate = rnorm(n_teams, mean = 3.5, sd = 0.5),
#   manager_experience = sample(1:20, n_teams, replace = TRUE)
# )
# 
# # Expand to individual-level data (Level 1)
# data <- team_data %>%
#   slice(rep(1:n(), each = n_individuals_per_team)) %>%
#   mutate(individual_id = 1:n_individuals) %>%
#   arrange(team_id)
# 
# # Simulate within-level predictors
# data <- data %>%
#   mutate(
#     hours_worked = rnorm(n_individuals, mean = 8, sd = 1.5),
#     perceived_stress = 0.4 * hours_worked + rnorm(n_individuals, mean = 0, sd = 1),
#     daily_mood = 5 +
#       0.3 * team_climate -
#       0.2 * perceived_stress +
#       rnorm(n_individuals, mean = 0, sd = 1)
#   )
# 
# 
# CorrMLM(
#   data = data,
#   cluster_var = "team_id"
# )
