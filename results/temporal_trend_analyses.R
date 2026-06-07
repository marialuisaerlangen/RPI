# LIBRARIES

library(tidyverse)
library(patchwork)

# WORKING DIRECTORY

setwd("C:/Users/leric/Documents/Masterpal_2/researchproject/results/")

# SAMPLES

samples <- c("100", "106", "120", "123", "141", "145")

# LISTS

results_list <- list()
model_list <- list()
plot_list <- list()

# LOOP


for(s in samples){
  

 # LOAD DATA
  
  years <- read.csv(
    paste0(s, "/years", s, ".csv"),
    sep = ";"
  )
  
  cements <- read.csv(
    paste0(s, "/cements", s, ".csv"),
    sep = ";"
  )
  
  # CONVERT TO NUMERIC
  
  years$Area <- as.numeric(
    gsub(",", ".", years$Area)
  )
  
  cements$Area <- as.numeric(
    gsub(",", ".", cements$Area)
  )

  # SUMMARY OF CEMENTS PER YEAR
  
  cement_summary <- cements %>%
    group_by(Year) %>%
    summarise(
      
      n_cements = n(),
      
      cement_area_sum =
        sum(Area, na.rm = TRUE),
      
      cement_area_mean =
        mean(Area, na.rm = TRUE)
    )
  
  # JOIN DATA
  
  data_combined <- years %>%
    left_join(cement_summary, by = "Year")

  # REPLACE NA WITH 0
  
  data_combined <- data_combined %>%
    mutate(
      
      across(
        c(
          n_cements,
          cement_area_sum,
          cement_area_mean
        ),
        ~replace_na(., 0)
      )
    )
  
  # CALCULATE RATIO

  data_combined <- data_combined %>%
    mutate(
      
      cement_ratio =
        cement_area_sum / Area,
      
      Sample = s
    )
  
  # SAVE DATA
  
  results_list[[s]] <- data_combined

  
  # LINEAR MODEL

  
  model <- lm(
    cement_area_sum ~ Area,
    data = data_combined
  )
  
  model_list[[s]] <- model
  
  # SCATTERPLOT

  p1 <- ggplot(
    data_combined,
    aes(
      x = Area,
      y = cement_area_sum
    )
  ) +
    geom_point(size = 2) +
    geom_smooth(method = "lm") +
    ggtitle(paste("Sample", s)) +
    theme_minimal()
  

  # RATIO PLOT

  p2 <- ggplot(
    data_combined,
    aes(
      x = Year,
      y = cement_ratio
    )
  ) +
    geom_line() +
    geom_point() +
    ggtitle(paste("Ratio", s)) +
    theme_minimal()

  # SAVE PLOTS
  
  plot_list[[paste0(s, "_scatter")]] <- p1
  plot_list[[paste0(s, "_ratio")]] <- p2
}

# COMBINE ALL DATA

all_data <- bind_rows(results_list)


# STATISTICAL TESTS
# SHAPIRO-WILK TESTS

shapiro_results <- list()

for(s in samples){
  
  shapiro_results[[s]] <-
    shapiro.test(
      results_list[[s]]$cement_ratio
    )
  
}
 print(shapiro_results)

# KRUSKAL-WALLIS TEST

kruskal_results <- kruskal.test(
  cement_ratio ~ Sample,
  data = all_data
)
print(kruskal_results)

# PAIRWISE WILCOXON

wilcox_results <- pairwise.wilcox.test(
  all_data$cement_ratio,
  all_data$Sample,
  p.adjust.method = "BH"
)
print(wilcox_results)

# LINEAR MODELS

year_models <- list()
year_summaries <- list()
year_plots <- list()

for(s in samples){
  
  model <- lm(
    cement_ratio ~ Year,
    data = results_list[[s]]
  )
  
  year_models[[s]] <- model
  
  year_summaries[[s]] <- summary(model)
  
  p <- ggplot(
    results_list[[s]],
    aes(
      x = Year,
      y = cement_ratio
    )
  ) +
    geom_point(size = 2) +
    geom_smooth(
      method = "lm",
      se = TRUE
    ) +
    labs(
      title = paste("Sample", s),
      x = "Year",
      y = expression(
        Cement~Ratio~
          (mm^2/mm^2)
      )
    ) +
    theme_minimal()
  
  year_plots[[s]] <- p
}

# COMBINED SCATTER AND LINEAR PLOTS ####

combined_plots <- list()

for(s in samples){
  
  scatter_plot <- plot_list[[paste0(s, "_scatter")]]
  
  trend_plot <- year_plots[[s]]
  
  combined_plots[[s]] <-
    scatter_plot +
    trend_plot +
    plot_layout(ncol = 2)
  
}
# COMBINED MODEL

combined_model <- lm(
  cement_ratio ~ Year * Sample,
  data = all_data
)

combined_summary <- summary(
  combined_model
)

combined_anova <- anova(
  combined_model
)


# MEAN DATA EXCLUDING 145

mean_data <- all_data %>%
  filter(Sample != "145") %>%
  group_by(Year) %>%
  summarise(
    mean_ratio =
      mean(
        cement_ratio,
        na.rm = TRUE
      )
  )

mean_model <- lm(
  mean_ratio ~ Year,
  data = mean_data
)

mean_summary <- summary(mean_model)

# PLOTTING MEAN SUMMARY

mean_plot <- ggplot(
  mean_data,
  aes(
    x = Year,
    y = mean_ratio
  )
) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title =
      "Mean Cement Ratio (without Sample 145)",
    x = "Year",
    y = expression(
      Mean~Cement~Ratio~
        (mm^2/mm^2)
    )
  ) +
  theme_minimal()

#########
# PDF WITH LINEAR MODELS

pdf(
  "linear_models.pdf",
  width = 10,
  height = 8
)

for(p in year_plots){
  
  print(p)
  
}

print(mean_plot)

dev.off()

# PDF WITH SCATTERPLOT LINEAR PLOT

pdf(
  "scatter_and_trend_plots.pdf",
  width = 14,
  height = 7
)

for(p in combined_plots){
  
  print(p)
  
}

print(mean_plot)

dev.off()
# PRINTING STATISTCAL RESULTS

sink("STATISTICAL_RESULTS.txt")

cat("====================================\n")
cat("SHAPIRO-WILK TESTS\n")
cat("====================================\n\n")
print(shapiro_results)

cat("\n\n")
cat("KRUSKAL-WALLIS TEST\n")
cat("====================================\n\n")
print(kruskal_results)

cat("\n\n")
cat("PAIRWISE WILCOXON TESTS\n")
cat("====================================\n\n")
print(wilcox_results)

cat("\n\n")
cat("LINEAR MODELS\n")
cat("====================================\n\n")
print(year_summaries)

cat("\n\n")
cat("COMBINED MODEL\n")
cat("====================================\n\n")
print(combined_summary)

cat("\n\n")
print(combined_anova)

cat("\n\n")
cat("MEAN MODEL\n")
cat("====================================\n\n")
print(mean_summary)

sink()

