#### [0] prep ####
library(tidyverse)
library(tidytext)

# country-level metrics
metrics_df <- read_csv("output/nutrient_metrics_all.csv")
nut_percap_df <- read_csv("output/nutrient_percapita_all.csv")
rni_perc_df <- read_csv("output/nutrient_rni_percpop_all.csv")
# nutrient-level metrics
nutrient_df <- read_csv("output/nutrient_stats_all.csv")


#### [1] plot country-level overall nutrient metrics ####
# rearrange for facetwrap
metrics_long <- metrics_df %>%
  pivot_longer(colnames(metrics_df)[2:5],
               names_to = "Metric",values_to = "Value") %>%
  group_by(Metric) %>% arrange(-Value) %>%
  mutate(pos_neg = ifelse(Value>0,"Positive","Negative"))

## plot average fulfillment
avg_df_all <- metrics_long %>%
  filter(Metric == "rni_avg_zscore") %>%
  arrange(desc(Value)) %>%          
  slice(c(1:15, (n()-14):n()))

ggplot(avg_df_all,
       aes(x = reorder(Country, Value), y = Value, fill = pos_neg)) +
  geom_bar(stat = "identity") +
  facet_wrap(~pos_neg, scales = "free_x") +
  scale_y_continuous(
    limits = c(-10, 1),labels = seq(-10,1,1), 
    breaks = seq(-10,1,1),oob = scales::squish                   
  )+
  labs(
    title = "Highest and Lowest Average RNI Fulfillments",
    subtitle ="Normalized Across All 7 Nutrients",
    y = "Average Z-Score",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust=0.5),
    strip.text = element_text(size = 12)  
  ) +
  guides(fill = "none")
# ggsave("output/rni_allnutrients.jpg")

#### [2] plot country-level nutrient-specific metrics - # of RNIs ####
nut_percap_plot <- nut_percap_df %>%
  group_by(category) %>%
  arrange(desc(rni_ratio)) %>%            
  slice(c(1:5, (n() - 4):n()))

nut_percap_plot_zscore <- nut_percap_df %>%
  group_by(category) %>%
  arrange(desc(rni_ratio_zscore)) %>%            
  slice(c(1:5, (n() - 4):n()))

# facet_wrap style
ggplot(nut_percap_plot, 
       aes(x = reorder_within(Country, rni_ratio,category),
           y = rni_ratio, fill = Country)) +
  geom_bar(stat = "identity", width = 0.8) +  # adjusted bar width for cleaner look
  scale_x_reordered() + 
  facet_wrap(~category, scales = "free") +
  guides(fill = guide_legend(nrow = 3, byrow = FALSE)) +
  labs(
    x = NULL,
    y = "RNI Fulfillment Ratio",
    title = "Highest and Lowest RNI Fulfillments from Seafood",
    subtitle = "Amount of Individual RNIs Gained or Lost Daily",
    fill = "Country"  # ensure legend is clear for country colors
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # keep x-axis clean for better spacing
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "bottom",  # move legend to the bottom for better layout
    legend.title = element_text(face = "bold"),  # make the legend title stand out
    panel.grid.major.x = element_blank()  # remove unnecessary grid lines for clarity
  )
# ggsave("output/rni_by_nutrient.jpg")

# normalized / facetwrap
ggplot(nut_percap_plot_zscore, 
       aes(x = reorder_within(Country, by=rni_ratio_zscore,within = category), # reorders the countries in each facet
           y = rni_ratio_zscore, fill = Country)) +
  geom_bar(stat = "identity", width = 0.8) +  # adjusted bar width for cleaner look
  scale_x_reordered() +  # ensure the reordered labels are correctly displayed
  # scale_y_continuous(limits = c(-12, 6),  # Adjusted y-axis limits slightly beyond the data range
  #                    breaks = seq(-12, 6, by = 3)) +  # Set tick marks every 2 units
  facet_wrap(~category, scales = "free_x") +
  guides(fill = guide_legend(nrow = 3, byrow = FALSE)) +
  labs(
    x = NULL,
    y = "Z-Score of RNI Fulfillment",
    title = "Highest and Lowest RNI Fulfillments from Seafood",
    subtitle = "Normalized RNIs Gained or Lost Daily",
    fill = "Country"  # ensure legend is clear for country colors
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # keep x-axis clean for better spacing
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "bottom",  # move legend to the bottom for better layout
    legend.title = element_text(face = "bold"),  # make the legend title stand out
    panel.grid.major.x = element_blank()  # remove unnecessary grid lines for clarity
  )
# ggsave("output/rni_by_nutrient_zscore.jpg")


# stacked barchart style
ggplot(nut_percap_plot,
       aes(reorder(Country,-rni_ratio),rni_ratio,fill=category)) +
  geom_bar(stat = "identity") +
  labs(x=NULL)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45))



#### [3] plot country-level nutrient-specific metrics - % of pop w/ fulfilled RNIs ####
rni_perc_plot <- rni_perc_df %>%
  group_by(category) %>%
  arrange(desc(rni_perc_pop_total)) %>%            
  slice(c(1:5, (n() - 4):n()))
rni_perc_daily_plot <- rni_perc_df %>%
  group_by(category) %>%
  arrange(desc(rni_perc_pop_daily)) %>%            
  slice(c(1:5, (n() - 4):n()))

# facet_wrap style - daily
ggplot(rni_perc_daily_plot, 
       aes(x = reorder_within(Country, rni_perc_pop_daily,category),
           y = rni_perc_pop_daily, fill = Country)) +
  geom_bar(stat = "identity", width = 0.8) +  # adjusted bar width for cleaner look
  scale_x_reordered() + 
  facet_wrap(~category, scales = "free") +
  guides(fill = guide_legend(nrow = 3, byrow = FALSE)) +  
  labs(
    x = NULL,
    y = "% Population",
    title = "Percent of Population Impacted by Seafood Trade",
    subtitle = "Recommended Nutrient Intakes Gained or Lost on Average Daily",
    fill = "Country"  # ensure legend is clear for country colors
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # keep x-axis clean for better spacing
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "bottom",  # move legend to the bottom for better layout
    legend.title = element_text(face = "bold"),  # make the legend title stand out
    panel.grid.major.x = element_blank()  # remove unnecessary grid lines for clarity
  )
# ggsave("output/percpop_by_nutrient.jpg")

# facet_wrap style - aggregate
ggplot(rni_perc_plot, 
       aes(x = reorder_within(Country, rni_perc_pop_total,category),
           y = rni_perc_pop_total, fill = Country)) +
  geom_bar(stat = "identity", width = 0.8) +  # adjusted bar width for cleaner look
  scale_x_reordered() + 
  facet_wrap(~category, scales = "free") +
  guides(fill = guide_legend(nrow = 3, byrow = FALSE)) +  
  labs(
    x = NULL,
    y = "% Population",
    title = "Percent of Population Losing or Gaining RNIs from Seafood",
    subtitle = "Aggregated Over Time Period",
    fill = "Country"  # ensure legend is clear for country colors
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # keep x-axis clean for better spacing
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 12),
    legend.position = "bottom",  # move legend to the bottom for better layout
    legend.title = element_text(face = "bold"),  # make the legend title stand out
    panel.grid.major.x = element_blank()  # remove unnecessary grid lines for clarity
  )


