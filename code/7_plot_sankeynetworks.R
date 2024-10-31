#### goal: use edge lists to make sankey and network visualizations ####
#### [0] prep ####
library(ggalluvial)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(tidygraph)

net_agg <- read_csv("output/edge_consumption_pics_nutrients.csv") # pics/non-pics data
# net_agg_countries <- read_csv("output/edge_consumption_countries_nutrients.csv") # country data


#### sankey: pics/non-pics ####
alluvial_data <- net_agg %>%
  mutate(Flow = paste(SourceContinent, "to", ExporterContinent, "to", ConsumerContinent)) %>%
  group_by(Flow, Nutrient) %>%
  summarise(Value = sum(Value)) %>%
  separate(Flow, into = c("SourceContinent", "ExporterContinent","ConsumerContinent"), sep = " to ") %>%
  ungroup() %>%  group_by(SourceContinent,ExporterContinent,ConsumerContinent)%>%
  mutate(Triad_proportion = Value / sum(Value)) %>%
  mutate(Nutrient = factor(Nutrient, levels = unique(Nutrient)),
         SourceContinent = factor(SourceContinent, levels = unique(SourceContinent)),
         ExporterContinent = factor(ExporterContinent, levels = unique(ExporterContinent)),
         ConsumerContinent = factor(ConsumerContinent, levels = unique(ConsumerContinent))) %>%
  droplevels() %>% ungroup() %>%
  mutate(Total_proportion = Value / sum(Value))


# Create labels for proportions
alluvial_data <- alluvial_data %>%
  mutate(Label_triad_prop = paste0(round(Triad_proportion * 100, 1), "%"),
         Label_total_prop = paste0(round(Total_proportion * 100, 1), "%"))

exporter_labels <- alluvial_data %>%
  group_by(ExporterContinent) %>%
  mutate(mid_y = cumsum(Value) + 1 * Value)


## Plot triple sankey
colors <- brewer.pal(7,"Set3") # establish color theme

ggplot(alluvial_data,
       aes(axis1 = SourceContinent, axis2 = ExporterContinent, axis3= ConsumerContinent, y = Value, fill = Nutrient)) +
  geom_alluvium(width = 1/12, knot.pos = 0.5) +
  geom_stratum(fill="grey95",width = 1/4, show.legend = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3, vjust = .5) +
  # geom_text(aes(label = Label_triad_prop,x=1.2), size = 5, color = "black", position = position_stack(vjust = .3),check_overlap = TRUE) +
  scale_x_discrete(limits = c("SourceContinent", "ExporterContinent","ConsumerContinent"), expand = c(0.1, 0.1),
                   labels = c("SourceContinent" = "Source","ExporterContinent" = "Exporter","ConsumerContinent"="Consumer")) +
  scale_fill_manual(name="Nutrient",values=colors,na.translate=FALSE)+
  labs(title = "Nutrient Flows Between PICs and Non-PICs",x=NULL,
       y = NULL, fill = "Nutrient") + 
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),#element_text(size=15),
        legend.text = element_text(size=13),
        legend.background = element_rect(fill = "transparent", colour = "grey20",linewidth=.2),
        legend.position = "bottom",
        axis.text.x = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,size=17)) +
  guides(fill = guide_legend(title = "Nutrient"),color="none")

# ggsave("output/sankey_nutrients_consumption_continents.jpg")
#


#### sankey: country-level ####
# choose nutrient
protein_data <- net_agg_countries %>%
  filter(Nutrient == "Protein")


ggplot(protein_data,
       aes(axis1 = SourceCountry, axis2 = ExporterCountry, axis3 = ConsumerCountry,
           y = Value)) +
  geom_alluvium(aes(fill = SourceCountry)) +
  geom_stratum() +
  guides(fill="none")+
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Source", "Exporter", "Consumer")) +
  theme_minimal() +
  labs(title = "Protein Flow: Source to Exporter to Consumer",
       x = "Stage", y = "Protein Value")
# ggsave("output/sankey_countries_protein.jpg")

# top flows
major_protein_flows <- protein_data %>%
  group_by(SourceCountry, ExporterCountry, ConsumerCountry) %>%
  summarize(Total = sum(Value)) %>%
  filter(Total > 1e8)

ggplot(major_protein_flows,
       aes(axis1 = SourceCountry, axis2 = ExporterCountry, axis3 = ConsumerCountry,
           y = Total)) +
  geom_alluvium(aes(fill = SourceCountry)) +
  geom_stratum() +
  guides(fill="none")+
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Source", "Exporter", "Consumer")) +
  theme_minimal() +
  labs(title = "Protein Flow: Source to Exporter to Consumer",
       x = "Stage", y = "Protein Value")
# ggsave("output/sankey_topcountries_protein.jpg")


#### network: country-level ####
# make edge list
edges_df <- data.frame(
  from = c(protein_data$SourceCountry, protein_data$ExporterCountry),
  to = c(protein_data$ExporterCountry, protein_data$ConsumerCountry),
  value = rep(protein_data$Value, 2)
)

# make nodes list
nodes_df <- data.frame(
  name = unique(c(protein_data$SourceCountry, protein_data$ExporterCountry, protein_data$ConsumerCountry)),
  type = ifelse(unique(c(protein_data$SourceCountry, protein_data$ExporterCountry, protein_data$ConsumerCountry)) %in% protein_data$SourceCountry, "source",
                ifelse(unique(c(protein_data$SourceCountry, protein_data$ExporterCountry, protein_data$ConsumerCountry)) %in% protein_data$ExporterCountry, "exporter", "consumer"))
)

# make network object
g <- graph_from_data_frame(d = edges_df, vertices = nodes_df, directed = TRUE)

# plot network
ggraph(g, layout = 'fr') + 
  geom_edge_link(aes(edge_alpha = value, edge_width = value), arrow = arrow(type = "closed", length = unit(3, 'mm'))) +
  geom_node_point(aes(color = type), size = 5) + 
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  scale_color_manual(values = c('source' = 'tomato', 'exporter' = 'gold', 'consumer' = 'dodgerblue')) +
  ggtitle("Protein Flow: Source to Consumer")

