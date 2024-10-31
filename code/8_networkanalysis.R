#### goal: use edge lists to analyze network metrics ####
#### [0] prep ####
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)

# edge_df_og <- read_csv("output/edge_consumption_countries_global_nutrients.csv")
edge_df_og <- read_csv("output/edge_consumption_countries_nutrients.csv")
edge_df_pics <- read_csv("output/edge_consumption_pics_nutrients.csv")

# establish pics
picts <- c("FJI", "PNG", "SLB", "VUT", "FSM", "PLW", "MHL", "KIR", "NRU", "WSM", "TON", "TUV")


#### [1] separate domestic consumption from triads ####
# Filter for domestic consumption (SourceCountry == ConsumerCountry)
domestic_consumption_df <- edge_df_og %>% 
  filter(ExporterCountry == ConsumerCountry & ConsumerCountry == SourceCountry)
triad_consumption_df <- anti_join(edge_df_og,domestic_consumption_df)

#### [2] weighted,directed networks - create separate networks for each nutrient ####
# Split the dataframe by Nutrient type
nutrient_networks <- triad_consumption_df %>% split(.$Nutrient)

# Create separate graph objects for each nutrient flow
graphs_by_nutrient <- lapply(nutrient_networks, function(df) {
  graph_from_data_frame(df %>% select(SourceCountry, ConsumerCountry, Nutrient, rni_amount), directed = TRUE)
})

#### [3] Function to calculate node-level metrics for each nutrient ####
calculate_node_metrics <- function(graph_triad) {
  
  # Set edge weights and handle zero or negative values
  E(graph_triad)$weight <- as.numeric(E(graph_triad)$rni_amount)
  E(graph_triad)$weight[E(graph_triad)$weight <= 0] <- 0.001
  
  # Calculate metrics
  in_degree <- degree(graph_triad, mode = "in")
  out_degree <- degree(graph_triad, mode = "out")
  betweenness_cent <- betweenness(graph_triad, directed = TRUE, weights = E(graph_triad)$weight)
  clustering_coeff <- transitivity(graph_triad, type = "local", isolates = "zero")
  total_inflow <- strength(graph_triad, mode = "in", weights = E(graph_triad)$weight)
  total_outflow <- strength(graph_triad, mode = "out", weights = E(graph_triad)$weight)
  
  # Community detection using Walktrap
  mod <- cluster_walktrap(graph_triad, weights = E(graph_triad)$weight)
  community_membership <- membership(mod)
  
  # Create a dataframe with the node-level metrics
  node_metrics_df <- data.frame(
    node = names(in_degree),
    in_degree = in_degree,
    out_degree = out_degree,
    total_inflow = total_inflow,
    total_outflow = total_outflow,
    net_flow = total_inflow - total_outflow,
    betweenness_cent = betweenness_cent,
    clustering_coeff = clustering_coeff,
    community = community_membership
  )
  
  return(node_metrics_df)
}

#### [4] Apply the function to all nutrient networks ####
# Create a list to store node metrics for each nutrient
node_metrics_by_nutrient <- lapply(graphs_by_nutrient, calculate_node_metrics)

# Name the list by nutrient so you can access each nutrient's metrics easily
names(node_metrics_by_nutrient) <- names(graphs_by_nutrient)


#### [5] Network-wide metrics for each nutrient ####
calculate_network_metrics <- function(graph_triad) {
  
  # Set edge weights and handle zero or negative rni_amounts
  E(graph_triad)$weight <- as.numeric(E(graph_triad)$rni_amount)
  E(graph_triad)$weight[E(graph_triad)$weight <= 0] <- 0.001
  
  # Community detection using Walktrap
  mod <- cluster_walktrap(graph_triad, weights = E(graph_triad)$weight)
  modularity_value <- modularity(mod)
  
  return(modularity_value)
}

# Calculate modularity for each nutrient network
network_modularity_by_nutrient <- lapply(graphs_by_nutrient, calculate_network_metrics)


#### [6] Other metrics - Shortest Paths ####
calculate_shortest_paths <- function(graph_triad) {
  
  # Set edge weights and handle zero or negative values
  E(graph_triad)$weight <- as.numeric(E(graph_triad)$rni_amount)
  E(graph_triad)$weight[E(graph_triad)$weight <= 0] <- 0.001
  
  # Calculate shortest paths between all nodes
  shortest_paths_matrix <- distances(graph_triad, weights = E(graph_triad)$weight)
  
  return(shortest_paths_matrix)
}

# Calculate shortest paths for each nutrient network
shortest_paths_by_nutrient <- lapply(graphs_by_nutrient, calculate_shortest_paths)




#### [7] plot metrics ####
## modularity
modularity_df <- data.frame(
  Nutrient = names(network_modularity_by_nutrient),
  Modularity = unlist(network_modularity_by_nutrient))
# plot
ggplot(modularity_df, aes(x = Nutrient, y = Modularity)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Create a bar plot
  theme_minimal() +                                # Use a minimal theme
  labs(title = "Modularity by Nutrient", 
       x = "Nutrient", 
       y = "Modularity Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## node metrics
combined_node_metrics <- list()

# loop through each nutrient and its node metrics
for (nutrient in names(node_metrics_by_nutrient)) {
  
  # get the node metrics dataframe for the current nutrient
  node_df <- node_metrics_by_nutrient[[nutrient]]
  
  # add a column for the nutrient type
  node_df$Nutrient <- nutrient
  
  # add the dataframe to the combined list
  combined_node_metrics[[nutrient]] <- node_df
}

# combine the list of dataframes into one large dataframe
combined_node_metrics_df <- bind_rows(combined_node_metrics)

# plot
ggplot(combined_node_metrics_df, 
       aes(x = Nutrient, y = net_flow, fill = Nutrient)) +
  geom_boxplot() +  # boxplot to show distribution of in-degree across nutrients
  theme_minimal() +
  labs(title = "comparison of net flows across nutrients", 
       x = "nutrient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # rotate x-axis labels

## another style w/ facet-wrapped bar chart
top_bottom_df <- combined_node_metrics_df %>%
  group_by(Nutrient) %>%
  arrange(desc(net_flow)) %>%
  slice(c(1:5, (n()-4):n())) %>%  # select top 5 and bottom 5 based on net flow
  ungroup()

# plot
ggplot(top_bottom_df, aes(x = reorder_within(node, net_flow,within=Nutrient), y = net_flow, fill = node)) +
  geom_bar(stat = "identity") +  # create bar chart with positive and negative net flows
  facet_wrap(~ Nutrient, scales = "free") +  # facet by nutrient with free scales
  scale_x_reordered() +
  theme_minimal() +
  labs(title = "Top and Bottom Net Flows for Each Nutrient", 
       x = "Node", 
       y = "Net Flow") + guides(fill="none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave("output/netflows_nutrients.jpg")



#### [8] multilayer networks ####
edge_df_pics <- read_csv("output/edge_consumption_pics_nutrients.csv")
# pick a nutrient
edge_df_pics <- edge_df_pics[edge_df_pics$Nutrient == "Protein", ]

# Append role to the node names to differentiate them
edge_df_pics$SourceContinent <- paste0(edge_df_pics$SourceContinent, "_Source")
edge_df_pics$ExporterContinent <- paste0(edge_df_pics$ExporterContinent, "_Exporter")
edge_df_pics$ConsumerContinent <- paste0(edge_df_pics$ConsumerContinent, "_Consumer")

# Create node types (Source, Exporter, Consumer)
source_nodes <- unique(edge_df_pics$SourceContinent)
exporter_nodes <- unique(edge_df_pics$ExporterContinent)
consumer_nodes <- unique(edge_df_pics$ConsumerContinent)

# Combine all unique nodes and assign their roles
all_nodes <- unique(c(source_nodes, exporter_nodes, consumer_nodes))
nodes <- data.frame(
  name = all_nodes,
  role = ifelse(grepl("_Source", all_nodes), "Source", 
                ifelse(grepl("_Exporter", all_nodes), "Exporter", "Consumer"))
)

# Create edges for Source -> Exporter
edges_source_to_exporter <- data.frame(
  from = edge_df_pics$SourceContinent,
  to = edge_df_pics$ExporterContinent,
  weight = edge_df_pics$rni_amount
)

# Create edges for Exporter -> Consumer
edges_exporter_to_consumer <- data.frame(
  from = edge_df_pics$ExporterContinent,
  to = edge_df_pics$ConsumerContinent,
  weight = edge_df_pics$rni_amount
)

# Combine the two edge sets into one dataframe
# edges_combined <- rbind(edges_source_to_exporter, edges_exporter_to_consumer)
edges_combined <- rbind(
  data.frame(from = edge_df_pics$SourceContinent, to = edge_df_pics$ExporterContinent, weight = edge_df_pics$rni_amount, Nutrient = edge_df_pics$Nutrient, connection = "Source to Exporter"),
  data.frame(from = edge_df_pics$ExporterContinent, to = edge_df_pics$ConsumerContinent, weight = edge_df_pics$rni_amount, Nutrient = edge_df_pics$Nutrient, connection = "Exporter to Consumer")
)


# Create a directed graph with nodes (Source, Exporter, Consumer) and weighted edges
g <- graph_from_data_frame(edges_combined, vertices = nodes, directed = TRUE)

# Set node attributes for roles (Source, Exporter, Consumer)
V(g)$role <- nodes$role

# Set edge weights based on the nutrient flow values
E(g)$weight <- edges_combined$weight

# Define node colors based on roles (Source, Exporter, Consumer)
V(g)$color <- ifelse(V(g)$role == "Source", "blue", 
                     ifelse(V(g)$role == "Exporter", "green", "red"))

# Scale edge widths based on the weight (nutrient flow)
E(g)$width <- E(g)$weight / max(E(g)$weight) * 10  # Normalize edge widths

#Calculate weighted degree (flow-based centrality)
weighted_degree <- strength(g, mode = "all", weights = E(g)$weight)

# Set node size proportional to weighted degree (larger size = more important in flow)
V(g)$size <- weighted_degree / max(weighted_degree) * 15  # Scale node sizes

# Plot the graph with ggraph
# sugiyama, circle
ggraph(g, layout = 'sugiyama') + 
geom_edge_link(aes(width = weight, color = connection), arrow = arrow(length = unit(4, 'mm')), end_cap = circle(3, 'mm')) + 
geom_node_point(aes(size = V(g)$size, color = V(g)$role)) + 
geom_node_text(aes(label = V(g)$name), repel = TRUE, size = 3) + 
scale_edge_width(range = c(0.2, 2)) + 
scale_size(range = c(3, 10)) + 
scale_color_manual(values = c("Source" = "blue", "Exporter" = "green", "Consumer" = "red")) + 
scale_edge_color_manual(values = c("Source to Exporter" = "purple", "Exporter to Consumer" = "orange")) +  # Color by connection
theme_void() + 
theme(legend.position = "bottom") + 
labs(title = "Vitamin B12 Flows in the Network", edge_width = "Flow", size = "Flow Importance", edge_color = "Connection Type")
