#### goal: use edge lists to calculate annual network metrics ####
## 'regular networks' and multilayer networks

#### [0] prep ####
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)

edge_df_pics <- read_csv("output/edge_consumption_pics_nutrients_annual.csv")

#### [1] prep data ####
nut_df <- edge_df_pics %>% filter(Nutrient == "Protein") %>%
  select(SourceContinent,ExporterContinent,ConsumerContinent,year,Nutrient,rni_amount) %>%
  mutate(ExporterContinent=str_replace(ExporterContinent,"Domestic PICs","PICs"))


#### [2] regular network analysis ####
## write function for creating networks, then calculate metrics 
network_metrics <- data.frame()
node_metrics <- data.frame()

# Loop through each year in the dataset
for (yr in unique(nut_df$year)) {
  
  #### filter data for the specific year ####
  annual_data <- nut_df %>% filter(year == yr)
  
  #### create edges from the filtered data ####
  edges <- rbind(
    data.frame(from = annual_data$SourceContinent, to = annual_data$ExporterContinent, weight = annual_data$rni_amount),
    data.frame(from = annual_data$ExporterContinent, to = annual_data$ConsumerContinent, weight = annual_data$rni_amount)
  )
  
  #### create a directed graph with weighted edges for this year ####
  g <- graph_from_data_frame(edges, directed = TRUE)
  
  #### calculate network-wide metrics ####
  degree_in <- mean(degree(g, mode = "in"))  # In-degree centrality
  degree_out <- mean(degree(g, mode = "out"))  # Out-degree centrality
  betweenness <- mean(betweenness(g, directed = TRUE))  # Betweenness centrality
  closeness <- mean(closeness(g, mode = "all"))  # Closeness centrality
  density <- edge_density(g)  # Network density
  avg_path_length <- mean_distance(g, directed = TRUE)  # Average path length
  modularity_value <- modularity(cluster_fast_greedy(as.undirected(g)))  # Modularity
  
  # Store network-wide metrics in the dataframe
  network_metrics <- rbind(network_metrics, data.frame(
    Year = yr,
    degree_in = degree_in,
    degree_out = degree_out,
    betweenness = betweenness,
    closeness = closeness,
    density = density,
    avg_path_length = avg_path_length,
    modularity = modularity_value
  ))
  
  #### calculate node-specific metrics ####
  node_degree_in <- degree(g, mode = "in")  # Node in-degree
  node_degree_out <- degree(g, mode = "out")  # Node out-degree
  node_betweenness <- betweenness(g, directed = TRUE)  # Node betweenness
  node_closeness <- closeness(g, mode = "all")  # Node closeness
  node_strength_in <- strength(g, mode = "in", weights = E(g)$weight)  # In-strength
  node_strength_out <- strength(g, mode = "out", weights = E(g)$weight) # out-strength
  node_strength_diff <- node_strength_in - node_strength_out
  
  # Store node-specific metrics in the dataframe
  node_metrics <- rbind(node_metrics, data.frame(
    Year = yr,
    Node = V(g)$name,
    degree_in = node_degree_in,
    degree_out = node_degree_out,
    betweenness = node_betweenness,
    closeness = node_closeness,
    strength_in = node_strength_in,  
    strength_out = node_strength_out,
    strength_diff = node_strength_diff
  ))
}

#### save outputs ####
# write.csv(network_metrics,"output/networkmetrics_annual_pics.csv",row.names = FALSE)
# write.csv(node_metrics,"output/nodemetrics_annual_pics.csv",row.names = FALSE)

#### explore network metric results ####
## initially:avg_path_length, modularity (makes sense since correlated)
# ggplot(network_metrics,aes(year,modularity)) + geom_line()
# summary(lm(modularity ~ year, data = network_metrics))

#### explore node metric results ####
ggplot(node_metrics,aes(year,strength_diff,colour = Node,group=Node)) + geom_line() +
  ggtitle(paste0(unique(nut_df$Nutrient)))




#### [3] multilayer network analysis ####
network_metrics_ml <- data.frame()
node_metrics_ml <- data.frame()

# Loop through each year in the dataset
for (yr in unique(nut_df$year)) {
  
  #### filter data for the specific year ####
  annual_data <- nut_df %>% filter(year == yr)
  
  #### append explicit role names ####
  annual_data$SourceContinent <- paste0(annual_data$SourceContinent, "_Source")
  annual_data$ExporterContinent <- paste0(annual_data$ExporterContinent, "_Exporter")
  annual_data$ConsumerContinent <- paste0(annual_data$ConsumerContinent, "_Consumer")
  
  #### create edges for Source -> Exporter and Exporter -> Consumer ####
  edges <- rbind(
    data.frame(from = annual_data$SourceContinent, to = annual_data$ExporterContinent, weight = annual_data$rni_amount),
    data.frame(from = annual_data$ExporterContinent, to = annual_data$ConsumerContinent, weight = annual_data$rni_amount)
  )
  
  #### create a directed graph with weighted edges for this year ####
  g <- graph_from_data_frame(edges, directed = TRUE)
  
  #### calculate network-wide metrics ####
  degree_in <- mean(degree(g, mode = "in"))  # In-degree centrality
  degree_out <- mean(degree(g, mode = "out"))  # Out-degree centrality
  betweenness <- mean(betweenness(g, directed = TRUE))  # Betweenness centrality
  closeness <- mean(closeness(g, mode = "all"))  # Closeness centrality
  density <- edge_density(g)  # Network density
  avg_path_length <- mean_distance(g, directed = TRUE)  # Average path length
  modularity_value <- modularity(cluster_fast_greedy(as.undirected(g)))  # Modularity
  
  # store network-wide metrics in the dataframe
  network_metrics_ml <- rbind(network_metrics_ml, data.frame(
    year = yr,
    degree_in = degree_in,
    degree_out = degree_out,
    betweenness = betweenness,
    closeness = closeness,
    density = density,
    avg_path_length = avg_path_length,
    modularity = modularity_value
  ))
  
  #### calculate node-specific metrics ####
  node_degree_in <- degree(g, mode = "in")  # Node in-degree
  node_degree_out <- degree(g, mode = "out")  # Node out-degree
  node_betweenness <- betweenness(g, directed = TRUE)  # Node betweenness
  node_closeness <- closeness(g, mode = "all")  # Node closeness
  node_strength_in <- strength(g, mode = "in", weights = E(g)$weight)  # In-strength
  node_strength_out <- strength(g, mode = "out", weights = E(g)$weight)  # Out-strength
  node_strength_diff <- node_strength_in - node_strength_out  # Strength difference
  
  # store node-specific metrics in the dataframe
  node_metrics_ml <- rbind(node_metrics_ml, data.frame(
    Year = yr,
    Node = V(g)$name,
    degree_in = node_degree_in,
    degree_out = node_degree_out,
    betweenness = node_betweenness,
    closeness = node_closeness,
    strength_in = node_strength_in,  
    strength_out = node_strength_out,
    strength_diff = node_strength_diff
  ))
}



#### save outputs ####
# write.csv(network_metrics_ml,"output/networkmetrics_annual_pics_ml.csv",row.names = FALSE)
# write.csv(node_metrics_ml,"output/nodemetrics_annual_pics_ml.csv",row.names = FALSE)

