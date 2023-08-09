library(igraph)
df_two_mode_90 <- read.csv('two_mode_90.csv', row.names = 1)
df_two_mode_90 <- df_two_mode_90[,!colnames(df_two_mode_90) %in% "pol"]
print(paste("There are", nrow(df_two_mode_90), "items."))
#### summary statistics ####
mean(rowSums(df_two_mode_90))
paste("On average, an article contains", round(mean(rowSums(df_two_mode_90)),2), "keyword themes")
# share of articles w/o themes, calculated from row sum
paste(round(table(rowSums(df_two_mode_90))[1] / nrow(df_two_mode_90),4), "articles do not contain any keyword themes.")
#### Simplified ####
df_two_mode_90_simple <- df_two_mode_90[rowSums(df_two_mode_90) > 0,]
print(paste(nrow(df_two_mode_90_simple), "are kept for network visualization."))
# How many rows in difference?
print(nrow(df_two_mode_90) - nrow(df_two_mode_90_simple))
two_mode_90_graph <- graph_from_incidence_matrix(as.matrix(df_two_mode_90_simple))
# How many nodes?
V_count <- length(V(two_mode_90_graph))
# Confirms that column names (features) are among the node names
colnames(df_two_mode_90) %in% V(two_mode_90_graph)$name
# Confirms that row names (ids) are among the node names
length(rownames(df_two_mode_90) %in% V(two_mode_90_graph)$name)
# V()$type returns FALSE or TRUE, FALSE are the row names
table(V(two_mode_90_graph)$type)

##### VISUALIZATION #####

#### LABELS ####
# default no node labels
V(two_mode_90_graph)$label <- "" 
# show feature labels
V(two_mode_90_graph)[V(two_mode_90_graph)$type == T]$label <- V(two_mode_90_graph)[V(two_mode_90_graph)$type == T]$name

#### COLOR ####
V(two_mode_90_graph)$color <- c("orange","lightsteelblue")[V(two_mode_90_graph)$type + 1]

#### DEGREE (could impact SIZE) ####
feature_degrees_two_mode_90_graph <- degree(two_mode_90_graph, v = V(two_mode_90_graph)[V(two_mode_90_graph)$type])
log_feature_degrees_two_mode_90_graph <- log(feature_degrees_two_mode_90_graph + 0.1)
feature_degree_range <- max(log_feature_degrees_two_mode_90_graph) - min(log_feature_degrees_two_mode_90_graph)
print(paste("The range of log-transformed degree of features is:", round(feature_degree_range,2)))
#### SIZE ####
## set initial sizes for ids and features
V(two_mode_90_graph)$size <- c(2, 20)[V(two_mode_90_graph)$type + 1]
## overwrite node size of features, depending on their degree
V(two_mode_90_graph)$size[V(two_mode_90_graph)$type] <- 15 + log_feature_degrees_two_mode_90_graph
# check feature size
V(two_mode_90_graph)$size[V(two_mode_90_graph)$type]

#### Visualize ####
set.seed(1234)
plot(two_mode_90_graph, vertex.frame.color = NA, vertex.label.cex = 1, vertex.label.font = 2, layout = layout_nicely)