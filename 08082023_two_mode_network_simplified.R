library(igraph)
df_two_mode_10 <- read.csv('two_mode_10.csv', row.names = 1)
df_two_mode_10 <- df_two_mode_10[,!colnames(df_two_mode_10) %in% "pol"]
print(paste("There are", nrow(df_two_mode_10), "items."))
df_two_mode_10_simple <- df_two_mode_10[rowSums(df_two_mode_10) > 0,]
print(paste(nrow(df_two_mode_10_simple), "are kept for network visualization."))
# How many rows in difference?
print(nrow(df_two_mode_10) - nrow(df_two_mode_10_simple))
two_mode_10_graph <- graph_from_incidence_matrix(as.matrix(df_two_mode_10_simple))
# How many nodes?
V_count <- length(V(two_mode_10_graph))
# Confirms that column names (features) are among the node names
colnames(df_two_mode_10) %in% V(two_mode_10_graph)$name
# Confirms that row names (ids) are among the node names
rownames(df_two_mode_10) %in% V(two_mode_10_graph)$name
# V()$type returns FALSE or TRUE, FALSE are the row names
table(V(two_mode_10_graph)$type)

##### VISUALIZATION #####

#### LABELS ####
# default no node labels
V(two_mode_10_graph)$label <- "" 
# show feature labels
V(two_mode_10_graph)[V(two_mode_10_graph)$type == T]$label <- V(two_mode_10_graph)[V(two_mode_10_graph)$type == T]$name

#### COLOR ####
V(two_mode_10_graph)$color <- c("orange","lightsteelblue")[V(two_mode_10_graph)$type + 1]

#### SIZE ####
V(two_mode_10_graph)$size <- c(2,20)[V(two_mode_10_graph)$type + 1]

#### Visualize ####
plot(two_mode_10_graph, vertex.frame.color = NA, vertex.label.cex = 1, vertex.label.font = 2, layout = layout_nicely)