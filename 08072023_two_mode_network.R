library(igraph)
df_two_mode_90 <- read.csv('two_mode_90.csv', row.names = 1)
df_two_mode_90 <- df_two_mode_90[,!colnames(df_two_mode_90) %in% "pol"]
two_mode_90_graph <- graph_from_incidence_matrix(as.matrix(df_two_mode_90))
# How many nodes?
V_count <- length(V(two_mode_90_graph))
# Confirms that column names (features) are among the node names
colnames(df_two_mode_90) %in% V(two_mode_90_graph)$name
# Confirms that row names (ids) are among the node names
rownames(df_two_mode_90) %in% V(two_mode_90_graph)$name
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

#### SIZE ####
V(two_mode_90_graph)$size <- c(2,20)[V(two_mode_90_graph)$type + 1]

#### Visualize ####
plot(two_mode_90_graph, vertex.frame.color = NA, vertex.label.cex = 1.5, vertex.label.font = 2, layout = layout_nicely)