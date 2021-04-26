source("R/01_source.R")

# Load data ---------------------------------------------------------------

review <- qread("output/net_review.qs")

property  <- qread("output/property.qs")
# https://www.jessesadler.com/post/network-analysis-with-r/
# https://kateto.net/network-visualization

# Creation of the network density df --------------------------------------

# My nodes are every property_ID.
nodes <-
  review %>%
  distinct(property_ID)

# list every property a user has been to
user_properties <-
review %>%
  arrange(date) %>%
  group_by(user_ID) %>%
  filter(n() > 1) %>%
  # distinct so that there's no duplicate edges for one user_ID
  distinct(property_ID, .keep_all=T) %>%
  summarize(list = list(property_ID))

edges <-
  user_properties %>%
  mutate(pairs = map2(list, list, expand.grid, stringsAsFactors = FALSE)) %>%
  select(-list) %>%
  unnest(pairs) %>%
  mutate(pairs = map2(Var1, Var2, c)) %>%
  select(-Var1, -Var2) %>%
  mutate(pairs = map(pairs, sort)) %>%
  rowwise() %>%
  filter(pairs[1] != pairs[2]) %>%
  ungroup() %>%
  distinct() %>%
  mutate(V1 = map_chr(pairs, `[`, 1),
         V2 = map_chr(pairs, `[`, 2)) %>%
  count(V1, V2, name = "weight", sort = TRUE)


qsavem(edges, nodes, file = "output/edges_nodes.qsm")

# Plotting it, analysis ---------------------------------------------------

qload("output/edges_nodes.qsm")

edges <- edges %>% filter(weight>4)
nodes <- nodes %>% 
  filter(property_ID %in% c(edges$V1, edges$V2))

# Tidygraph ---------------------------------------------------------------

library(tidygraph)
library(ggraph)

graph <- tbl_graph(nodes = nodes, edges = edges, directed=F, node_key = "property_ID")

# to_components(graph, type = "strong")

graph %>% 
  morph(to_components) %>% 
  activate(edges) %>% 
  data.frame()

library(graphlayouts)

graph %>%
  ggraph(layout = "nicely") +
  geom_edge_diagonal(color = "grey50", alpha = 0.5) +
  geom_node_point(aes()) +
  scale_color_identity() +
  theme_graph()

graph %>% 
  mutate(dist_to_center = node_distance_to(node_is_center())) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(aes(size = dist_to_center), colour = 'steelblue') + 
  scale_size_continuous(range = c(6, 1)) + 
  theme_graph()


# Igraph ------------------------------------------------------------------

library(igraph)



# igraph_graph <- graph.edgelist(as.matrix(uncount(edges, weight)), directed = FALSE)
igraph_graph <- graph.data.frame(edges, directed = FALSE)

# eigenvector centrality

#' "In graph theory, eigenvector centrality (also called eigencentrality or prestige 
#'score) is a measure of the influence of a node in a network. Relative scores are 
#'assigned to all nodes in the network based on the concept that connections to 
#'high-scoring nodes contribute more to the score of the node in question than 
#'equal connections to low-scoring nodes. A high eigenvector score means that a 
#'node is connected to many nodes who themselves have high scores."
eigenvector_centrality <- 
eigen_centrality(igraph_graph, scale=FALSE, directed = FALSE)$vector %>% #look at weights argument
  as.data.frame() %>% 
  rownames_to_column("property_ID") %>% 
  rename(eigenvector_centrality = 2) %>% 
  arrange(-eigenvector_centrality) %>% 
  as_tibble()

#' Degree is a simple centrality measure that counts how many ties a node has. 
node_degrees <- 
degree(igraph_graph) %>% 
  as.data.frame() %>% 
  rownames_to_column("property_ID") %>% 
  rename(degree = 2) %>% 
  arrange(-degree) %>% 
  as_tibble()

#'  It is used on weighted networks, which are networks where ties are labeled 
#'  with a connection strength. To compute the strength of a node in a weighted 
#'  network you sum the weights of the ties attached to a given node.
node_strength <- 
  strength(igraph_graph) %>% 
  as.data.frame() %>% 
  rownames_to_column("property_ID") %>% 
  rename(strength = 2) %>% 
  arrange(-strength) %>% 
  as_tibble()

# Getting that summarized in one df
nodes_improved <- 
  nodes %>% 
  left_join(eigenvector_centrality) %>% 
  left_join(node_degrees) %>% 
  inner_join(node_strength)

nodes_improved %>% 
  ggplot()+
  geom_point(aes(x = degree, y = strength))+
  geom_smooth(aes(x = degree, y = strength),
              method="lm", se=F)

cor(nodes_improved$degree, nodes_improved$strength)



# Further inspection
# the next plot can be interesting if we filter in only, at minimum, edges with
# weights of 3

count_max_cliques(igraph_graph, min = 10)

simplified_igraph <- igraph::simplify(igraph_graph)
communities <- cluster_louvain(simplified_igraph)
# identify which communities have fewer than x members
small <- which(table(communities$membership) < 11)
# Which nodes should be kept?
keep <- V(simplified_igraph)[!(communities$membership %in% small)]
# Get subgraph & plot
simplified_igraph2  <- induced_subgraph(simplified_igraph, keep)
communities2 <- cluster_louvain(simplified_igraph2)
plot(communities2, simplified_igraph2, vertex.label = NA, vertex.size=5, 
     edge.arrow.size = .2)

# 3d interactive ----------------------------------------------------------

library(visNetwork)

nodesd3 <- 
  nodes %>% 
  rowid_to_column("id") %>% 
  mutate(id = id-1) # because it needs to start by 0 for next steps

edgesd3 <-
  edges %>% 
  left_join(nodesd3, by = c("V1" = "property_ID")) %>% 
  select(-V1) %>% 
  rename(V1 = id)

edgesd3 <-
  edgesd3 %>% 
  left_join(nodesd3, by = c("V2" = "property_ID")) %>% 
  select(-V2) %>% 
  rename(V2 = id)

nodes_d3 <- mutate(nodesd3, id = id) %>% as.data.frame()
edges_d3 <- mutate(edgesd3, from = V1, to = V2) %>% as.data.frame()

networkD3::forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
                        NodeID = "property_ID", Group = "id", Value = "weight", 
                        opacity = 1, fontSize = 30, zoom = TRUE)

networkD3::sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to",
              NodeID = "property_ID", Value = "weight", fontSize = 16, unit = "Reviews", sinksRight=F)


# Diving in the highest concentration -------------------------------------

#biggest nodes groups
big_node_1 <- 
  nodes_d3 %>% 
  filter(id %in% (edges %>% 
                    filter(V2 == (nodes_d3 %>% filter(property_ID == "ab-7993099") %>% pull(id))) %>% 
                    pull(V1)) | 
           property_ID == "ab-7993099") %>% 
  pull(property_ID)

big_node_2 <- 
  nodes_d3 %>% 
  filter(id %in% (edges %>% 
                    filter(V1 == (nodes_d3 %>% filter(property_ID == "ab-21817765") %>% pull(id))) %>% 
                    pull(V2)) | 
           property_ID == "ab-21817765") %>% 
  pull(property_ID)


# Diving in the first node
review_text %>% 
  filter(property_ID %in% big_node_1) %>% 
  group_by(user_ID) %>% 
  filter(n() > 1)

review_user %>% 
  filter(user_ID %in% (review_text %>% 
                         filter(property_ID %in% big_node_1) %>% 
                         group_by(user_ID) %>% 
                         filter(n() > 1) %>% 
                         pull(user_ID) %>% unique())) %>%
  arrange(date) %>% 
  distinct(user_ID, .keep_all=T) %>% View
  

# Interesting that these users are 7/8 from Montreal, and have very similar
# account creation dates. The 5 that have a description all "loves traveling" yet
# reviews listings in Montreal. AFter a quick look online, they also all get 
# reviews at a high repetition by user 498177, which has one listing very commercial that isn't
# even in the list of properties that we flaged.
# The host always reviews them as guest with "Great" or other responses that are always reused
# to a user and the other.
# Maybe here for the density network I should keep reviews shorter than 5 words.

qload("output/str_processed.qsm")

property %>%
  st_drop_geometry() %>%
  filter(property_ID == "ab-21817765") %>% View

review_text %>% 
  filter(property_ID %in% big_node_2) %>% 
  group_by(user_ID) %>% 
  filter(n() > 1) %>% View

review_user %>% 
  filter(user_ID %in% (review_text %>% 
                         filter(property_ID %in% big_node_2) %>% 
                         group_by(user_ID) %>% 
                         filter(n() > 1) %>% 
                         pull(user_ID) %>% unique())) %>%
  arrange(date) %>% 
  distinct(user_ID, .keep_all=T) %>% View

# Same relative here: All from Montreal, and very similar description. They also
# all get reviewed at repetition by another host: 125900826. Small reviews and always
# the same. "Great"






#' I think it's interesting the degrees and strengths ONLY when I filter in
#' only the biggest edges (weights higher than 2). However, if I look at all the
#' data, it doesn't seem to make sense anymore, while the eigenvector centrality
#' looks very promising (the 4 tops have reviews to Sam, which is also the same
#' host I flagged earlier in big_node_1)

heighest_eigen <- 
nodes_improved %>% 
  arrange(-eigenvector_centrality) %>% 
  slice(1:36) %>% 
  pull(property_ID)

property %>% 
  filter(property_ID %in% heighest_eigen) %>% 
  count(city)

review_text <- qread("output/net_review_text.qs")
review_text_pred <- qread(here("output","review_text_pred_liwc.qs"))

review_text %>% 
  filter(property_ID %in% (property %>% 
                             filter(property_ID %in% heighest_eigen) %>% 
                             pull(property_ID))) %>% 
  pull(review_ID)

review_text_pred %>% 
  filter(review_ID %in%(review_text %>% 
                          filter(property_ID %in% (property %>% 
                                                     filter(property_ID %in% heighest_eigen) %>% 
                                                     pull(property_ID))) %>% 
                          pull(review_ID))) %>% View
