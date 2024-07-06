# 安装和加载igraph包
install.packages("igraph")
library(igraph)

# 示例数据：点矩阵和边矩阵
nodes <- data.frame(
  id = c(1, 2, 3, 4, 5, 6),
  group = c("A", "A", "B", "B", "C", "C")
)

edges <- data.frame(
  from = c(1, 2, 3, 4, 5),
  to = c(2, 3, 4, 5, 6)
)

# 创建图对象
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

# 绘制基本图
plot(g, vertex.color = V(g)$group)

# 安装并加载ggplot2和ggplot2的扩展包ggraph
install.packages("ggplot2")
install.packages("ggraph")
library(ggplot2)
library(ggraph)

# 创建分面网络图
ggraph(g, layout = 'fr') + 
  geom_edge_link(aes(edge_alpha = 0.8)) +
  geom_node_point(aes(color = group), size = 5) +
  facet_nodes(~group) +
  theme_void() +
  theme(legend.position = "none")
