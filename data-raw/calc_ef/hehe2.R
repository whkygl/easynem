# 加载所需的库
library(igraph)

# 定义节点和边
nodes <- data.frame(
  id = c("R", "A", "B", "C", "D"),
  label = c("R", "168.3", "507.0", "70.3", "52.7"),
  size = c(30, 30, 50, 30, 30),
  color = c("grey", "orange", "blue", "brown", "green")
)

edges <- data.frame(
  from = c("R", "R", "A", "A", "B", "C"),
  to = c("A", "D", "B", "C", "C", "D"),
  weight = c(17.2, 18.5, 6.4, 10.0, 5.2, 70.3),
  label = c(17.2, 18.5, 6.4, 10.0, 5.2, 70.3)
)

# 创建图对象
graph <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

# 设定节点和边的属性
V(graph)$size <- nodes$size
V(graph)$color <- nodes$color
E(graph)$width <- edges$weight / 10
E(graph)$label <- edges$label

# 手动设置节点位置
layout_matrix <- matrix(c(
  0, 0,   # R
  0, 3,   # A
  3, 3,   # B
  3, 0,   # C
  1.5, 1.5  # D
), ncol = 2, byrow = TRUE)

# 绘制图
plot(graph, vertex.label = V(graph)$label, edge.label = E(graph)$label, 
     vertex.label.cex = 1.5, edge.label.cex = 1.5, 
     edge.arrow.size = 0.5, layout = layout_matrix)
