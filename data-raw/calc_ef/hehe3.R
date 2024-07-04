library(igraph)
library(ggplot2)
library(ggraph)
# 创建示例网络图数据
set.seed(123) # 为了重复性设置随机种子
graph1 <- erdos.renyi.game(10, p=0.3, directed=FALSE)
graph2 <- erdos.renyi.game(10, p=0.3, directed=FALSE)
graph3 <- erdos.renyi.game(10, p=0.3, directed=FALSE)
graph4 <- erdos.renyi.game(10, p=0.3, directed=FALSE)

# 将网络图转为数据框格式
graph1_df <- as_data_frame(graph1, what = "edges")
graph2_df <- as_data_frame(graph2, what = "edges")
graph3_df <- as_data_frame(graph3, what = "edges")
graph4_df <- as_data_frame(graph4, what = "edges")

# 添加分面标签
graph1_df$Row <- "Row 1"
graph1_df$Column <- "Column 1"

graph2_df$Row <- "Row 1"
graph2_df$Column <- "Column 2"

graph3_df$Row <- "Row 2"
graph3_df$Column <- "Column 1"

graph4_df$Row <- "Row 2"
graph4_df$Column <- "Column 2"

# 合并数据
combined_df <- rbind(graph1_df, graph2_df, graph3_df, graph4_df)
# 使用ggraph包绘制分面图
plot <- ggraph(graph_from_data_frame(combined_df, directed = FALSE)) + 
  geom_edge_link(aes(color = Row), show.legend = FALSE) +
  geom_node_point() +
  facet_grid(Row ~ Column) +
  theme_void()

# 显示图
print(plot)
