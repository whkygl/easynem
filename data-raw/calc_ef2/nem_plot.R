#' nem_plot
#' @description For visualization of nematode community data.
#' @param object ef2 or other types data.
#' @return An ggplot object.
#' @rdname ef2
#' @name ef2
#' @aliases nem_plot,ef2-method
#' @import ggplot2
#' @import ggalt
#' @import igraph
#' @import ggraph
#' @export
setMethod("nem_plot", signature("ef2"), function(object){
  # object = hehe
  result = object@result
  result = result[,-1]
  result2 = result |>
    dplyr::group_by(!!rlang::sym(names(result)[1]),!!rlang::sym(names(result)[2])) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)))
  result3 = result |>
    dplyr::group_by(!!rlang::sym(names(result)[1]),!!rlang::sym(names(result)[2])) |>
    dplyr::summarise(Use = stats::sd(U)/sqrt(dplyr::n()))
  result4 = merge(result2,result3, by = c(names(result)[1], names(result)[2]))
  result4_U = result4[,c(names(result4)[1], names(result4)[2], "U", "Use")]
  result4_U$U = format(result4_U$U, digits = 2, nsmall = 2)
  result4_U$Use = format(result4_U$Use, digits = 2, nsmall = 2)
  result4_U = result4_U |> dplyr::mutate(U = paste0("U = ", U, " (", "\u00B1", Use, ")"))
  result4_nodes = result4[,c(names(result4)[1], names(result4)[2], "OM", "BM", "HM", "FM")]
  result4_nodes$R = 600
  names(result4_nodes) = c(names(result4_nodes)[1], names(result4_nodes)[2], "Omnivores_carnivores", "Bacterivores", "Herbivores", "Fungivores", "Resources")
  result4_nodes_long = reshape2::melt(result4_nodes,id.vars = c(names(result4_nodes)[1], names(result4_nodes)[2]), variable.name = "Feeding", value.name = "Fresh_biomass")
  result4_edges = result4[,c(names(result4)[1],names(result4)[2], "fbo", "fho", "ffo", "frb", "frh", "frf")]
  result4_edges_long = reshape2::melt(result4_edges,id.vars = c(names(result4_nodes)[1],names(result4_nodes)[2]), variable.name = "group", value.name = "Energy_flow")
  result4_edges_long = result4_edges_long |>
    dplyr::mutate(group = dplyr::case_when(
      group == "fbo" ~ "Bacterivores-Omnivores_carnivores",
      group == "fho" ~ "Herbivores-Omnivores_carnivores",
      group == "ffo" ~ "Fungivores-Omnivores_carnivores",
      group == "frb" ~ "Resources-Bacterivores",
      group == "frh" ~ "Resources-Herbivores",
      group == "frf" ~ "Resources-Fungivores",
      TRUE ~ group  # 保留其他未匹配的值
    ))
  result4_edges_long = result4_edges_long |>
    tidyr::separate(group, into = c("from1", "to1"), sep = "-")
  result4_edges_long = result4_edges_long |> dplyr::mutate(from = paste0(!!rlang::sym("from1"), !!rlang::sym(names(result4)[1]), !!rlang::sym(names(result4)[2])))
  result4_edges_long = result4_edges_long |> dplyr::mutate(to = paste0(!!rlang::sym("to1"), !!rlang::sym(names(result4)[1]), !!rlang::sym(names(result4)[2])))
  result4_edges_long = result4_edges_long |> dplyr::select(!!rlang::sym("from"), !!rlang::sym("to"), dplyr::everything())
  nodes <- data.frame(
    id = c("Resources", "Bacterivores", "Herbivores", "Fungivores", "Omnivores_carnivores"),
    Feeding = c("Resources", "Bacterivores", "Herbivores", "Fungivores", "Omnivores_carnivores")
  )
  # nodes = dplyr::bind_rows(replicate(nrow(result4), nodes, simplify = FALSE))
  # nodes[[names(result4)[1]]] = sort(rep(result4[[1]], times = 5))
  # nodes[[names(result4)[2]]] = rep(unique(result4[[2]]), times = length(unique(result4[[1]]))*5)
  nodes = merge(nodes, result4_nodes_long, by = "Feeding")
  nodes = nodes |> dplyr::mutate(id = paste0(!!rlang::sym("id"), !!rlang::sym(names(result4)[1]), !!rlang::sym(names(result4)[2])))
  nodes$Feeding = factor(nodes$Feeding, levels = c("Bacterivores", "Fungivores", "Herbivores", "Resources", "Omnivores_carnivores"))
  result4_U = result4_U[, -which(names(result4_U) == "Use")]
  nodes = merge(nodes, result4_U, by = c(names(result)[1], names(result)[2]))
  # nodes = merge(nodes, result4_nodes_long, by = c(names(result4)[1], names(result4)[2], "Feeding"), all = TRUE)
  nodes = dplyr::select(nodes, id, dplyr::everything())
  nodes$Fresh_biomass = round(nodes$Fresh_biomass,2)
  nodes$label = nodes$Fresh_biomass
  nodes = nodes |> dplyr::mutate(label = ifelse(Feeding == "Resources", "R", label))
  order = rep(c("Bacterivores", "Fungivores", "Herbivores", "Omnivores_carnivores", "Resources"), times = nrow(nodes)/5)
  positions <- integer(length(order))
  used_indices <- logical(length(order))
  for (i in 1:length(order)){
    for(j in 1:length(order)){
      if(order[i] == nodes[["Feeding"]][j] && !used_indices[j]){
        positions[i] = j
        used_indices[j] <- TRUE
        found <- TRUE
        break
      }
    }
  }
  nodes = nodes[positions,]
  # edges = dplyr::bind_rows(replicate(nrow(result4), edges, simplify = FALSE))
  # edges[[names(result4)[1]]] = sort(rep(result4[[1]], times = 6))
  # edges[[names(result4)[2]]] = rep(unique(result4[[2]]), times = length(unique(result4[[1]]))*6)
  # edges = edges |> dplyr::mutate(from = paste0(!!rlang::sym("from"), !!rlang::sym(names(result4)[1]), !!rlang::sym(names(result4)[2])))
  # edges = edges |> dplyr::mutate(to = paste0(!!rlang::sym("to"), !!rlang::sym(names(result4)[1]), !!rlang::sym(names(result4)[2])))
  # edges = merge(edges, result4_U, by = c(names(result)[1], names(result)[2]))
  # edges = merge(edges, result4_edges_long, by = c("from", "to"), all = TRUE)
  edges = result4_edges_long
  edges$Energy_flow = round(edges$Energy_flow, 2)
  graph = tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE, node_key = "id")
  # graph <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  layout_matrix <- matrix(c(
    0, 3,   
    3, 0,
    1.5, 1.5,
    3, 3, 
    0, 0 
  ), ncol = 2, byrow = TRUE)
  formu = stats::as.formula(paste0(names(result4)[2], "~", names(result4)[1]))
  p3 = ggraph::ggraph(graph, layout = layout_matrix) + 
    ggraph::geom_edge_fan(ggplot2::aes(colour = to1, width = Energy_flow, label = Energy_flow), family = NA, show.legend = FALSE) +
    ggraph::scale_edge_width(range=c(0.5, 4)) + 
    ggplot2::geom_text(ggplot2::aes(x = 2, y = 0.5, label = U),family = NA, data = nodes)+
    ggraph::geom_node_point(ggplot2::aes(colour = Feeding, size = Fresh_biomass)) +
    ggraph::geom_node_text(ggplot2::aes(label = label), repel = TRUE) +
    ggraph::theme_graph(foreground = 'steelblue', fg_text_colour = 'white', base_family = NA) + 
    ggplot2::scale_size(range = c(5,13)) +
    ggplot2::facet_grid(formu, scales = "free")
  p3
})

devtools::install_github("whkygl/easynem")
library(easynem)
bac <- read_nem(tab = easynem_example("nemotu.csv"), 
                tax = easynem_example("nemtax.csv"), 
                meta = easynem_example("meta.csv"))
hehe <- bac |> calc_nemindex() |> calc_ef2(con_crop, season)
hehe
hehe2 <- nem_plot(hehe)
hehe2
