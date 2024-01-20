#' nem_plot
#' 
#' This is a generic function for visualization.
#' @param object easynem or other types data.
#' @param ... Other parameters.
#' @export
setGeneric("nem_plot", function(object,...){
  standardGeneric("nem_plot")
})
#' nem_plot
#' @description For visualization of nematode community data.
#' @param object easynem or other types data.
#' @param level Used to adjust the size of the confidence ellipse.
#' @param type Method used to adjust the display of scatter area.
#' @param ... Other parameters for ggplot2::stat_ellipse and ggalt::geom_encircle functions.
#' @return An ggplot object.
#' @rdname beta
#' @name beta
#' @aliases nem_plot,beta-method
#' @import ggplot2
#' @import ggalt
#' @export
setMethod("nem_plot", signature("beta"), function(object, level = 0.6, type = 1, ...){
  my_color <- c("#4f72d2","#E64B35","#4DBBD5","#F2A900","#00A087","#3C5488",
                "#F39B7F","#0096b0","#DC0000","#c06e26",
                "#b5426a")
  meta = object@meta
  result = object@result
  temp = object@temp
  if (length(temp) == 1){
    NMDS1 = meta[,3]
    NMDS2 = meta[,4]
    group = meta[,2]
    p = ggplot2::ggplot(meta,ggplot2::aes(x=NMDS1,y=NMDS2,color=!!dplyr::sym(colnames(group))))+
      ggplot2::labs(x=colnames(NMDS1),y = colnames(NMDS2), title=temp)+ggplot2::geom_point(size=4)+ 
      ggplot2::scale_color_manual(values = my_color)+
      ggplot2::theme(legend.title = ggplot2::element_blank())+
      ggplot2::theme_test() + ggplot2::coord_fixed(1)
    if (level > 0){
      if (type == 1){
        p = p+ggplot2::stat_ellipse(level=level)
      } else if(type == 2){
        p = p+ggalt::geom_encircle(ggplot2::aes(fill=!!dplyr::sym(colnames(group))), alpha = 0.1, show.legend = F)
      } else {
        stop("Type value must be 1 or 2.")
      }
    } else if (level == 0){
      p
    } else {
      stop("Invalid level value.")
    }
  } else {
        PCoA1 = meta[,3]
        PCoA2 = meta[,4]
        group = meta[,2]
    if (colnames(PCoA1)=="PCoA1"){
        p = ggplot2::ggplot(meta,ggplot2::aes(x=PCoA1,y=PCoA2,color=!!dplyr::sym(colnames(group))))
    } else {
        PC1 = meta[,3]
        PC2 = meta[,4]
        group = meta[,2]
        p = ggplot2::ggplot(meta,ggplot2::aes(x=PC1,y=PC2,color=!!dplyr::sym(colnames(group))))
    }
    p = p+ggplot2::labs(x = temp[1], y=temp[2], title=temp[3])+ggplot2::geom_point(size=4)+ 
      ggplot2::scale_color_manual(values = my_color)+
      ggplot2::theme(legend.title = ggplot2::element_blank())+
      ggplot2::theme_test() + ggplot2::coord_fixed(1)
    if (level > 0){
      if (type == 1){
        p = p+ggplot2::stat_ellipse(level=level)
      } else if(type == 2){
        p = p+ggalt::geom_encircle(ggplot2::aes(fill=!!dplyr::sym(colnames(group))), alpha = 0.1, show.legend = F)
      } else {
        stop("Type value must be 1 or 2.")
      }
    } else if (level == 0){
      p
    } else {
      stop("Invalid level value.")
    }
  }
})