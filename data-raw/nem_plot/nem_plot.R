setGeneric("nem_plot", function(object,...){
  standardGeneric("nem_plot")
})
setMethod("nem_plot", signature("beta"), function(object, level = 0.6, type = 1){
  # object = pca
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
    p = ggplot2::ggplot(meta,aes(x=NMDS1,y=NMDS2,color=!!sym(colnames(group))))+
      ggplot2::labs(x=colnames(NMDS1),y = colnames(NMDS2), title=temp)+geom_point(size=4)+ 
      ggplot2::scale_color_manual(values = my_color)+
      ggplot2::theme(legend.title = ggplot2::element_blank())+
      ggplot2::theme_test() + ggplot2::coord_fixed(1)
    if (level > 0){
      if (type == 1){
        p = p+ggplot2::stat_ellipse(level=level)
      } else if(type == 2){
        p = p+geom_encircle(aes(fill=!!sym(colnames(group))), alpha = 0.1, show.legend = F)
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
      p = ggplot2::ggplot(meta,aes(x=PCoA1,y=PCoA2,color=!!sym(colnames(group))))
    } else {
      p = ggplot2::ggplot(meta,aes(x=PC1,y=PC2,color=!!sym(colnames(group))))
    }
    p = p+ggplot2::labs(x = temp[1], y=temp[2], title=temp[3])+geom_point(size=4)+ 
      ggplot2::scale_color_manual(values = my_color)+
      ggplot2::theme(legend.title = ggplot2::element_blank())+
      ggplot2::theme_test() + ggplot2::coord_fixed(1)
    if (level > 0){
      if (type == 1){
        p = p+ggplot2::stat_ellipse(level=level)
      } else if(type == 2){
        p = p+geom_encircle(aes(fill=!!sym(colnames(group))), alpha = 0.1, show.legend = F)
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
# p = nem_plot(pcoa,level = 0.6)
# p = nem_plot(pcoa,type = 2)
# p = nem_plot(nmds,type = 2)
# p = nem_plot(pca)