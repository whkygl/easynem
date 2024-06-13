# devtools::install_github("whkygl/easynem")
library(easynem)
bac <- read_nem(tab = easynem_example("bacotu.csv"), 
                tax = easynem_example("bactax.csv"), 
                meta = easynem_example("meta.csv"))
hehe <- bac |> filter_name(meta, con_crop %in% c("Y2","Y11")) |> calc_compare2(.group1 = con_crop, .group2 = season, y = pH, method = TTest2)
hehe
setMethod("nem_plot", signature("compare2"), function(object, type1 = 1, type2 = 1, add, ...){
  # object = hehe
  meta = object@meta
  temp = object@temp
  result = object@result
  if(temp == "TTest2"){
    if (type1 == 1){
      if (type2 == 1){
        p = ggplot2::ggplot(meta, ggplot2::aes(x = !!rlang::sym(names(meta)[2]), y = !!rlang::sym(names(meta)[4]), fill = !!rlang::sym(names(meta)[3]))) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = !!rlang::sym(names(meta)[3]))) + ggplot2::geom_jitter(ggplot2::aes(fill = !!rlang::sym(names(meta)[3])), shape = 21,position = ggplot2::position_jitterdodge())
        p
      } else if (type2 == 2){
        
      }
    } else if (type1 == 2){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    }
  } else if (temp == "WilcoxTest2"){
    if (type1 == 1){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    } else if (type1 == 2){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    }
  } else if (temp == "KruskalTest2"){
    if (type1 == 1){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    } else if (type1 == 2){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    }
  } else if (temp == "LSD2"){
    if (type1 == 1){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    } else if (type1 == 2){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    }
  } else if (temp == "HSD2"){
    if (type1 == 1){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    } else if (type1 == 2){
      if (type2 == 1){
        
      } else if (type2 == 2){
        
      }
    }
  } 
})
        