rm(list = ls())
trans_norm <- function(data, method, MARGIN = 2, ...){
  method = deparse(substitute(method))
  data@tab[,-1] = vegan::decostand(data@tab[,-1], method = method, MARGIN = MARGIN, ...)
  return(data)
}
# hehe2 = trans_norm(data = hehe, method = total)
trans_rare <- function(data, sample = 0, ...){
  # data = hehe
  # sample = 0
  resulttab = data@tab
  resulttax = data@tax
  resultmeta = data@meta
  nametab = colnames(data@tab)[1]
  nametax = colnames(data@tax)[1]
  namemeta = colnames(data@meta)[1]
  data@tab = as.data.frame(data@tab)
  rownames(data@tab) <- data@tab[[1]]
  if (sample == 0) {
    sample = min(colSums(data@tab[,-1]))
    data@tab = tibble::as_tibble(tibble::rownames_to_column(as.data.frame(t(vegan::rrarefy(t(data@tab[ ,-1]), sample))), var = nametab))
  } else if (sample > 0){
    suppressWarnings(result <- as.data.frame(t(vegan::rrarefy(t(data@tab[ ,-1]), sample))))
    warning("All samples with less than ", sample, " species will be deleted\n")
    data@tab <- tibble::as_tibble(tibble::rownames_to_column(result[ , colSums(result) >= sample], var = nametab))
  } else {
    stop("sample should be a positive number")
  }
  resulttax = resulttax[resulttax[[nametax]] %in% data@tab[[nametab]], ]
  data@tax = resulttax
  resultmeta = resultmeta[resultmeta[[namemeta]] %in% colnames(data@tab[,-1]), ]
  data@meta = resultmeta
  return(data)
}
# hehe3 <- trans_rare(hehe, 20000)
# hehe4 <- trans_rare(hehe, 0)
trans_formula <- function(data, var, formu, ...){
  var = deparse(substitute(var))
  resultmeta = data@meta
  suppressWarnings(resultmeta[[var]] <-  eval(formu[[2]], list(x = data@meta[[var]]),environment(formu)))
  data@meta = resultmeta
  return(data)
}
# hehe2 <- trans_formula(hehe,pH,~1/x)
#' Title
#'
#' @param data 
#' @param num 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
filter_num  <- function(data, num, ...){
  # 提取第一列的列名
  result = data@tab
  colname = colnames(result)[1]
  result2 = data@tax
  colname2 = colnames(result2)[1]
  if(num >= 0 && num < 1){
    #删除发现率小于num的行
    result = result[rowSums(result[,-1] != 0) / ncol(result[,-1]) >= num, ]
  } else if(num >= 1){
    #删除总计数小于num的行
    result = result[rowSums(result[,-1]) >= num, ]
  } else {
    stop("num should be a positive number")
  }
  # 在result2中只保留result中OTUID对应的行
  result2 = result2[result2[[colname2]] %in% result[[colname]], ]
  data@tab = result
  data@tax = result2
  return(data)
}
# hehe2 <- filter_num(hehe, num = 100)
filter_name <- function(data, target = meta, ...){
  # data = hehe
  # season == "Summer"
  # 元编程无引号参数
  target = deparse(substitute(target))
  if(target == 'meta'){
    result = data@meta
    colname = colnames(result)[1]
    result = dplyr::filter(result, ...)
    data@meta = result
    result2 = data@tab
    # result2中只保留result中第一列的名称对应的列
    # result3 = result2[,-1][,colnames(result2[,-1]) %in% result[["SampleID"]]]
    result3 = result2[,-1][,colnames(result2[,-1]) %in% result[[colname]]]
    # 将result2中的第一列和result3合并
    result2 = cbind(result2[,1], result3)
    data@tab = result2
  } else if(target == 'tax'){
    result = data@tax
    colname = colnames(result)[1]
    result = dplyr::filter(result, ...)
    data@tax = result
    result2 = data@tab
    colname2 = colnames(result2)[1]
    # result2中只保留result中第一列的名称对应的行
    result2 = result2[result2[[colname2]] %in% result[[colname]], ]
    data@tab = result2
  } else if(target == 'tab'){
    result = data@tab
    colname = colnames(result)[1]
    result = dplyr::filter(result, ...)
    data@tab = result
    result2 = data@tax
    colname2 = colnames(result2)[1]
    result3 = data@meta
    colname3 = colnames(result3)[1]
    # result2中只保留result中第一列的名称对应的行
    result2 = result2[result2[[colname2]] %in% result[[colname]], ]
    data@tax = result2
    # result3中只保留result中列名对应的行
    result3 = result3[result3[[colname3]] %in% colnames(result[,-1]), ]
    data@meta = result3
  } else{
    stop("target should be one of 'meta', 'tax' and 'tab'")
  }
  return(data)
}
# 整合函数定义
nem_trans <- function(data, f, ...){
  result = f(data, ...)
  return(result)
}
# hehe2 <- nem_trans(hehe, filter_num, num = 100)
# hehe3 <- nem_trans(hehe, filter_name, target = meta, season == "Summer")
# hehe4 <- nem_trans(hehe, filter_name, target = tax, Phylum == "Proteobacteria")
# hehe5 <- nem_trans(hehe, filter_name, target = tab, OTUID == "ASV_6941")