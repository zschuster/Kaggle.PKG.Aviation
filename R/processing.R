
#' Coerce select columns to a certain class
#'
#' @param data A data.table
#'
#' @param columns A character vector of columns to coerce
#'
#' @param fun A function to coerce columns. Default is as.factor
#'
#' @return a data.table with columns specified in \code{columns} coerced to a
#'  certain class
#'   
#' @export

coerceClass = function(data, columns, fun = "as.factor"){
  
  stopifnot(data.table::is.data.table(data))
  
  data = data.table::copy(data)
  func = match.fun(fun)
  
  for(name in columns){
    data.table::set(
      data, j = name, value = func(data[[name]])
    )
  }
  
  return(data)
  
}


#' Code a factor or character vector as integer
#'
#' @param fac A factor, typically "non-numeric" levels.
#'
#' @return A converted integer vector. Values start at 0.
#'   
#' @export

multiCodeVars = function(fac){
  
  stopifnot(is.factor(fac) || is.character(fac))
  
  fac = as.factor(fac)
  levs = levels(fac)
  vec = integer(length(fac))
  
  for (i in seq_along(levs)) {
    vec[which(fac == levs[i])] = i - 1
  }
  
  return(vec)
}



#' Format prediction probabilities produced by xgboost model
#'
#' @param xgbMod A model produced using xgboost package
#'
#' @param newdat New data of class "xgb.DMatrix"
#'
#' @param classes A vector of class labels
#'
#' @return a matrix of probabilities with dimensions \code{[nrow(newdat), length(classes)]}
#'   
#' @export


formatXgbProbs = function(xgbMod, newdat, classes = LETTERS[1:4]){
  
  # error checking
  stopifnot(inherits(newdat, "xgb.DMatrix"))
  
  # get predictions from model on new data
  probs = predict(xgbMod, newdat)
  
  # create a matrix of predictions
  prob_mat = matrix(probs, ncol = length(classes), byrow = TRUE)
  colnames(prob_mat) = classes
  
  return(prob_mat)
}
