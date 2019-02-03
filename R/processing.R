
#' Read in training or test data
#'
#' @param path A string representing a file path.
#'
#' @param target_name Name of response column.
#'
#' @param name Either train or test to denote type of data.
#'
#' @return Assigns two objects to the parent frame. An data.table of independent
#'   variables and a data.table with dependent variable.
#'   
#' @export


readAndSplit = function(path, target_name, name = "train"){
  
  file = data.table::fread(path)
  
  # if it is the test set, keep the id column in the response portion
  if (name == "test"){
    assign("x_test",
           file[, !'id', with = FALSE],
           envir = parent.frame())
    assign("y_test",
           file[, "id", with = FALSE],
           envir = parent.frame())
  }
  
  # if it isn't the test set, no need to worry about the id column (not there)
  assign(paste("x", name, sep = "_"),
         file[, setdiff(names(file), target_name), with = FALSE],
         envir = parent.frame())
  assign(paste("y", name, sep = "_"),
         file[, target_name, with = FALSE, drop = FALSE],
         envir = parent.frame())
}


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


#' Process training data
#'
#' @param data A data.table of independent variables
#'
#' @param y A data.table with one columns, your dependent variable
#'
#' @return A list containing the following elements
#'  \itemize{
#'  \item \strong{x_train} a processed training data.table
#'  \item \strong{y_train} a data.table with response variable matching x_train index
#'  \item \strong{scale_means} a vector of means used to scale training data
#'  \item \strong{scale_sds} a vector of standard deviations used to scale training data
#'  \item \strong{scale_cols} a character vector of columns that were scaled
#'  }
#' 
#' @export

processTrainData = function(data, y){
  
  stopifnot(data.table::is.data.table(data))
  stopifnot(nrow(data) == nrow(y))
  
  dat = data.table::copy(data)
  
  dat[, experiment := NULL]
  
  dat[, time_bin := cut(time, breaks = c(0, 90, 180, 270, 365),
                        labels = FALSE)]
  
  # coerce proper variables to categorical
  dat = coerceClass(dat, c("crew", "seat", "time_bin"),
                    fun = as.factor)
  
  # get the numeric columns (exluding time)
  cols = names(dat)[vapply(dat, is.numeric,
                           FUN.VALUE = logical(1))]
  
  # remove time 
  cols = cols[cols != "time"]
  
  # calculate vector of lower and upper bounds for each numeric column
  lb = vapply(dat[, ..cols], function(x) mean(x) - 4*sd(x),
              FUN.VALUE = numeric(1))
  ub = vapply(dat[, ..cols], function(x) mean(x) + 4*sd(x),
              FUN.VALUE = numeric(1))
  
  # get index of observations to remove
  ind = apply(dat[, ..cols], 1, function(x){
    return(!any(x < lb | x > ub))
  })
  
  # remove rows containing outliers
  dat = dat[ind]
  y = y[ind]
  
  # now put time back into cols
  cols = c(cols, "time")
  
  # scale numeric variables
  means = vapply(dat[, ..cols], mean, FUN.VALUE = numeric(1L))
  sds = vapply(dat[, ..cols], sd, FUN.VALUE = numeric(1L))
  
  dat[, (cols) := mapply(
    function (mean, sd, vec) {
      return((vec - mean) / sd)
    },
    mean = means,
    sd = sds,
    vec = .SD,
    SIMPLIFY = FALSE
  ),
  .SDcols = cols]

  # code variables to be numeric
  char_vars = names(dat)[sapply(dat, is.factor)]
  dat = dat[, (char_vars) := lapply(.SD, multiCodeVars),
            .SDcols = char_vars]
  
  
  # return processed data and lda model
  return(
    list(x_train = dat,
         y_train = y,
         scale_means = means,
         scale_sds = sds,
         scale_cols = cols
    )
  )
  
}


#' Format test data. Part of workflow with processTrainData
#'
#' @param dat A data.table containing test data
#'
#' @param sc_means A vector of means produced by processTrainData
#' 
#' @param sc_sds A vector of standard deviations produced by processTrainData
#'
#' @param scale_cols A vector of columns to scale produced by processTrainData
#'
#' @return a data.table containing the processed data to make predictions with
#'   
#' @export

processTestData = function(dat, sc_means, sc_sds, scale_cols){ #, lda_model, lda_cols
  
  # dat = data.table::copy(data)
  data.table::setDT(dat)
  
  # get rid of experiment column
  dat[, experiment := NULL]
  
  dat[, time_bin := cut(time, breaks = c(0, 90, 180, 270, 365),
                        labels = FALSE)]
  
  cols = c("crew", "seat", "time_bin")
  # coerce classes to factor to code them properly
  dat[, (cols) := lapply(.SD, as.factor),
      .SDcols = cols]
  
  # preds = predict(lda_model, dat[, ..lda_cols])$posterior
  # 
  # # bind predictions to test data
  # dat = cbind(dat, preds)
  
  # scale numeric columns the same as train
  dat[, (scale_cols) := mapply(
    function (mean, sd, vec) {
      return((vec - mean) / sd)
    },
    mean = sc_means,
    sd = sc_sds,
    vec = .SD,
    SIMPLIFY = FALSE
  ),
  .SDcols = scale_cols]
  
  
  # code nominal variables to start at 0
  char_vars = names(dat)[sapply(dat, is.factor)]
  dat[, (char_vars) := lapply(.SD, multiCodeVars),
      .SDcols = char_vars]
  
  return(dat)
}

