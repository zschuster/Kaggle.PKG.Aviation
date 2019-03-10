
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
