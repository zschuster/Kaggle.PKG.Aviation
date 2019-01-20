
# The goal of this function is to read in a file and split into two data.tables
# One with the independent variables and one with the dependent variable and id.

readAndSplit = function(path, target_name, name = "train"){
  
  file = data.table::fread(path)
  
  # if it is the test set, keep the id column in the response portion
  if (name == "test"){
  assign(x = "x_test",
           file[, !c(target_name, 'id'), with = FALSE],
           envir = parent.frame())
    assign("y_test",
           file[, c("id", target_name), with = FALSE],
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


# given a data.table and column names, coerce to desired classes

coerceClass = function(data, columns, fun = "as.factor", ...){
  
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
