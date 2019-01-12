## The purpose of this document is to clean and preprocess the training data
## before modeling


# load libraries ----
library(data.table)

readAndSplit(path = "data/train.csv",
             target_name = "event",
             name = "train")

# for now, remove the experiment as the test set has a totally different 
# experiment!
x_train[, experiment := NULL]

# coerce proper variables to categorical
coerceClass(x_train, c("crew", "seat"), fun = as.factor)




# create a test/validation set for hyperparamter tuning

