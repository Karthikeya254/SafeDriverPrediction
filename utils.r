#Compute mode of the given vector
getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

#Count NA and Percentage NA in each column
na_summary <- function(x) {
  count_na = length(which(is.na(x)))
  perc = round(100*(length(which(is.na(x))))/length(x), digits = 2)
  return(list(count_na, perc))
}

#Predict NA values using linear regression on a correlated variable
predictNAfromLM <- function(x, ind_var, dep_var) {
  data_ind_dep = subset(x, select = c(ind_var, dep_var))
  na_ids_ind = which(is.na(data_ind_dep[, ind_var]))
  na_ids_dep = which(is.na(data_ind_dep[, dep_var]))
  total_na_ids = union(na_ids_ind, na_ids_dep)
  predictable_ids = setdiff(na_ids_dep, na_ids_ind)
  
  if(length(na_ids_ind) >= length(na_ids_dep)) {
    cat("not enough predictor observations...\n")
  }
  
  lm.fit = lm(formula(paste(dep_var,"~", ind_var)), data = data_ind_dep[-total_na_ids,])
  pred_dep_values = predict(lm.fit, data_ind_dep[predictable_ids,], type = "response")
  return(list(ids = predictable_ids, values = pred_dep_values))
}