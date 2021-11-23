f1_scores <- function(levels, table){
  num_levels = length(levels)
  
  scores <- vector()
  
  for (i in 1:num_levels){
    precision <- table[i,i] / sum(table[i,])
    recall <- table[i,i] / sum(table[,i])
    
    scores[[i]] <- (2 * precision * recall) / (precision + recall)
  }
  scores
}

f1_score_macro <- function(levels, table){
  mean(f1_scores(levels, table))
}

# sum of sum(table[i,]) is the count of actual values, which can be used for the weight
# https://en.wikipedia.org/wiki/Weighted_arithmetic_mean
#
f1_score_weighted <- function(levels, table){
  num_levels = length(levels)
  
  averages_by_weight = 0
  sum_of_weights = 0
  for (i in 1:num_levels){
    weight = sum(table[i,])
    
    precision <- table[i,i] / weight
    recall <- table[i,i] / sum(table[,i])
    
    score = (2 * precision * recall) / (precision + recall)
    
    averages_by_weight = averages_by_weight + (weight * score)
    
    sum_of_weights = sum_of_weights + weight
  }
  averages_by_weight / sum_of_weights
}

f1_score_micro <- function(levels, table){
  num_levels = length(levels)
  
  total_true_positives = 0
  total_false_positives = 0
  total_false_negatives = 0
  
  for (i in 1:num_levels){
    current_true_positive = table[i,i]
    current_false_positive = sum(table[i,]) - current_true_positive
    current_false_negative = sum(table[,i]) - current_true_positive
    
    total_true_positives = total_true_positives + current_true_positive
    total_false_positives = total_false_positives + current_false_positive
    total_false_negatives = total_false_negatives + current_false_negative
  }
  
  precision = total_true_positives / (total_true_positives + total_false_positives)
  recall = total_true_positives / (total_true_positives + total_false_negatives)
  
  (2 * precision * recall) / (precision + recall)
}