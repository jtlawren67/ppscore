CV_ITERATIONS = 4
RANDOM_SEED = 587136
NUMERIC_AS_CATEGORIC_BREAKPOINT = 15

.cv_score <- function(df, target, feature, task_name){
  df = sample(df, replace = F)

  #Create Samples
  fold <- NULL
  df$fold <- sample(c(1:CV_ITERATIONS), nrow(df), replace = T)

  if(task_name == 'classification'){
    df[, target] = as.factor(df[, target])
  }

  ###Do Cross Validating
  cv_results <- purrr::map_dbl(1:CV_ITERATIONS, function(i){
    # Build Model
    rp <- rpart::rpart(as.formula(paste0(target,"~",feature)), data = df, subset = (fold != i))
    # Predict
    if(task_name == 'classification'){
      pred = predict(rp, df[df$fold==i, ], type="class")
      score = .f1(df[df$fold==i, target], pred)
      return(score)
    }else{
      pred = predict(rp, df[df$fold==i, ])
      score = .mae(df[df$fold==i, target], pred)
      return(score)
    }
  }
  )

  return(mean(cv_results))

}

.mae <- function(actual, predicted){
  return(mean(abs(actual-predicted)))
}

###This is the Weighted F1 Score From SciKit Learn Where
###Each Class gets an F1 Score and its weighted by number of
###Instances
.f1 <- function(actual, predicted){

  res <- purrr::map_dfr(unique(actual), function(level){
    precision = (sum(actual == level & predicted == level)) / (sum(predicted == level))
    recall = (sum(actual == level & predicted == level)) / (sum(actual == level))
    f1 = (2*precision*recall)/(precision+recall)
    weight = sum(actual == level)/length(actual)
    return(data.frame(
      precision = ifelse(is.nan(precision), 0, precision),
      recall = ifelse(is.nan(recall), 0, recall),
      f1 = ifelse(is.nan(f1), 0, f1),
      weight = weight
    ))
  })

  weighted_f1 = sum(res$weight*res$f1)

  return(weighted_f1)
}


.maybe_sample <- function(df, sample_val){
  #Sample the rows of the given df to have at most X rows.
  #If set to none or if samples < rows, then no sampling will be done.

  if(!is.na(sample_val) & nrow(df) > sample_val){
    set.seed(RANDOM_SEED)
    df = df[sample(nrow(df), sample_val), ]
  }

  return(df)
}

.infer_task <- function(df, target, feature){
  #Returns string with name of the inferred tasks based on X and Y

  if(target==feature){
    return("predict_itself")
  }

  category_count = length(unique(df[, target]))

  if(category_count==1){
    return("predict_constant")
  }

  if((length(unique(df[, feature])) > NUMERIC_AS_CATEGORIC_BREAKPOINT) & (
    is.character(df[, feature][[1]]) | is.factor(df[, feature][[1]]))){
    return("too_many_feature_levels")
  }

  if(category_count == 2){
    return("classification")
  }

  if((category_count > NUMERIC_AS_CATEGORIC_BREAKPOINT) & (
    is.character(df[, target][[1]]) | is.factor(df[, target][[1]]))){
    return("predict_id")
  }


  if(category_count <= NUMERIC_AS_CATEGORIC_BREAKPOINT){
    return("classification")
  }

  if(is.numeric(df[, target][[1]])){
    return("regression")
  }

  stop(paste("Could not infer a valid task. The data type",class(df[, target][[1]])))
}

.normalized_mae_score <- function(model_mae, naive_mae){
  if(model_mae > naive_mae){
    return(0)
  }else{
    1 - (model_mae / naive_mae)
  }
}

.mae_normalizer <- function(df, target, model_score){
  df$naive = quantile(df[, target], .5)
  baseline_score = .mae(df[, target], df$naive)

  ppscore = .normalized_mae_score(abs(model_score), baseline_score)

  return(
    list(ppscore, baseline_score)
  )
}

.normalized_f1_score <- function(model_f1, baseline_f1){
  if(model_f1 < baseline_f1){
    return(0)
  } else {
    scale_range = 1.0 - baseline_f1
    f1_diff = model_f1 - baseline_f1
    return(f1_diff / scale_range)
  }
}

.f1_normalizer <- function(df, target, model_score){
  df$naive = as.factor(names(sort(table(df[, target]), decreasing = T)[1]))
  baseline_score = .f1(df[, target], df$naive)
  ppscore = .normalized_f1_score(model_score, baseline_score)

  return(list(ppscore, baseline_score))
}
