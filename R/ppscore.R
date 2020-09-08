#' @title Generate Predictive Power Score (PPS) for "feature predicts target"
#'
#' @description This function calculates the Predictive Power Score for "feature predicts target"
#' The score always ranges from 0 to 1 and is data-type agnostic
#' A score of 0 means that the column x cannot predict the column y better than a naive baseline model.
#' A score of 1 means that the column x can perfectly predict the column y given the model.
#' A score between 0 and 1 states the ratio of how much potential predictive power the model achieved compared to the baseline model.
#' @param df A data.frame containing the target and feature variables
#' @param target The target of the relationship. What we want to explain. The dependent variable.
#' @param feature The feature that explains the target.  The independent variable.
#' @param task Name of the prediction task, e.g. classification or regression. If the task is not specified, it is infered based on the y column The task determines which model and evaluation score is used for the PPS
#' @param sample_val Number of rows for sampling. The sampling decreases the calculation time of the PPS. If None there will be no sampling. Default is 5000.
#' @return A data.frame containing:
#' * The Target Variable
#' * The Feature Variable
#' * The Task Name
#' * The Predictive Power Score
#' * The Evaluation metric
#' * The Baseline Score (Evaluate Metric vs. a Zero-R Classifer/Median)
#' * The Model Score
#' @keywords zuh
#' @importFrom rpart rpart
#' @importFrom purrr map_dfr map2_dfr map_dbl
#' @importFrom stats as.formula complete.cases predict quantile
#' @export
#' @examples
#' data(titanic_example)
#' ppscore(titanic_example, "Sex", "Survived")



ppscore <- function(df, target, feature, task=NA, sample_val = 5000){
  #Calculate the Predictive Power Score (PPS) for "x predicts y"
  #Score ranges from 0 to 1 and is data-type agnostic

  #A score of 0 means the column x cannot predict the column y better than a naive baseline model
  #A score of 1 means that the column x can perfectly predict the column y given the model
  #A score between 0 and 1 states the ratio of how much predictive power the model has compared to the baseline model

  #Parameters
  #=================
  #df = Dataframe taht contains the column x and y
  #x = Name of column x which acts as the feature
  #y = Name of column y which acts as the target
  #task = Name of prediction task ('classification', 'regression').  If nothing is specified, it will be inferred based on the y column.
  #sample = Number of rows for sampling.  The sampling decreases the time of the PPS

  #Returns
  #===================

  if(target==feature){
    task_name = "predict_itself"
  } else{
    df = df[, c(target, feature)]
    df = df[complete.cases(df),]
    if(nrow(df)==0){
      stop("After dropping missing values, there are no valid rows left")
    }

    df = .maybe_sample(df, sample_val)

    if(is.na(task)){
      task_name = .infer_task(df, target, feature)
    }
    else{
      task_name = task
    }
  }

  if(task_name %in% c('predict_constant', 'predict_itself')){
    model_score = 1
    ppscore = 1
    baseline_score = 1
  }else if(task_name %in% c('predict_id', 'too_many_feature_levels')){
    model_score = 0
    ppscore = 0
    baseline_score = 0
  }else{
    #Model Scoring
    model_score = .cv_score(df, target, feature, task_name)
    if(task_name == "classification"){
      scores = .f1_normalizer(df, target, model_score)
    }else if(task_name == "regression"){
      scores = .mae_normalizer(df, target, model_score)
    }
    ppscore = scores[[1]]
    baseline_score = scores[[2]]
  }

  return(
    data.frame(
      target = target,
      feature = feature,
      task_name = task_name,
      ppscore = ppscore,
      metric = ifelse(task_name=="classification", "f1", "mae"),
      baseline_score = baseline_score,
      model_score = abs(model_score)
    )
  )
}
