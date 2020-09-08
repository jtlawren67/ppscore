#' @title Generate Predictive Power Score for all variable in a data.frame
#'
#' @description Calculate the Predictive Power Score (PPS) for all columns in the dataframe.  Data is returned in long-format
#' @param df A data.frame the data to be analyzed
#' @return A data.frame containing 1 row for each PPS calculation:
#' * The Target Variable
#' * The Feature Variable
#' * The Task Name
#' * The Predictive Power Score
#' * The Evaluation metric
#' * The Baseline Score (Evaluate Metric vs. a Zero-R Classifer/Median)
#' * The Model Score
#' @importFrom purrr map2_dfr
#' @keywords zuh
#' @export
#' @examples
#' data(titanic_example)
#' pps_matrix(titanic_example)

pps_matrix <- function(df){
  #Returns a Data Frame Containing all the combinations of PPS Scores for Variables
  #Data is Returned in a Long Format

  #Get All Comparisons
  name_combos <- expand.grid(
    target = names(df),
    feature = names(df)
  )

  results <- map2_dfr(name_combos$target, name_combos$feature, function(target, feature){
    res = tryCatch({
      message(paste0(Sys.time(), " Running target:", target, "/feature:", feature))
      res =  ppscore(df, target, feature)
      message(paste0(Sys.time(), " Completed target:", target, "/feature:", feature, " as task ", res$task_name))
      return(res)
    },
    error = function(e){
      warning(paste0("The combination target:", target, "/feature:", feature,
                     " has been skipped due to an error"))

      res = data.frame(
        target = target,
        feature = feature,
        task_name = "skipped",
        ppscore = 0,
        metric = "skipped",
        baseline_score = 0,
        model_score = 0
      )
      return(res)
    })
  })

  return(results)

}
