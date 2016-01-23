DESCRIPTION <- "find the enumeration that changed the most significantly between years"

#' Identify which enumeration have changed most year-to-year in a verisr object
#' NOTE: df *must contain* timeline.incident.year.  If plus.dbir_year exists, it will be used.
#' 
#' @param df A verisr object of logical columns to analyze.
#' @param current_year The year of interest. Defaults to this year.
#' @param last_year The year to compare to. Defaults to last year.
#' @param filter Bool.  If TRUE, only 5th & 95th percentile results in at least 1/1000th of the records will be shown
#' @return An ordered data frame of the difference and ratio of the percentage of each enumeration in \code{current_year} vs \code{last_year} as well as the count of the enumeration in the current_year.
#' @examples
#'   incidents <- vz %>% select(timeline.incident.year,
#'                              contains("variety"), 
#'                              contains("vector"),
#'                              starts_with("attribute.confidentiality.data_disclosure"), 
#'                              starts_with("data_discovery"), 
#'                              matches("^victim.(industry2.|employee_count|orgsize)"), 
#'                              matches("timeline.*.unit.*"))
#'   breaches <- incidents %>% filter(attribute.confidentiality.data_disclosure.Yes)
#'   y2y <- year2year(breaches)
year2year <- function(df, current_year=NULL, last_year=NULL, filter=TRUE) {
  # I'm sure I'm doing something wrong by writing this but it's helpful for comparing the ratios
  abs_ratio <- function(x) {x[x < 1] <- 1/x[x < 1]; x}
  
  # Set the years to compare
  if (is.null(current_year)) {
    current_year <- format(Sys.Date(), "%Y")
  }

  if (is.null(last_year)) {
    last_year <- as.character(as.numeric(current_year) - 1)
  }
 
  # Year based on dbir year if possible
  if ("plus.dbir_year" %in% colnames(df) & as.numeric(current_year) >= 2009) {
    current_df <- df %>% filter(plus.dbir_year == as.character(current_year))
  } else {
    current_df <- df %>% filter(timeline.incident.year == as.character(as.numeric(current_year) - 1))
  }
  
if (nrow(current_df) == 0) {
  throw("No data for current year!")
}
  
#  if ("plus.dbir_year" %in% colnames(df)) {
#    last_df <- df %>% filter(plus.dbir_year == last_year)
#  } else {
#    last_df <- df %>% filter(timeline.incident.year == as.character(as.numeric(last_year) - 1))
#  }
  # well, since no-one coded plus.dbir_year in the 2014 data, we'll use incident year
  last_df <- df %>% filter(timeline.incident.year == as.character(as.numeric(last_year) - 1))

  if (nrow(last_df) == 0) {
    throw("No data for last year!")
  }
  
  if ("plus.dbir_year" %in% colnames(df)) {  
    current_df <- current_df %>% select(-timeline.incident.year, -plus.dbir_year)
    last_df <- last_df %>% select(-timeline.incident.year, -plus.dbir_year)
  } else {
    current_df <- current_df %>% select(-timeline.incident.year)
    last_df <- last_df %>% select(-timeline.incident.year)
    
  }
    
  # get this year's breach percents
  current_df_pcts <- colSums(current_df) / nrow(current_df)
  
  # get last year's breach percents
  last_df_pcts <- colSums(last_df) / nrow(last_df)
  
    df_ratio <- current_df_pcts/last_df_pcts
    df_ratio[is.nan(df_ratio)] <- 1
  # going from ratio abs difference
  df_diff <- current_df_pcts - last_df_pcts 
  
  # Below line might be useful in dealing with infinits
#  df_ratio[is.infinite(df_ratio)] <- max(df_ratio) * 1.20
  
  # Visibly inespect breach_ratio for fun
  # breach_ratio[order(breach_ratio, decreasing=T)]
  # ggplot(melt(breach_ratio), aes(x=seq_along(value), y=value, size=sqrt(colSums(current_breaches)))) + geom_point(stat="identity")

  # Change to a data frame and add the record counts
  #    df_ratio <- df_ratio %>% melt() %>% add_rownames(var="feature")
  #    df_feature_cnts <- current_df %>% select(one_of(df_ratio$feature)) %>% colSums() %>% melt() %>% add_rownames(var="feature")
  #    df_ratio <- full_join(df_ratio, df_feature_cnts, by="feature")
  #    names(df_ratio) <- c("Feature", "Ratio", "Count")
  # turn the ratio into a dataframe
  ret <- df_ratio %>% melt()
  # bind the difference to it
  ret <- cbind(df_diff, ret)
  # set their names
  names(ret) <- c("Difference", "Ratio")
  # add the actual enumeration names
  ret <- ret %>% add_rownames(var="Enumeration")
  # Get the counts for each enumeration in the current year
  df_feature_cnts <- current_df %>% select(one_of(ret$Enumeration)) %>% colSums() %>% melt(value.name = "Count") %>% add_rownames(var="Enumeration")
  # Join the counts to the return dataframe
  ret <- full_join(ret, df_feature_cnts, by="Enumeration")
  
  if (filter) {  
    # Need to choose outliers
    # The data is long tailed so no mean/SD
    ratio_percentiles <- quantile(abs_ratio(df_ratio), c(0.97))
    diff_percentiles <- quantile(abs(df_diff), c(0.95))

    # Filter
#    df_ratio <- df_ratio[df_ratio > percentiles[1]]
    ret <- ret[abs_ratio(ret[["Ratio"]]) > ratio_percentiles[1] | abs(ret[["Difference"]]) > diff_percentiles[1], ]
        
    # Filter record counts at or below the 50th percentile (picked totally arbitrarily)
    #df_ratio %>% filter(Count > quantile(df_ratio$Count, c(.5))["50%"])
    # Filter record counts at or below 45 rather than the above quantile filtering.
    #  (We use 45 in the standard report)
    #df_ratio %>% filter(Count > 45)
    # Filter record counts at or or below 1/1000 of the total records
#    df_ratio <- df_ratio %>% filter(Count > nrow(df) / 1000)
  }
  
  # Order it
#  df_ratio <- df_ratio %>% arrange(desc(abs(Ratio)))
  ret <- ret %>% arrange(desc(abs(Difference)))
  
  # Return it
#  df_ratio
  ret
}