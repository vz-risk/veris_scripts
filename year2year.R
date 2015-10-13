DESCRIPTION <- "find the features that changed the most significantly between years"

#' Identify which features have changed most year-to-year in a verisr object
#' NOTE: df *must contain* timeline.incident.year.  If plus.dbir_year exists, it will be used.
#' 
#' @param df A verisr object of logical columns to analyze.
#' @param current_year The year of interest. Defaults to this year.
#' @param last_year The year to compare to. Defaults to last year.
#' @param filter Bool.  If TRUE, only 5th & 95th percentile results in at least 1/1000th of the records will be shown
#' @return The ratio of the percentage each feature in \code{current_year} vs \code{last_year}.
#' @examples
#'   incidents <- vz %>% select(timeline.incident.year,
#'                              plus.dbir_year,
#'                              contains("variety"), 
#'                              contains("vector"),
#'                              starts_with("attribute.confidentiality.data_disclosure"), 
#'                              starts_with("data_discovery"), 
#'                              matches("^victim.(industry2.|employee_count|orgsize)"), 
#'                              matches("timeline.*.unit.*"))
#'   breaches <- incidents %>% filter(attribute.confidentiality.data_disclosure.Yes)
#'   y2y <- year2year(breaches)
year2year <- function(df, current_year=NULL, last_year=NULL, filter=TRUE) {
  # Set the years to compare
  if (is.null(current_year)) {
    current_year <- format(Sys.Date(), "%Y")
  }

  if (is.null(last_year)) {
    last_year <- as.character(as.numeric(current_year) - 1)
  }
 
  # Year based on dbir year if possible
  if ("plus.dbir_year" %in% colnames(df)) {
    current_df <- df %>% filter(plus.dbir_year == as.character(current_year))
  } else {
    current_df <- df %>% filter(timeline.incident.year == as.character(as.numeric(current_year) - 1))
  }
  
#  if ("plus.dbir_year" %in% colnames(df)) {
#    last_df <- df %>% filter(plus.dbir_year == last_year)
#  } else {
#    last_df <- df %>% filter(timeline.incident.year == as.character(as.numeric(last_year) - 1))
#  }
  # well, since no-one coded plus.dbir_year in the 2014 data, we'll use incident year
  last_df <- df %>% filter(timeline.incident.year == as.character(as.numeric(last_year) - 1))
  
  current_df <- current_df %>% select(-timeline.incident.year, -plus.dbir_year)
  last_df <- last_df %>% select(-timeline.incident.year, -plus.dbir_year)
  
  # get this year's breach percents
  current_df_pcts <- colSums(current_df) / nrow(current_df)
  
  # get last year's breach percents
  last_df_pcts <- colSums(last_df) / nrow(last_df)
  
  df_ratio <- current_df_pcts/last_df_pcts
  df_ratio[is.nan(df_ratio)] <- 1
  # Below line might be useful in dealing with infinits
  #df_ratio[is.infinite(df_ratio)] <- max(df_ratio) * 1.20
  
  # Visibly inespect breach_ratio for fun
  # breach_ratio[order(breach_ratio, decreasing=T)]
  # ggplot(melt(breach_ratio), aes(x=seq_along(value), y=value, size=sqrt(colSums(current_breaches)))) + geom_point(stat="identity")

  if (filter) {  
    # Need to choose outliers
    # The data is long tailed so no mean/SD
    percentiles <- quantile(df_ratio[!is.infinite(df_ratio)], c(0.05, 0.95))
    
    # going to ask if bin size decreases to the median
    # If it is, I'm going to assume this is not a normal distribution and the lower
    #   quantile doesn't matter.  If bins increasing to median bin, 
    r <- hist(df_ratio, plot=FALSE)
    median_bin <- length(r$breaks[r$breaks < median(df_ratio)]) + 1
    use_lower_bound <- is.unsorted(rev(r$density[1:2]))
    
    if (use_lower_bound) {
      df_ratio <- df_ratio[df_ratio < percentiles[1] | df_ratio > percentiles[2]]
    } else {
      df_ratio <- df_ratio[df_ratio > percentiles[2]]
    }

    # Change to a data frame and add the record counts
    df_ratio <- df_ratio %>% melt() %>% add_rownames(var="feature")
    df_feature_cnts <- current_df %>% select(one_of(df_ratio$feature)) %>% colSums() %>% melt() %>% add_rownames(var="feature")
    df_ratio <- full_join(df_ratio, df_feature_cnts, by="feature")
    names(df_ratio) <- c("Feature", "Ratio", "Count")
    
    # Filter record counts at or below the 50th percentile (picked totally arbitrarily)
    #df_ratio %>% filter(Count > quantile(df_ratio$Count, c(.5))["50%"])
    # Filter record counts at or below 45 rather than the above quantile filtering.
    #  (We use 45 in the standard report)
    #df_ratio %>% filter(Count > 45)
    # Filter record counts at or or below 1/1000 of the total records
    df_ratio <- df_ratio %>% filter(Count > nrow(df) / 1000)
  }
  
  # Order it
  df_ratio <- df_ratio %>% arrange(desc(Ratio))
  
  # Return it
  df_ratio
}