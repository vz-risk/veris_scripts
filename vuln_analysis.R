#' Function to calculate AUC and ROC for a vulnerability dataset
#' 
#' @param df The dataframe to analyze
#' @param time name of datetime column representing the scan identification
#'     date of the vulnerability
#' @param host name of column representing individual hosts, (e.g. IP_Address)
#' @param finding name of column representing individual findings, (e.g.
#'     "Vulnerability_Name")
#' @param subset name of column to generate individual trend lines for. (
#'      e.g. 'organizational_group', 'severity', 'asset_type', etc)  If 
#'      empty or NA, a single trend line will be generated for the full
#'      data frame
#' @param max_t Difftime to use as cutoff
#'     Defaults to 12 weeks (1 quarter)
#'     Other good options include 10, 30, and 100 days (the 1st, 2nd/median,
#'     and 3rd quartiles of vulnerability exploitation)
#' @returns
vuln_analysis <- function(
  df,
  time = "time",
  host = "host",
  finding = "finding",
  subset = NA,
  max_t = as.difftime(12, units="weeks") 
) {
  chunk <- df %>%
    # dplyr::filter(Vulnerability_Severity != "Info") %>%  # to remove info. Please filter before passing in
    dplyr::rename_(host = host, finding = "finding", time="time")
  if (!is.na(subset)) {
    chunk <- chunk %>% dplyr::group_by_("host", subset, "finding")  %>%
      dplyr::summarize(first=min(time), last=max(time)) %>%
      dplyr::mutate(dur = last - first)  %>% 
      dplyr::ungroup()
  } else {
    chunk <- chunk %>%
      dplyr::summarize(first=min(time), last=max(time)) %>%
      dplyr::mutate(dur = last - first)
  }
  
  if (!is.na(subset)) {
    chunk <- chunk %>% 
      dplyr::group_by_(subset) %>%
      dplyr::filter(chunk, first <= max(last) - max_t) # these findings haven't had a chance to age out yet
      dplyr::filter(!(dur == 0 & first == max(last))) %>% # Remove findings only seen at the last scan of the org.
      tidyr::nest() %>%
      dplyr::mutate(curves = lapply(data, function(x) {
        units(x$dur) <- "weeks"
        df <- data.frame(x=quantile(x$dur, c(seq(0, .97, .01), seq(.971, 1, .001))), y=c(seq(0, .97, .01), seq(.971, 1, .001)))
        df <- rbind( data.frame(x=as.difftime(0, units="weeks"), y=0), # establish a first point
                     df)
        last_date <- max(df$x[df$x <= max_t], na.rm=T) # used to esetablish a last point
        df <- rbind( df, 
                     data.frame(x=max_t, y=max(df[df$x == last_date, "y"]))) # establish a last point
        df %>% arrange(y)
      })) %>%
      dplyr::mutate(auc = unlist(lapply(curves, function(x) {x <- x[x$x <= max_t, ]
      MESS::auc(x=x$x/as.numeric(max_t), y=x$y)
      }))) %>%
      dplyr::mutate(cot = unlist(lapply(curves, function(df) {
        max(df[df$x == max_t, "y"])
      }))) %>%
      dplyr::mutate(n = unlist(lapply(data, nrow)))
  } else {
    chunk <- chunk %>% 
      dplyr::filter(chunk, first <= max(last) - max_t) # these findings haven't had a chance to age out yet
      dplyr::filter(!(dur == 0 & first == max(last))) %>% # Remove findings only seen at the last scan of the org.
      dplyr::mutate(curves = lapply(., function(x) {
        units(x$dur) <- "weeks"
        df <- data.frame(x=quantile(x$dur, c(seq(0, .97, .01), seq(.971, 1, .001))), y=c(seq(0, .97, .01), seq(.971, 1, .001)))
        df <- rbind( data.frame(x=as.difftime(0, units="weeks"), y=0), # establish a first point
                     df)
        last_date <- max(df$x[df$x <= max_t], na.rm=T) # used to esetablish a last point
        df <- rbind( df, 
                     data.frame(x=max_t, y=max(df[df$x == last_date, "y"]))) # establish a last point
        df %>% arrange(y)
      })) %>%
      dplyr::mutate(auc = unlist(lapply(curves, function(x) {x <- x[x$x <= max_t, ]
      MESS::auc(x=x$x/as.numeric(max_t), y=x$y)
      }))) %>%
      dplyr::mutate(cot = unlist(lapply(curves, function(df) {
        max(df[df$x == max_t, "y"])
      }))) %>%
      dplyr::mutate(n = unlist(lapply(data, nrow)))    
  }
  
  # return
  chunk
}