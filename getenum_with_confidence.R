#### getenum for verisr 2.2 (paired with veris 1.4) ####

#### OPTION 1 ####

subsets <- lapply(thisn[1:length(thisn)-1], function(x) {
  subsets <- 
}
names(subsets) <- thisn[[1]]

)

subset <- function(subsets, enums) {
  if (class(subsets) != "list") {
    lapply(enums[[1]], function(x) {
      subsets[subsets[ , which(names(subsets) == x)], ]  # return the subset of the dataframe where the enumeration x is true
    })
  } else if(length(enums) > 1) {
    # subset
    # recurse on each subset with the next enums[2:length(enums)]
    # return the list
  } else {
    # subset and return the list
    # or just do the summation of rows right here
  }
}
#### OPTION 2 ####
outdf <- expand.grid(thisn)
outdf$x <- unlist(apply(outdf, MARGIN=1, function(x) {
  sum(rowSums(df[, which(names(df) %in% x)]) == length(x))
}))


#' Extract counts from one (or more) enumerations
#' 
#' When exploring VERIS data, you may want to get a simple count of the values within a value or enumeration.  
#' Given one or more enumerations, this will return the subsequent underlying logical values in an ordered data frame.  
#' The data frame should be formatted for use in \code{ggplot2} graphics.
#' 
#' As of version 2.2, all backwards compatibility prior to version 2.1 has been removed
#' @param veris A verisr object
#' @param enum The main enumeration field. Character or character vector of enumeration types
#' @param na A logical variable indicating whether to include '.NA' enumerations. Required if '.NA' enumerations exist
#' @param alpha The significance level for the confidence intervals. Must be a real number in the interval [0, 1]
#' @param fillzero A logical variable. False indicates that enumerations with no records (x=0) will not be included in the output
#' @param labels either 'last' or 'all'. Chooses to return the entire enumeration tree (a.b.c.d) or just the final section (.d)
getenumCI_noTS <- function(veris, enum, na = NULL, alpha=0.95, fillzero=FALSE, labels="last", ...) {
  library(MultinomialCI)

  if (is.null(na) & any(grep("[.]NA$", names(df)))) { stop("'na' must be specified if any column names end in .NA")}get
        
  df <- as.data.frame(veris)

  # build regex for enumerations.  Should work with meta-columns (e.g. action.Hacking) as well
  gkey <- paste0("^",enum,"[.][A-Za-z][^.]+$")
  # get list of leafs for each enumeration
  thisn <- lapply(gkey, grep, names(df), value=TRUE) #TODO: check that we use both 'savethisn' and 'thisn'
  
  # remove 'unknown' as they equate to 'unmeasured' in all cases where
  thisn_filtered <- lapply(thisn, function(x) {
    # if only the last value is changing, 'unknown' is effectively unmeasured
    # the sub removes everything form the last period on.
    # it then takes the rest, collects unique ones, and checks length.
    # if there is more than one unique one, it means the enumeration is above the final level and 'Unknowns' should be counted in 'n' (as they count against the higher level)
    if (length(unique(sub("[.][^.]+$", "", x))) > 1) {
      x
    } else {
      # only one enumeration meaning only the leaf is changing and therefore 'unknowns' are 'unmeasured'
      grep("[.][U|u]nknown$", x, value=TRUE, invert=TRUE)
    }
  })  
  if (!is.null(na)) {
    if (na == FALSE) {
      thisn_filtered <- lapply(thisn_filtered, function(x) {grep("[.][Nn][Aa]$", x, value=TRUE, invert=TRUE)})
    }
  }
    
  # subset the dataframe to only those records that have a measurement in each enumeration
  #  and only the columns we wish to measure
  for (e in thisn_filtered) {
    df <- df[rowSums(df[, which(names(df) %in% e)]) > 0, ]
  }
  n <- nrow(df)
  # filter to only the columns we wish to measure
  df <- df[ , unlist(thisn)]
  
  outdf <- expand.grid(thisn)
  outdf$x <- unlist(apply(outdf, MARGIN=1, function(x) {
    sum(rowSums(df[, which(names(df) %in% x)]) == length(x))
  }))
  # add n. (complete sample, not per any enumeration)
  outdf$n <- n
  # Remove n for columns not included in n
  remove_n_rows <- unlist(lapply(thisn, function(x) {
    if (length(unique(sub("[.][^.]+$", "", x))) <= 1) {
      grep("[.][U|u]nknown$", x, value=TRUE, invert=FALSE)
    }
  }))
  if (!is.null(na)) {
    if (na == FALSE) {
      remove_n_rows <- c(remove_n_rows, unlist(lapply(thisn_filtered, function(x) {grep("[.][Nn][Aa]$", x, value=TRUE, invert=TRUE)})))
    }
  }
  for (i in 1:length(enum)) {
  outdf[outdf[ , i] %in% remove_n_rows , "n"] <- NA
  }   
  # add frequency
  outdf$freq <- outdf$x/outdf$n
  # apply the multinomial confidence interval to the first (correct) value and ignore the 2nd one which is just there to balance it
  # (I know I'm doing each multinomial separately.  Testing showed that changing other rows (other than the sum of x) doesn't change anything)
  # bind enums, x, n, freq, lower and upper confidence intervals
  outdf <- cbind(outdf, t(apply(data.frame(x=outdf$x, n=outdf$n-outdf$x), MARGIN=1, function(x) {if (any(is.na(x))) {c(NA, NA)} else {multinomialCI(as.numeric(x), alpha)[1, ] }})))
  names(outdf) <- c(paste0("enum", 1:length(enum)), "x", "n", "freq", "lower", "upper")
  
  # TODO: handle character vectors
  # TODO: allow multiple enumerations (done)
  #       expand.grid(column_names)
  #
  # TODO: Handle time-series data (timeline.incident.year, plus.dbir.year) (will probably be a surrogate function)
  # TODO: Fix enumeration labels.  Compress down matching enumerations.
  
  # order chunk
  outdf <- outdf[order(-outdf$x), ]
  
  # remove zeros if chosen
  if (!fillzero) {
    outdf <- outdf[outdf$x > 0, ]
  }
  
  # return
  outdf
}