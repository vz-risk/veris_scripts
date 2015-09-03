DESCRIPTION = "getTSenum() is equivalent to getenum in verisr, however it provides time data.  This is very useful for either returning matrixes of features over time or data for visualization over time.  See Example code at the bottom for usage."

#. Takes a list of strings and a pattern and returns how many times that pattern exists in the string
#'
#' @param strings Vector of strings.
#' @param pattern A grepable string pattern to count
string.counter<-function(strings, pattern){  
  counts<-NULL
  for(i in 1:length(strings)){
    counts[i]<-length(attr(gregexpr(pattern,strings[i])[[1]], "match.length")[attr(gregexpr(pattern,strings[i])[[1]], "match.length")>0])
  }
  return(counts)
}

#. getTSenum takes a data frame and returns either a data frame of c('timeline.incident.year', 'enum', 'x', 'n', 'freq', 'count/total') a matrix of 'x' or 'freq' for enum vs timeline.incident.year
#'
#' @param data frame type object
#' @param depth controls at what depth to cut off enum sections.  For example, if enums are action.hacking.variety.C2, setting depth=2 will return enums of variety.C2 (the last 2). By default, it attempts to return the last section.
#' @param table 'df', 'x', or 'freq'. defaults to 'df'.  'df' returns a dataframe of names c('timeline.incident.year', 'enum', 'x', 'n', 'freq', 'count/total'). 'x' returns a matrix of enum vs timeline.incident.year with value of 'x' with enumerations as rows.  'freq' returns a matrix of enum vs timeline.incident.year with value of 'freq' with enumerations as rows.
#' @param tarnspose logical. default FALSE. When TRUE, table='x' and table='freq' returns timeline.incidnet.year as rows and enumerations as columns.
#' @param order.by the column name of the year
getTSenum <- function(data, depth=NULL, table="df", transpose=FALSE) {
  require(lazyeval)
  require(dplyr)
  require(reshape2)
  
  cNames <- names(data)
  if (is.null(depth)) {
    depth <- cNames[cNames != "timeline.incident.year"] %>% string.counter("[.]") %>% max()
    depth <- depth - 1
  }
  
  if (length(names(data)) <= 1) {
    warning("One or less columns supplied to getTSenum.  This will likely cause the function to fail.")
  }
      
  temp <- data %>% 
    gather(enum, 
           value, 
           -timeline.incident.year) %>% 
    filter(!is.na(value)) %>% 
    separate_('enum', 1:depth, "[.]", extra="merge") %>%
    select_('timeline.incident.year', as.name(depth), 'value') %>%
    rename_('enum' = as.name(depth)) %>%
    group_by(timeline.incident.year, enum) %>%
    summarize(x = sum(value)) %>%
    mutate(n = sum(x)) %>%
    mutate(freq = round(100 * x/n, 2), `count/total` = paste(x, n, sep="/")) %>%
    arrange(desc(timeline.incident.year)) %>% 
    ungroup()
  
  if (table == "count") {
    temp <- temp %>% acast(enum~timeline.incident.year, value.var="count/total")
  } else if (table == "freq") {
    temp <- temp %>% acast(enum~timeline.incident.year, value.var="freq", fill=0)
  } else if (table == "x") {
    temp <- temp %>% acast(enum~timeline.incident.year, value.var="x", fill=0)
  }
  
  if (transpose == TRUE) {
    temp <- t(temp)
  }
  
  temp
}


#. Bob's version of top_cols.  Returns a vector set of the top L columns over M years
#'
#' @param data frame type object
#' @param f the column name of the feature to maximize. (e.g. 'x')
#' @param L the number of years to consider
#' @param M the number of values to keep per year
#' @param order.by the column name of the year
top_cols <- function(dat, f, L=4, M=6, order.by="timeline.incident.year") {
  
  dat <- data.frame(dat)
  years <- sort(unique(dat$timeline.incident.year), decreasing=TRUE)[1:L]
  unique(as.vector(sapply(years, function(yr) {
    dat %>% 
      filter_(.dots=list(interp(~which_col==yr,
                                .values=list(which_col=as.name(order.by),
                                             yr=yr)))) %>% 
      select_(f, "enum") %>% 
      arrange_(.dots=list(interp(~desc(f), .values=list(f=as.name(f))))) %>% 
      head(M) %>% 
      .$enum
  })))
}


######## EXAMPLE USAGE  #########
#library(ggplot2)
#library(reshape2)
## Select Assets and build the time series data with getTSenum
#chunk <- vz %>% 
#  select(starts_with("asset.assets.variety."), 
#              timeline.incident.year) %>%
#  getTSenum(depth=4)
## view the data
#chunk
## view the data as a matrix. (NOTE: this could have been done by putting `, table='x'` in the getTSenum() call.)
#chunk %>% acast(enum~timeline.incident.year, value.var="x", fill=0)
## Visualize with ggplot
#chunk %>% 
#  ggplot(aes(timeline.incident.year, x, color=enum, fill=enum)) +  # set the asthetics ('aes'). This is the x, the y, and how to color/fill things.
#  geom_area( position = 'stack') + # this is the shape we want.  We want lines with areas between them so we use geom_area.  we set 'stack' because we want them on top of each other.
#  scale_colour_tableau() +  # The default line colors are ugly.  Use the tableau colors.
#  scale_fill_tableau()  # The default shape colors are ugly.  Use the tableau colors.

# That was WAY too much information in a matrix or chart.  We can, however, subset it.
m = 4  # we want to find the top assets over the last 4 years
l = 6  # we only want the top 6 assets per year
# Get the set of the top assets
topCols <- top_cols(chunk, 'x', L=l, M=m)
# Group together all the assets which aren't in the top assets
ee <- chunk %>%
  filter(!enum %in% topCols) %>%   # remove the top assets
  group_by(timeline.incident.year) %>%  # group by the year
  summarize(x = sum(x), n = median(n)) %>%  # Sum up the counts of the other assets.  (In theory, all the 'n's are the same, but we'll use the median to be safe.)
  mutate(enum = 'Everything Else', freq = round(100 * x/n, 2), `count/total` = paste(x, n, sep="/"))  # Set all of the enumerations to 'Everything Else', calculate the 'freq' & 'count/total' columns
chunk_filtered <- bind_rows(chunk %>% filter(enum %in% topCols), ee) # bind the 'everything else' assets to the top assets

# view the filtered data
chunk_filtered
# view the data as a matrix. (NOTE: this could have been done by putting `, table='x'` in the getTSenum() call.)
chunk_filtered %>% acast(enum~timeline.incident.year, value.var="x", fill=0)
# Visualize with ggplot
chunk_filtered %>% 
  ggplot(aes(timeline.incident.year, x, color=enum, fill=enum)) +  # set the asthetics ('aes'). This is the x, the y, and how to color/fill things.
  geom_area( position = 'stack') + # this is the shape we want.  We want lines with areas between them so we use geom_area.  we set 'stack' because we want them on top of each other.
  scale_colour_tableau() +  # The default line colors are ugly.  Use the tableau colors.
  scale_fill_tableau()  # The default shape colors are ugly.  Use the tableau colors.
# Ah, that's much easier to see.
