DESCRIPTION <- "A vinette of multiple time series visualizations of the same data using various packages.  Skip down past the setup to get to the visualizations."

## START SETUP ##
# Install packages that are not necessarily in CRAN
install.packages("devtools")
devtools::install_github("hrbrmstr/streamgraph")  # Stream Graphs
devtools::install_github("hrbrmstr/taucharts")  # Tau Charts
devtools::install_github("timelyportfolio/parcoords")  # Parallel Coordinates Chart
devtools::install_github("jayjacobs/verisr")  # Verisr
devtools::install_github("bokeh/rbokeh") # R Bokeh

# Prepare Environment
suppressPackageStartupMessages(library(pbapply))
suppressPackageStartupMessages(library(rbokeh))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(streamgraph))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(dygraphs))
suppressPackageStartupMessages(library(parcoords))
suppressPackageStartupMessages(library(taucharts))
suppressPackageStartupMessages(library(xts))
#suppressPackageStartupMessages(library(slopegraph))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(binom))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(verisr))
suppressPackageStartupMessages(library(lazyeval))
source('/Volumes/verizon/Customer and Partner Data/DBIR/dbir2015-support.R')
opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE,
               results="markdown", prompt=FALSE, error=FALSE,
               fig.width=8, fig.height=5, cache=FALSE)

theme_set(theme_minimal() + 
            theme(panel.background=element_rect(fill="floralwhite", color="gray75"),
                  panel.grid.major=element_line(color="gray75", size=0.1),
                  axis.ticks=element_blank(),
                  title = element_text(face="italic", size=10)
            ))


# Get VCDB Data
load("/Users/v685573/Documents/Development/VCDB/data/verisr/vcdb.dat")

# Load functions to create time series data
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
  years <- sort(unique(tline$timeline.incident.year), decreasing=TRUE)[1:L]
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

#  Format the data to time series. See getTSenum.R for an understanding of this section.
chunk <- vcdb %>% 
  filter(attribute.confidentiality.data_disclosure.Yes) %>% 
  select(matches("action.*.vector.*"), timeline.incident.year) %>% 
  getTSenum(depth=2)
topCols <- top_cols(chunk, 'x', M=4, L=4)
ee <- chunk %>% 
  filter(!enum %in% topCols) %>% 
  group_by(timeline.incident.year) %>% 
  summarize(x = sum(x), n = median(n)) %>% 
  mutate(enum = 'Everything Else', freq = round(100 * x/n, 2), `count/total` = paste(x, n, sep="/"))
chunk_filtered <- bind_rows(tline %>% filter(enum %in% topCols), ee)
chunk_matrix <- vcdb %>% 
  filter(attribute.confidentiality.data_disclosure.Yes) %>% 
  select(matches("action.*.vector.*"), timeline.incident.year) %>% 
  getTSenum(depth=2, table='x') %>% as.data.frame()
chunk_matrix_filtered <- chunk_matrix[rownames(chunk_matrix) %in% topCols,]

## END SETUP ##

###### OK, HERE's WHAT YOU'RE HERE FOR.  VISUALIZATIONS. ######
# (for reference, this is a filtered list of the vectors that threat actors take.)


# Streamgraph - Great for visualizing multiple values over time.  It's very bright and can be a bit confusing.  However, it's interactive which makes it easier to use.
chunk_filtered %>% streamgraph("enum", "x", "timeline.incident.year") %>% sg_fill_tableau %>% sg_axis_x(tick_interval=1, tick_units="year")

# GGPLOT is the most versatile figure generator, but is non-interactive.  I won't even a fraction of the options. Just some of the major plot types.
# This is the ggplot version of stream graphs. It's not smoothed though.  In this one we do it all in one line
chunk_filtered %>% 
  ggplot(aes(timeline.incident.year, x, color=enum, fill=enum)) +  # set the asthetics ('aes'). This is the x, the y, and how to color/fill things.
  geom_area( position = 'stack') + # this is the shape we want.  We want lines with areas between them so we use geom_area.  we set 'stack' because we want them on top of each other.
  scale_colour_tableau() +  # The default line colors are ugly.  Use the tableau colors.
  scale_fill_tableau()  # The default shape colors are ugly.  Use the tableau colors.

# This is a ggplot with each vector as a line plus a trend line to boot.  Here each addition to the plot is on a separate line to help with debugging (just for example)
gg <- ggplot(chunk_filtered, aes(x=factor(timeline.incident.year), y=freq, colour=factor(enum), group=enum))
gg <- gg + geom_line(linetype = 2)
gg <- gg + geom_smooth(method="lm", se=FALSE, formula = y ~ x + I(x^2))
gg <- gg + scale_colour_tableau()  # The default line colors are ugly.  Use the tableau colors.
gg <- gg + scale_fill_tableau()  # The default shape colors are ugly.  Use the tableau colors.
gg  # since we've been storing it, we need to call it w/o saving to visualize it

# Tau Chart uses a JS library and can do multiple types of charts. (http://rpubs.com/hrbrmstr/taucharts)  I like it best for bar graphs (stacked & side-by-side.)
chunk_filtered %>%  
  arrange(timeline.incident.year) %>%  # necessary to ensure the columns are in order of year
  ungroup %>% mutate(timeline.incident.year=factor(timeline.incident.year)) %>%  # convert timeline to a factor for taucharts (ungroup because timeline.incident.year is apparently still grouped and needs to be ungrouped for the mutate to work.)
  rename(Year = timeline.incident.year, Attribute = enum) %>%  # Changing the name because it's too long
  tauchart %>% tau_stacked_bar("Year", "x", "Attribute") %>% tau_legend() %>% # Create stacked bar chart
  tau_guide_x(label="Year") %>%
  tau_guide_y(label="Count") %>%
  tau_title(title="Count of breaches per vector over time") 

# HTML Widget Line chart with pygraph.  This is nice, interactive, stacked line chart, but requires the data as a matrix and the x-axis as a time series
chunk_matrix_xts <- chunk_matrix_filtered  %>% t()  # switch the rows for the columns.  i.e. transpose it
chunk_matrix_xts <- as.data.frame(chunk_matrix_xts)  # make it a data frame
chunk_matrix_xts$year <- as.vector(dimnames(chunk_matrix_xts)[[1]]) 
chunk_matrix_xts$year <- as.Date(paste("31-12", as.character(chunk_matrix_xts$year), sep="-"), "%d-%m-%Y")  # turn the years into dates
xts(chunk_matrix_xts %>% select(-year), order.by=chunk_matrix_xts$year) %>% dygraph() %>% dyOptions(stackedGraph = TRUE)

# R Bokeh is an R interface to the Bokeh JS library.  It also produces reasonable line graphs, but they stacked well and that legend has to be moved manually.
figure() %>% ly_lines(timeline.incident.year, x, group=enum, color=enum, data=arrange(chunk_filtered, timeline.incident.year), stacked=T)

# paracoords produces a nice, interactive, parallel coordinates plot.  If you want the items in order each year with equal spacing, you'll need to uncomment the 'apply' line below.
# Paracoords does expect the data in a matrix-like data frame though.
# It is _very_ interactive.  You can drag the columns to reorder. (Important since the enumeration tends to show up on the right rather than the left.)
# You can also select a section of each year and only get the enumerations which pass through that year
len <- chunk_matrix_filtered %>% colnames() %>% length()  # get the number of years
ord <- c(len+1, c((len-6):len))  # keep only the last 6 and put the enumeration up front
chunk_matrix_filtered_par <- chunk_matrix_filtered %>%
  #apply(MARGIN=2, rank, ties.method="random") %>%  # include if you only care about the order and not actual values
  as.data.frame()  # well, not really a matrix.  a data frame.
chunk_matrix_filtered_par$`enum` <- as.factor(rownames(chunk_matrix_filtered_par))  # Add the column for the enumeration names
chunk_matrix_filtered_par <- chunk_matrix_filtered_par[,ord]  # apply 'ord' to filter/re-order the columns
parcoords(chunk_matrix_filtered_par, rownames=F, brush="1d-axes", reorderable=T, color= list(colorBy="enum", colorScale = htmlwidgets::JS('d3.scale.category10()' )))

