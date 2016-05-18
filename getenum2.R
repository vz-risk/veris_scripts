DESCRIPTION <- "This is currently just an annotated version of getenum()  However, in the future, it will be updated version of getenum() designed to handle veris resear questions and hypothesis more accurately."

#' Extract counts from one or more enumerations
#'
#' When exploring VERIS data, you may want to get a simple count of the values within a value or enumeration.  
#' Given one or more enumerations, this will return the subsequent underlying logical values in an ordered data frame.  
#' The data frame should be formatted for use in \code{ggplot2} graphics.
#' 
#' As of version 1.1: the \code{enum} variable may be a vector of one or more enumerations.  
#' This enables any number of dimensions to be specified.  This makes the \code{primary} and \code{secondary}
#' obsolete but are still supported for the time being.
#' 
#' Note there are some special values that can be set as the enumeration, 
#' that may not be obvious. :
#' * actor, action, attribute: will all return the next level down.  For example, just querying for "action" will return "malware", "hacking", and so on.
#' * action.variety: will return the variety enumerations across all actions (e.g. top N actions) (not in getenumby() yet)
#' * asset.variety: will return the type of assets, "Server", "Network, "User Dev" and so on
#' * victim.industry2: will return a short label of industries based on 2 values of NAICS code.
#' * victim.industry3: will return a short label of industries based on 3 values of NAICS code.
#' * pattern: will return the pattern the incidents are in.
#'
#' Change in 1.1: the "add.n" and "add.freq" options are now TRUE by default.
#' #' @aliases getenumby
#' @param veris a verisr object
#' @param enum the main enumeration field 
#' @param primary the primary enumeration to filter on
#' @param secondary the (optional) secondary enumeration to filter on
#' @param filter limit what records are searched (optional)
#' @param add.n include a total count of variables found (denominator)
#' @param add.freq include a percentage (x/n)
#' @param fillzero fill in missing matches with zeros
#' @param exclusive logical value, If true, will count the unknown value only if it exclusive and it will not count the Unknown if it is selected with other attributes in the enumeration.
#' @export
#' @import data.table
#' @examples
#' \dontrun{
#' # old method:
#' a2 <- getenum(veris, "action", primary="asset.variety")
#' # new method:
#' a4 <- getenum(veris, c("action", "asset.variety", "actor", "attribute"))
#' }
getenum2 <- function(veris, enum, primary=NULL, secondary=NULL, filter=NULL, 
                    add.n=T, add.freq=T, fillzero=T, exclusive=F) {
  
  # you can pass a logical vector the same length as the number of rows and it will filter rows out of the veris object
  if (missing(filter)) {
    filter <- rep(T, nrow(veris))
  } else if (length(filter) != nrow(veris)) {
    warning(paste0("filter is not same length (", length(filter),
                   ") as object (", nrow(veris), ")."))
    return(NULL)
  }
  
  # get the column names
  cnames <- colnames(veris)
  # get the enumerations to use as a vector
  # this is probably left over from when 'enum' wasn't treated as a vector and only allowed "primary" and "secondary"
  enum <- c(enum, primary, secondary)
  if (length(enum)>1 & exclusive) {
    warning("Cannot retrieve multiple enumerations and have exclusive set to TRUE, ignoring exclusive argument.")
    exclusive <- FALSE
  }
  
  # convert "asset.assets" to "asset.variety" in the chosen enumerations. (backwards compatability with getenumby())
  if(any(enum %in% c("asset.assets"))) {
    # message("getenumby(): as of version 1.1, asset.assets should be replaced by asset.variety")
    enum[which(enum %in% c("asset.assets"))] <- "asset.variety"
  }
  # turn the enums into regexes
  fullkey <- paste0('^', enum, "$")
  
  # get a logical vector of which enumerations are in the column names.  This includes regex wildcards within the enumeration
  fulln <- sapply(fullkey, function(x) any(grepl(x, cnames)))
  
  # if there's one enumeration and it's in the column names.  If the enumeration column is logical, ignore it. If it's a factor, run table to get the counts of the logical
  if (length(enum)==1 & all(fulln)) {
      if(is.logical(veris[[enum]])) {
      warning(paste0("single logical field requested: ", enum, ", skipping..."))
      return(data.frame())
    } else { # not a logical field, assuming factor
      out.table <- table(veris[[enum]])
      # If there's no data in the table vector, just return an empty dataframe
      if(!length(out.table)) {
        return(data.frame())
      }
      # if there's data in the table vector, convert it to a data farme, add n and freq, and return it ordered.
      outdf <- data.frame(enum=names(out.table), x=as.vector(out.table))
      if (add.n) outdf$n <- sum(!is.na(veris[[enum]]))
      if (add.freq) outdf$freq <- outdf$x/outdf$n
      # If the input dataframe was ordered, keep that order.  If not, order it by the enum factor
      if (is.ordered(veris[[enum]])) {
        outdf$enum <- factor(outdf$enum, levels=levels(veris[[enum]]), ordered=T)
      } else {
        outdf$enum <- factor(outdf$enum, levels=outdf$enum, ordered=T)
      }
    }
  # either there are multiple enumerations or the enumeration isn't in the column names
  } else {
    # since the enumeration(s) wasn't/weren't in a column name, create a new regex which captures 1 level below the enumeration name.
    gkey <- paste0("^", enum, ".[^.]+$")
    # get all the columns with the enum plus 1 more level.  This is a list with one vector for each enumeration
    savethisn <- thisn <- lapply(gkey, function(x) cnames[grep(x, cnames)])
    # check each vector in thisn to make sure something was found
    allfound <- sapply(thisn, function(x) length(x)>0)
    # if not all the enumerations were found, toss an error and return an empty data frame
    if(!all(allfound)) {
      warning(paste0("getenumby(): No columns matched \"", enum[!allfound], "\"", collapse="\", \""))
      return(data.frame())
    }
    
    # the next 2 columns do a cross product of the enumerations and a column of 0's so you get a dataframe with names c(enum, enum1, x) & all combinations of enum & enum1
    thisn$x <- 0
    outdf <- as.data.table(expand.grid(thisn))
    # The outdf columns are actually named something like "VAR1, "VAR2", "x" so get the names for VAR1 and VAR2
    cnm <- colnames(outdf)[1:(ncol(outdf)-1)]
    # just look in first enum (exclusive) for unknowns
    # returns a vector of the locations within the first enumeration that unknown exists.
    # NOTE: "Other" should not be "Unknown" even though it's used that way in asset. - GDB 02/04/16
    myunks <- unique(unlist(sapply(c("Unknown", " - Other", "unknown"), function(p) grep(p, thisn[[1]])), use.names=F))
    # For each enum combination (row in outdf), get the enumerations on that line, and all the rows (in the filtered rows), 
    #  which have all the values true (or at least) and count how many rows that is.  This becomes the 'x' for that row of the output.
    for(i in seq(nrow(outdf))) {
      this.comp <- as.character(unlist(outdf[i, cnm, with = F]))
      count <- rowSums(veris[filter, this.comp, with=F]) == length(enum)
      # if the row is an 'unknown' row, and we aren't including unknown rows, otherwise,  just keep the total.
      # WARNING: I think this is a bug.  the numbers in myunks was derived from [[1]], but i is from outdf.
      #   (This may because the first iteration of enum 1 in ) - GDB 02/04/16
      if (exclusive && i %in% myunks) {
        # only count an unknown row true if it is the only value of the enumeration true in the row
        count <- sum(count & rowSums(veris[filter, thisn[[1]], with=F])==1)
      } else {
        count <- sum(count)
      }
      outdf[i, x:=count]
    }
    
    # This converts the values of the enum columns to the last avlue in the name rather than the whole string, so "Worm" instead of "action.malware.variety.Worm"
    for(column in cnm) {
      # get the last value of each line of the enum
      tempcol <- getlast(as.character(unlist(outdf[ , column, with=F])))
      # repopulate it back into the column
      outdf[ , column:=tempcol, with=F]
    }
    # create the enum1, enum2, etc if there is more than 1 enum and set the column names to those names
    extra.names <- NULL
    if (length(enum)>1) extra.names <- paste0('enum', seq((length(enum)-1)))
    setnames(outdf, c('enum', extra.names, 'x'))
    # take all the columns from all the enumerations, sum the rows, then sum the sums to get a total count of enumerations
    # WARNING: I don't think this is accurate.  It doesn't accurately handle 'Unknowns'.  
    #   Also, overcounts the number of incidents because any incident w/ more than one enumeration gets counted mupltiple times. - GDB 02/04/16
    n <- sum(rowSums(veris[filter ,unlist(savethisn), with=F], na.rm=T) > 0, na.rm=T)
    # return an empty data frame if theres no data
    if (n==0) return(data.frame())
    # remove zeros if fillzero was false
    if (!fillzero) {
      outdf <- outdf[outdf$x>0,]
    }
    # add in if it was selected
    if (add.n) outdf$n <- n
    # add frequency if it was selected
    if (add.freq) outdf$freq <- outdf$x/n
    # how about we put some order to the chaos- Jay
    # get the 4A high level values
    a4names <- names(geta4names())
    # for i in length enum, if an enumeration is a top level 4A, get the last section of it, and replace that enum column with factors of the top level enumeration
    for(i in seq_along(enum)) {
      if (enum[i] %in% c('actor', 'action', 'asset.variety', 'attribute')) {
        n.order <- getlast(a4names[grep(paste0('^', enum[i]), a4names)])
        this.col <- colnames(outdf)[i]
        outdf[[this.col]] <- factor(outdf[[this.col]], levels=rev(n.order), ordered=T)
      }    
    }
  }
  # name the columns... enum enum1 enum2 (?)
  # print(outdf)
  #outdf <- outdf[order(-rank(x), enum)]
  #outdf$enum <- factor(outdf$enum, levels=outdf$enum, ordered=T)
  outdf
}



#' Get the last element from a column name
#' 
#' Givn a vector with one or more column names (veris fields), this will
#' return the last string in the name, as it is seperated by [.].
#' 
#' @param nm the vector of column names
getlast <- function(nm) {
  sapply(nm, function(x) {
    temp <- unlist(strsplit(x, '[.]'))
    temp[length(temp)]
  })
}

#' Convenience function for the a4 names and values
#' 
#' This returns a named vector where the names are the column names 
#' in the final verisr data table and the valus are suitable for using
#' in a regex in the existing column names.  
geta4names <- function() {
  convenience <- function(nm) {
    # this appears to be a bug.  I suspect setnames should also be applied to 'out'
    out <- tolower(nm)
    setNames(tolower(nm), nm)
  }
  actor <- convenience(paste('actor', 
                             c('External', 'Internal', 'Partner', 'Unknown'), 
                             sep="."))
  action <- convenience(paste('action', 
                              c('Malware', 'Hacking', 'Social', 'Physical', 
                                'Misuse', 'Error', 'Environmental', 'Unknown'), 
                              sep="."))
  attribute <- convenience(paste('attribute', 
                                 c('Confidentiality', 'Integrity', 'Availability'), 
                                 sep="."))
  assetmap <- c("S "="Server", "N "="Network", "U "="User Dev", "M "="Media", 
                "P "="Person", "T "="Kiosk/Term", "Un"="Unknown")
  asset <- setNames(paste('asset.assets.variety', names(assetmap), sep='.'),
                    paste('asset.variety', assetmap, sep='.'))
  c(actor, action, asset, attribute)  
}
