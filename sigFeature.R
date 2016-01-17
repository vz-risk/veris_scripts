DESCRIPTION <- "identify the features which make a group withen an enumeration of the data unique."

#' Takes a data set of features of interest and a feature to group on and returns a matrix of group vs factors with
#'              a value representing how unique the factor is.  The higher the number, the more likely it better describes the group,
#'              differentiating it from the groups.
#' WARNING: df must only contain logicals which roll up to factors.  Thats the majority of the VERISR data, but not all of it.
#' WARNING: The distribution comparison algorithm assumes a sum of 100% across a vector.  Since we have some groups that don't have
#'          the feature at all, and some that have more than 100% (say when 2 values for action.hacking.variety are chosen) 
#'          the numbers are not completely accurate and should only be used as a guide for exploring the data.
#' WARNING: Due to the vectors not being exactly as expected as described above, some "Inf" and some "NA"/"NaN" will appear in the results.
#'          I'm not yet sure of the best way to interpret them.  
#'          Inf appears to be when the group is completely in the non-group. e.g. group: c(1,0,1,0) non-group: c(1,5,1,0)
#'          "NA"/"NaN" may equate to "does not exist in the group". Either may indicate relevance.
#' NOTE: I'm handling factors but not hierarchicals like the DBIR actually has 
#'       maybe the bayesian stuff in the entropy package can help with this.
#' NOTE: For continuous variables, I could notionally use em2d(a,b) from emdist (earth movers dist package)
#'       problem 1: that seems to require 'binning' the vector
#'       problem 2: the values max at the distances so c(1,0,0) to c(0,0,1) would be 3. That needs to be normalized
#' NOTE: This uses a nested 'for' loop.  I'm sorry.
#' NOTE: This takes a second to run.  See note about nested 'for' loops and me being sorry.
#' 
#' @param df A verisr object subsetted to feature columns of interest plus the \code{group_feature} column(s)
#' @param group_feature The feature you want to understand how the patterns are different in. Maybe be a single column like 'pattern' or a column part of a set of columns like 'action.malware.variety.Other'
#' @return a matrix of the levels of group_feature vs the other features with values representing the significance of the feature to the level.
#' @examples
#' sf <- vz %>% select(contains("variety"), contains("vector"), pattern) %>% sigFeatures("pattern")` will let us know what
#'          features relevant per pattern.  `sf["Cyber-Espionage", ] %>% sort(decreasing=TRUE) %>% View()` will produce the following:
#'           1   action.hacking.variety  Inf
#'           2   actor.external.variety  5.492158e+02
#'           3   attribute.confidentiality.data.variety  7.418538e+01
#'           4   attribute.availability.variety  6.560892e+01
#'           5   asset.assets.variety    5.287068e+01
#'           ...
#'           To understand "Cyber-Espionage" we should therefore subset to it and enumerate "action.hacking.variety" or "action.external.variety":
#'           vz %>% filter(pattern.Cyber-Espionage) %>% getenum("action.external.variety")
#'           vz %>% filter(!pattern.Cyber-Espionage) %>% getenum("action.external.variety")
#'   incidents <- vz %>% select(timeline.incident.year,
#'                              plus.dbir_year,
#'                              contains("variety"), 
#'                              contains("vector"),
#'                              starts_with("attribute.confidentiality.data_disclosure"), 
#'                              starts_with("data_discovery"), 
#'                              matches("^victim.(industry2.|employee_count|orgsize)"), 
#'                              matches("timeline.*.unit.*"))
#'   breaches <- incidents %>% filter(attribute.confidentiality.data_disclosure.Yes)
#'   sf <- breaches %>% select(-timeline.incident.year, -plus.dbir_year) %>% sigFeatures("action.malware.vector.Email link")
#'   sf["Email link", ] %>% melt() %>% add_rownames() %>% arrange(desc(value))
#'   action.hacking.variety      Inf
#'   actor.external.variety      Inf
#'   asset.assets.variety        Inf
#'   action.malware.vector       224.46550114
#'   timeline.compromise.unit    35.83258006
#'   victim.industry2            4.51195209
#'   action.malware.variety      3.77334349
#'   ...
#'   vz %>% filter(`action.malware.vector.Email link`) %>% getenum("timeline.compromise.unit")
#'          enum  x  n       freq
#'    1:    Days 11 49 0.22448980
#'    2:   Hours  2 49 0.04081633
#'    3: Minutes  1 49 0.02040816
#'    4:  Months  0 49 0.00000000
#'    5:      NA 15 49 0.30612245
#'    6:   Never  0 49 0.00000000
#'    7: Seconds 11 49 0.22448980
#'    8: Unknown  7 49 0.14285714
#'    9:   Weeks  2 49 0.04081633
#'    10:   Years  0 49 0.00000000
#'   vz %>% filter(`action.malware.vector.Email link`) %>% select(starts_with("timeline.compromise.unit"), timeline.incident.year) %>% getTSenum(depth=4, table="x", transpose=TRUE)
#'   Days Hours Minutes Months NA Never Seconds Unknown Weeks Years
#'   2010    4     0       0      0  0     0       0       0     0     0
#'   2011    0     0       0      0  0     0       1       0     1     0
#'   2012    0     1       0      0  0     0       5       3     0     0
#'   2013    3     0       1      0  4     0       5       3     0     0
#'   2014    4     1       0      0 11     0       0       1     1     0
sigFeatures <- function(df, group_feature) {
  require(qdapTools)
  require(tidyr)
  require(reshape2)
  require(entropy)
  require(dplyr)

  # Raise errors basedon df size
  if (nrow(df[df[[group_feature]], ]) == 0) {
    stop(paste0(group_feature, " not present in df."))
  }
  if (nrow(df[!df[[group_feature]], ]) == 0) {
    stop(paste0("No rows exist where ", group_feature, " is not present in df so no rows to compare to."))
  } else {
  
    # if the group_feature is logical, it needs to be part of the chunk_gather
    if (class(df[[group_feature]]) == "logical")  {
      chunk_group_feature <- strsplit(group_feature, "[.](?!.*[.])", perl=T)[[1]][1]

      # Get the count of records by group_feature to divide by
      group_feature_sums <- df %>% select(starts_with(chunk_group_feature)) %>% colSums() %>% as.data.frame() %>% add_rownames()
      # split the group feature column into key and value (list of vector of 2 as a column)
      group_feature_sums[[1]] <- strsplit(group_feature_sums[[1]], "[.](?!.*[.])", perl=T)
      # keep only the 2nd item and get back to a vector
      group_feature_sums[[1]] <- sapply(group_feature_sums[[1]], function(x) {x[2]})
            
      # filter where the group feature doesn't exist in the data
      # This is because those rows could implicitly have any value for this enumeration.
      # NOTE: This should leave any row with the major enumeration.  (e.g. if feature is action.hacking.variety.DoS, all action.*.variety.* should be included)
      # Doing so will cause issues later on in the row duplication block immediately below.
      group_feature_rows <- df %>% select(starts_with(chunk_group_feature)) %>% apply(MARGIN=1, any)
      #df <- df[group_feature_rows,]  # This line was causing errors and, anyway `df <- df[rep.int(seq(nrow(df)), times=l) ,]` below accomplishes the same thing

            
      # get a column to represent the combination of the group_feature function
      # create a feature with a list of the group_feature that are trust
      df[[chunk_group_feature]] <- df %>% select(starts_with(chunk_group_feature)) %>% apply(MARGIN=1, function(x) {names(x[x==TRUE])})
      # unwind that list to a strait vector of all true features
      group <-unlist(df[[chunk_group_feature]])
      # separate group to only the last section
      group <- sapply(group, function(x) {ret <- strsplit(x, "[.](?!.*[.])", perl=T); ret[[1]][2]})
      # get the number of true features per row
      l <- sapply(df[[chunk_group_feature]], length)
      # l <= 0 won't work
      if (length(l) <= 1) {
        stop(paste("The feature to analyze,", chunk_group_feature, "has", l, "values.  It must have at least 2 values."))
      }
      # duplicate each row by the number of true features
      df <- df[rep.int(seq(nrow(df)), times=l) ,]
      # replace the list of features per row with a single feature per row
      df[[chunk_group_feature]] <- group

    } else {
      if (length(unique(df[[group_feature]])) <= 1) {
        stop(paste("The feature to analyze,", group_feature, "has", l, "values.  It must have at least 2 values."))
      }
      
      chunk_group_feature <- group_feature
      # Get the count of records by group_feature to divide by
      group_feature_sums <- df %>% group_by_(chunk_group_feature) %>% summarize(count=n()) %>% as.data.frame()
    }
  
    # identify the logical and illogical columns to be gathered
    logical_columns <- names(df)[sapply(df, is.logical)]
    illogical_columns <- names(df)[sapply(df, function(x) {!is.logical(x)})]
  
    ### Gather the Logicals ###
    if (length(logical_columns) > 0) {
      # gather to group_feature, key, value sets
      #chunk_gather <- df[ , colnames(df) %in% c(logical_columns, chunk_group_feature), with=F] %>% 
      chunk_gather <- df[ , colnames(df) %in% c(logical_columns, chunk_group_feature)] %>% 
        gather_("enum", "value", setdiff(logical_columns, chunk_group_feature)) %>% 
        filter(!is.na(value)) %>% 
        filter(value)
      # get to factors rather than logicals
      chunk_gather <- chunk_gather %>% select(-value) %>% separate(enum, c("enum", "value"), sep = "[.](?!.*[.])", extra="merge")  
      # Add the counts back in
      chunk_gather$sum <- lookup(chunk_gather[[chunk_group_feature]], as.data.frame(group_feature_sums))
    }
    ### End Gather the Logicals ###
    ### Gather the Factors ###
    if (length(illogical_columns) > 1) {
      # illogical..., as in character, factor, numeric columns.  get it?! GET IT?!!
      #illogical_chunk_gather <- df[ , !colnames(df) %in% setdiff(logical_columns, chunk_group_feature), with=F] %>% 
      illogical_chunk_gather <- df[ , !colnames(df) %in% setdiff(logical_columns, chunk_group_feature)] %>% 
        gather_("enum", "value", setdiff(colnames(df), c(logical_columns, chunk_group_feature)))
      # Add the counts back in
      illogical_chunk_gather$sum <- lookup(illogical_chunk_gather[[chunk_group_feature]], as.data.frame(group_feature_sums))
      # Make sure value is a character vector
      illogical_chunk_gather$value <- as.character(illogical_chunk_gather$value)
      ### End Gather the Factors ###
      # Bind the two
      chunk_gather <- rbind(chunk_gather, illogical_chunk_gather)
    }
      
    # build a results matrix
    results <- matrix(data=0, 
                      nrow=length(unique(chunk_gather[[chunk_group_feature]])), 
                      ncol=length(unique(chunk_gather$enum)), 
                      dimnames=list(unique(chunk_gather[[chunk_group_feature]]), unique(chunk_gather$enum)))

    # I feel dirty.  hope to replace these for loops with something more R'y
    for (feature in unique(chunk_gather$enum)) {
        # Build the table for a single feature
        chunk_cast <- chunk_gather %>% 
            # select the feature
            filter(enum==feature) %>%  #ERROR: Replace "action.error.variety"
            # get rid of the enumeration column as we subsetted to it
            group_by_(chunk_group_feature, "value") %>% 
            # convert counts to percentages
            summarize(sum=n()) %>% 
            # create a matrix dataframe that can then have rows summed and compared to all other rows.
            reshape2::dcast(list(chunk_group_feature, "value"), value.var="sum", fill=0)
        # get cluster as row names
        if (any(is.na(chunk_cast[[chunk_group_feature]]))) {
            stop("The Group Feature cannot have any values of 'NA'")
        } else {
          rownames(chunk_cast) <- chunk_cast[[chunk_group_feature]]
        }
          
        feature_chunk_cols <- unique(chunk_gather[chunk_gather$enum == feature, ]$value)
        feature_chunk_rows <- unique(chunk_gather[[chunk_group_feature]])
        
        feature_chunk <- matrix(data=0, 
                          nrow=length(feature_chunk_rows), 
                          ncol=length(feature_chunk_cols), 
                          dimnames=list(feature_chunk_rows, feature_chunk_cols))  %>%
                         as.data.frame()

        feature_chunk[rownames(chunk_cast), colnames(feature_chunk)] <- chunk_cast[,setdiff(colnames(chunk_cast), chunk_group_feature)]
        
        feature_chunk <- as.matrix(feature_chunk)

        if (ncol(feature_chunk) > 2) {
            for (group in unique(chunk_gather[[chunk_group_feature]])) {
              
                g <- feature_chunk[as.character(group), ]
                
                if (nrow(feature_chunk) > 2) {
                  not_g <- colSums(feature_chunk[setdiff(rownames(feature_chunk), as.character(group)), ]) %>% as.vector()
                } else {
                  not_g <- feature_chunk[setdiff(rownames(feature_chunk), as.character(group)), ] %>% as.vector()
                }

                # checks if not_g is all 0's.  if it is, chi2 will fail
                if ((length(not_g) != 0) && (unique(not_g) != 0)) {
                    results[as.character(group), feature] <- 0.5 * chi2.plugin(g, not_g)
                } else {
                  results[as.character(group), feature] <- NaN
                }
            }
        }
    }

    results
  }
}

