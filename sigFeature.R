DESCRIPTION <- "identify the features which make a group withen an enumeration of the data unique."

# DESCRIPTION: Takes a data set of features of interest and a feature to group on and returns a matrix of group vs factors with
#              a value representing how unique the factor is.  The higher the number, the more likely it better describes the group,
#              differentiating it from the groups.
# EXAMPLE: `sf <- vz %>% select(contains("variety"), contains("vector"), pattern) %>% sigFactors("pattern")` will let us know what
#          features relevant per pattern.  `sf["Cyber-Espionage", ] %>% sort(decreasing=TRUE) %>% View()` will produce the following:
#           1   action.hacking.variety  Inf
#           2   actor.external.variety  5.492158e+02
#           3   attribute.confidentiality.data.variety  7.418538e+01
#           4   attribute.availability.variety  6.560892e+01
#           5   asset.assets.variety    5.287068e+01
#           ...
#           To understand "Cyber-Espionage" we should therefore subset to it and enumerate "action.hacking.variety" or "action.external.variety":
#           vz %>% filter(pattern.Cyber-Espionage) %>% getenum("action.external.variety")
#           vz %>% filter(!pattern.Cyber-Espionage) %>% getenum("action.external.variety")
# WARNING: df must only contain logicals which roll up to factors.  Thats the majority of the VERISR data, but not all of it.
# WARNING: The distribution comparison algorithm assumes a sum of 100% across a vector.  Since we have some groups that don't have
#          the feature at all, and some that have more than 100% (say when 2 values for action.hacking.variety are chosen) 
#          the numbers are not completely accurate and should only be used as a guide for exploring the data.
# WARNING: Due to the vectors not being exactly as expected as described above, some "Inf" and some "NA"/"NaN" will appear in the results.
#          I'm not yet sure of the best way to interpret them.  
#          Inf appears to be when the group is completely in the non-group. e.g. group: c(1,0,1,0) non-group: c(1,5,1,0)
#          "NA"/"NaN" may equate to "does not exist in the group". Either may indicate relevance.
# NOTE: I'm handling factors but not hierarchicals like the DBIR actually has 
#       maybe the bayesian stuff in the entropy package can help with this.
# NOTE: For continuous variables, I could notionally use em2d(a,b) from emdist (earth movers dist package)
#       problem 1: that seems to require 'binning' the vector
#       problem 2: the values max at the distances so c(1,0,0) to c(0,0,1) would be 3. That needs to be normalized
# NOTE: This uses a nested 'for' loop.  I'm sorry.
# NOTE: This takes a second to run.  See note about nested 'for' loops and me being sorry.
sigFeatures <- function(df, group_feature) {
    require(dplyr)
    require(tidyr)
    require(qdapTools)
    require(reshape2)
    require(entropy)

    # Get the count of records by group_feature to divide by later
    group_feature_sums <- df %>% group_by_(group_feature) %>% summarize(count=n())
    # gather to group_feature, key, value sets
    chunk_gather <- df %>% gather_("enum", "value", setdiff(colnames(df), group_feature)) %>% filter(!is.na(value)) %>% filter(value)
    # get to factors rather than logicals
    chunk_gather <- chunk_gather %>% select(-value) %>% separate(enum, c("enum", "value"), sep = "[.](?!.*[.])", extra="merge")
    # Add the counts back in
    chunk_gather$sum <- lookup(chunk_gather[ ,group_feature, with=F], as.data.frame(group_feature_sums))

    # build a results matrix
    results <- matrix(data=0, 
                      nrow=length(unique(chunk_gather[[group_feature]])), 
                      ncol=length(unique(chunk_gather$enum)), 
                      dimnames=list(unique(chunk_gather[[group_feature]]), unique(chunk_gather$enum)))

    # I feel dirty.  hope to replace these for loops with something more R'y
    for (feature in unique(chunk_gather$enum)) {
        # Build the table for a single feature
        chunk_cast <- chunk_gather %>% 
            # select the feature
            filter(enum==feature) %>%  #ERROR: Replace "action.error.variety"
            # get rid of the enumeration column as we subsetted to it
            group_by_(group_feature, "value") %>% 
            # convert counts to percentages
            summarize(sum=n()) %>% 
            # create a matrix dataframe that can then have rows summed and compared to all other rows.
            reshape2::dcast(list(group_feature, "value"), value.var="sum", fill=0)
        # get cluster as row names
        if (any(is.na(chunk_cast[[group_feature]]))) {
            stop("The Group Feature cannot have any values of 'NA'")
        } else {
          rownames(chunk_cast) <- chunk_cast[[group_feature]]
        }
          
        feature_chunk_cols <- unique(chunk_gather[chunk_gather$enum == feature]$value)
        feature_chunk_rows <- unique(chunk_gather[[group_feature]])
        
        feature_chunk <- matrix(data=0, 
                          nrow=length(feature_chunk_rows), 
                          ncol=length(feature_chunk_cols), 
                          dimnames=list(feature_chunk_rows, feature_chunk_cols))  %>%
                         as.data.frame()

        feature_chunk[rownames(chunk_cast), colnames(feature_chunk)] <- chunk_cast[,setdiff(colnames(chunk_cast), group_feature)]
        
        feature_chunk <- as.matrix(feature_chunk)

        if (ncol(feature_chunk) > 2) {
            for (group in unique(chunk_gather[[group_feature]])) {
              
                g <- feature_chunk[as.character(group), ]
              
                not_g <- colSums(feature_chunk[setdiff(rownames(feature_chunk), as.character(group)), ])
     
                results[as.character(group), feature] <- 0.5 * chi2.plugin(g, not_g)
                #results[group, feature] <- KL.plugin(feature_chunk[group, 
                #                                                        setdiff(colnames(feature_chunk), group_feature)], 
                #                                             colSums(feature_chunk[setdiff(rownames(feature_chunk), group), 
                #                                                                setdiff(colnames(feature_chunk), group_feature)]))
            }
        }
    }

    results
}

