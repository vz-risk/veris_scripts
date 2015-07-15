DESCRIPTION <- "Filter a specific industry.  Only use one of the lines at a time."

load("./data.R")

# Filter to the first 3 characters of a NAICS code. (In this case 316)
# Replace 316 with the beginning of the naics code you would like.  (Can be 0 to 6 numbers.)
vz <- vz[grepl("^316.*", vz$victim.industry), ]

# Filter multiple NAICS codes
# In this case, filtering to 31*, 32*, and 33* for manufacturing
vz <- vz[grepl("^3[1-3].*", vz$victim.industry), ]

# Filter a specific 2-number industry
# In this case Finance and Insurance (52)
vz <- vz %>% filter(victim.industry2 == "51")

