DESCRIPTION <- "build a table of year by some enumeration in the data. Extremely useful for stream, stacked bar, and line charts."

load(data)

# motive by year (2009-2014) for Breaches
## TO USE: 0. Change the filters in the first line unless you want no Error actions, only Breaches, and no other filters
##         1. Change the starts_with()'s in the first select statement to the columns you want
##         2. Change the "4" in the separate line to the number of columns. (if you're doing actor.Internal.motive.m, it'd be 4 because there's 4 parts.)
##         3. Change the "4" in the select line below the separate line to whatever number you used in step 2.
##         4. Change the "4" in the rename line to whatever number you used in step 2.
## NOTE 1: This only works if all enumerations are the same level down the tree.  (ie no actor.Internal.motive.m joined with actor.unknown)
## NOTE 2: If you remove the separate-select-rename lines you don't join the lower level values
temp <- vz %>% 
  filter(attribute.confidentiality.data_disclosure.Yes, action.Error != TRUE) %>% 
  select(starts_with("actor.Internal.motive"), 
         starts_with("actor.External.motive"), 
         starts_with("actor.Partner.motive"), 
         timeline.incident.year) %>% 
  gather(enum, 
         value, 
         -timeline.incident.year) %>% 
  filter(value) %>% 
  select(-value) %>%
  separate(enum, 1:4, "[.]", extra="merge") %>%
  select(timeline.incident.year, `4`) %>%
  rename(enum = `4`) %>%
  group_by(timeline.incident.year, enum) %>%
  summarize(x = n()) %>%
  mutate(n = sum(x)) %>%
  mutate(freq = round(100 * x/n, 2), `count/total` = paste(x, n, sep="/"))
temp %>% acast(enum~timeline.incident.year, value.var="count/total")
temp %>% acast(enum~timeline.incident.year, value.var="freq", fill=0)