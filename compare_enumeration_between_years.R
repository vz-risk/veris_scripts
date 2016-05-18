DESCRIPTION <- "Calculate the change in a value between two years."

chunk <- vcdb %>% 
  # TODO: Change the years in the below line
  filter(plus.dbir_year %in% c(2015, 2016)) %>% 
  # TODO: Subset the data
  filter(pattern.Crimeware) %>%
  # TODO: Select the enumeration you are looking for change in
  select(plus.dbir_year, starts_with("action.malware.variety.")) %>%
  select(-ends_with("Unknown")) %>%
  # filter where that attribute doesn't exist
  filter(rowSums(.[ , !"plus.dbir_year", with=F]) > 0) %>%
  # calculate N
  group_by(plus.dbir_year) %>% 
  mutate(n=n()) %>% 
  ungroup() %>%
  # calculate counts and frequency of the enumeration
  gather("enum", "value", -plus.dbir_year, -n) %>%
  filter(value) %>%
  group_by(plus.dbir_year, enum) %>%
  summarize(x=n(), n=as.numeric(names(sort(table(n), decreasing=T)[1]))) %>%
  mutate(freq=x/n) %>%
  # Calculate change. TODO: Adjust year appropriately.
  # If looking at relative percent change, uncomment below
#  dcast(enum~plus.dbir_year, value.var="freq", fill=0) %>%
#  mutate(change=paste0(round((`2016`-`2015`/`2015`*100, 2),"%") %>%
  # if looking at absolute change, uncomment below
#  dcast(enum~plus.dbir_year, value.var="x", fill=0) %>%
#  mutate(change=(`2016`-`2015`)) %>% 
  arrange(desc(change))