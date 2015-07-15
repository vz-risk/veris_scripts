DESCRIPTION <- "Data Amount, Actions, and Attributes for EU states from 2013 back."

# Set static variables
data <- "./data-full.rda"
countries <- c('victim.country.AT', 
              'victim.country.BE', 
              'victim.country.BG', 
              'victim.country.HR', 
              'victim.country.CY', 
              'victim.country.CZ', 
              'victim.country.DK', 
              'victim.country.EE', 
              'victim.country.FI', 
              'victim.country.FR', 
              'victim.country.DE', 
              'victim.country.GR', 
              'victim.country.HU', 
              'victim.country.IE', 
              'victim.country.IT', 
              'victim.country.LV', 
              'victim.country.LT', 
              'victim.country.LU', 
              'victim.country.MT', 
              'victim.country.NL', 
              'victim.country.PL', 
              'victim.country.PT', 
              'victim.country.RO', 
              'victim.country.SK', 
              'victim.country.SI', 
              'victim.country.ES', 
              'victim.country.SE', 
              'victim.country.GB')


# load libraries
library(data.table)
library(verisr)
library(dplyr)
library(tidyr)

# load the data
load(data)


# Create an European Union column so we can filter on it
vz.EU <- vz %>% select(match(countries, names(vz)))
vz$victim.country.EU <- ifelse(apply(vz.EU, 1, function(x) any(x==T)), T, F)

# Evalutate the number of records per year
vz %>% filter(victim.country.EU == T) %>% group_by(timeline.incident.year) %>% summarize(count = n()) %>% arrange(desc(timeline.incident.year))
# subset to 2013 and earlier
vz_pre_2014 <- vz %>% filter(timeline.incident.year <= 2013) %>% filter(victim.country.EU == T)

# Break Down Actions
eu_action <- vz_pre_2014 %>% getenum('victim.country', 'action') %>% filter(x!=0)
eu_malware <- vz_pre_2014 %>% getenum('victim.country', 'action.malware.variety') %>% filter(x!=0)
eu_hacking <- vz_pre_2014 %>% getenum('victim.country', 'action.hacking.variety')  %>% filter(x!=0)
eu_social <- vz_pre_2014 %>% getenum('victim.country', 'action.social.variety')  %>% filter(x!=0)
eu_misuse <- vz_pre_2014  %>% getenum('victim.country', 'action.misuse.variety')  %>% filter(x!=0)
eu_error <- vz_pre_2014  %>% getenum('victim.country', 'action.error.variety') %>% filter(x!=0)


# Break Down Attributes
eu_attribute <- vz_pre_2014 %>% getenum('victim.country', 'attribute') %>% filter(x!=0)
eu_availability <- vz_pre_2014 %>% getenum('victim.country', 'attribute.availability.variety') %>% filter(x!=0)
eu_confidentiality_variety <- vz_pre_2014 %>% getenum('victim.country', 'attribute.confidentiality.data.variety') %>% filter(x!=0)

vz_pre_2014$row <- row.names(vz_pre_2014)

temp <- vz_pre_2014 %>% select(match(c(countries, c('row')), names(vz_pre_2014))) %>% 
                        gather(country, value, -row) %>% 
                        filter(value) %>% 
                        select(row, country)
temp$row <- as.numeric(temp$row)
vz_pre_2014$country <- temp %>% arrange(row) %>% select(country)
rm(temp)

# Do the same aggregation through 'gather' for amounts
amt_cols <- grep('attribute.confidentiality.data.amount', names(vz_pre_2014), value=T)
eu_records <- vz_pre_2014 %>% select(match(c(amt_cols, c('country')), names(vz_pre_2014))) %>% 
                              gather(data.variety, amount, -country) %>% 
                              group_by(country, data.variety) %>% 
                              summarize(amount = sum(amount, na.rm=T))

save(eu_records, eu_confidentiality_variety, eu_availability, eu_attribute, eu_error, eu_misuse, eu_social, eu_hacking, eu_malware, eu_action, file="./eu.rda")