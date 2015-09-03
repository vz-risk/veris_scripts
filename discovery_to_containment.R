DESCRIPTION <- "Analyze discovery to containment time.  First check the data to make sure it's useful for analysis.  Then do all the stuff to calculate the difference.  Then chart it in a histogram."

# Check the number of incidents we have to work with.  Should be about 145 out of 200k
vz %>% filter(timeline.discovery.value > 0 & timeline.containment.value > 0) %>% dim

# Test to ensure actions are not too biased
vz %>% filter(timeline.discovery.value > 0 & timeline.containment.value > 0) %>% getenum("action")
# (They aren't)

# Test that attributes aren't too biased
vz %>% filter(timeline.discovery.value > 0 & timeline.containment.value > 0) %>% getenum("attribute")
# (They aren't)

# Gather the containment times
chunk <-  vz %>% filter(timeline.discovery.value > 0 & timeline.containment.value > 0) %>% 
  select(starts_with("timeline.containment.unit."),
         timeline.containment.value, 
         plus.master_id) %>%
  gather(`Containment Unit`, `Containment Value`, -plus.master_id, -timeline.containment.value) %>%
  filter(!is.na(`Containment Value`)) %>%
  filter(`Containment Value`) %>%
  select(-`Containment Value`) %>%
  filter(!grepl("Unknown", `Containment Unit`)) %>% 
  rename(`Containment Value` = timeline.containment.value)

# gather the discovery times
temp <-  vz %>% filter(timeline.discovery.value > 0 & timeline.containment.value > 0) %>% 
  select(starts_with("timeline.discovery.unit."),
         timeline.discovery.value, 
         plus.master_id) %>%
  gather(`Discovery Unit`, `Discovery Value`, -plus.master_id, -timeline.discovery.value) %>%
  filter(!is.na(`Discovery Value`)) %>%
  filter(`Discovery Value`) %>%
  select(-`Discovery Value`) %>%
  filter(!grepl("Unknown", `Discovery Unit`)) %>% 
  rename(`Discovery Value` = timeline.discovery.value)

# Join discovery and containment times
chunk <- full_join(chunk, temp, by="plus.master_id")

# get rid of the one line with an NA in it at the end
chunk <- chunk[1:144,]

# Convert names into number of days
chunk$`Containment Unit` <- chunk$`Containment Unit` %>% plyr::mapvalues(c("timeline.containment.unit.Days", "timeline.containment.unit.Hours", "timeline.containment.unit.Minutes", "timeline.containment.unit.Months", "timeline.containment.unit.Weeks", "timeline.containment.unit.Years"), c(1, 1/24, 1/(24 * 60), 30, 7, 365))
chunk$`Discovery Unit` <- chunk$`Discovery Unit` %>% plyr::mapvalues(c("timeline.discovery.unit.Days", "timeline.discovery.unit.Hours", "timeline.discovery.unit.Minutes", "timeline.discovery.unit.Months", "timeline.discovery.unit.Weeks", "timeline.discovery.unit.Years", "timeline.discovery.unit.Seconds"), c(1, 1/24, 1/(24 * 60), 30, 7, 365, 1/(24*60*60)))

# cast factors to numerics
chunk$`Discovery Unit` <- as.numeric(levels(chunk$`Discovery Unit`))[chunk$`Discovery Unit`]
chunk$`Containment Unit` <- as.numeric(levels(chunk$`Containment Unit`))[chunk$`Containment Unit`]

# Calculate the differential
chunk <- chunk %>% mutate(differential = `Containment Value` * `Containment Unit` - `Discovery Value` * `Discovery Value`)

# since sometimes contaiment is from discovery & sometimes it's from compromise, we'll fix that
chunk <- chunk %>% filter(differential < 0) %>% mutate(differential = `Containment Value` * `Containment Unit`)

gg <- chunk %>% ggplot() 
gg <- gg + aes(x=differential) 
gg <- gg + geom_histogram()
gg <- gg + geom_vline(aes(xintercept=median(differential), color='red'))
gg <- gg + annotate("text", x=9, y=-1, label="Median", color="red")
gg <- gg + labs(x="Days", y="Incidents", title="Discovery to Containment Time (Incidents)")
gg <- gg + theme_hc()
gg

