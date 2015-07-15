DESCRIPTION <- "Filter for database-related breaches."

# Database Preso / Blog

# Number of Database breaches
vz %>% filter(`asset.assets.variety.S - Database`==T | action.hacking.variety.SQLi==T | `action.malware.variety.SQL injection`==T) %>% dim

# Number of years covered
vz %>% filter(`asset.assets.variety.S - Database`==T | action.hacking.variety.SQLi==T | `action.malware.variety.SQL injection`==T) %>% select(timeline.incident.year) %>% unique
vz %>% filter(`asset.assets.variety.S - Database`==T | action.hacking.variety.SQLi==T | `action.malware.variety.SQL injection`==T) %>% select(timeline.incident.year) %>% unique %>% dim
# Breaches by Year
gg <- ggplot(db.years, aes(x=enum, y=x)) + geom_bar(stat="identity", fill="steelblue") + coord_flip() + xlab("Year") + ylab("Number of Incidents")
print(gg)

# Data Disclosure
db.disclosure <- vz %>% filter(`asset.assets.variety.S - Database`==T | action.hacking.variety.SQLi==T | `action.malware.variety.SQL injection`==T) %>% getenum('attribute.confidentiality.data_disclosure')
gg <- ggplot(db.disclosure, aes(x=enum, y=x)) + geom_bar(stat="identity", fill="steelblue") + coord_flip() + xlab("Disclosure Status") + ylab("Number of Incidents")
print(gg)

# Compare Attributes
db.attribute <- vz %>% filter(`asset.assets.variety.S - Database`==T | action.hacking.variety.SQLi==T | `action.malware.variety.SQL injection`==T) %>% getenum('attribute')
gg <- ggplot(db.attribute, aes(x=enum, y=x)) + geom_bar(stat="identity", fill="steelblue") + coord_flip() + xlab("Attribute Breached") + ylab("Number of Incidents")
print(gg)
