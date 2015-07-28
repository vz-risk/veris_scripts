
# Initial ingestion and filter
load(savefile)
fullccc <- getccc(vz)
vz <- vz %>% filter(fullccc$complexity>7 |
                    + action.hacking.variety.DoS |
                    + attribute.confidentiality.data_disclosure.Yes) %>%
                    + filter(!action.Unknown)
vz <- vz[ ,grep("targeted.Not Applicable", colnames(vz), invert=T), with=F]
vz <- vz[ ,grep("attribute.availability.duration.unit.Not applicable", colnames(vz), invert=T), with=F]

# Actions for Overall
action <- vz %>% getenum('action.malware.variety')
action$action <- "Malware"
tmp <- vz %>% getenum('action.hacking.variety')
tmp$action <- "Hacking"
action <- bind_rows(action, tmp)
tmp <- vz %>% getenum('action.social.variety')
tmp$action <- "Social"
action <- bind_rows(action, tmp)
tmp <- vz %>% getenum('action.physical.variety')
tmp$action <- "Physical"
action <- bind_rows(action, tmp)
tmp <- vz %>% getenum('action.misuse.variety')
tmp$action <- "Misuse"
action <- bind_rows(action, tmp)
tmp <- vz %>% getenum('action.error.variety')
tmp$action <- "Error"
action <- bind_rows(action, tmp)
tmp <- vz %>% getenum('action.environmental.variety')
tmp$action <- "Environmental"
action <- bind_rows(action, tmp)
action <- action %>% arrange(desc(x)) %>% select(-freq, -n)
action$n <- dim(vz)[1]
action$freq <- round(100 * action$x/action$n, 2)
action_overall <- action

# Actions for Industry 52
vz <- vz %>% filter(victim.industry2 == "52")
action <- vz %>% getenum('action.malware.variety')
action$action <- "Malware"
tmp <- vz %>% getenum('action.hacking.variety')
tmp$action <- "Hacking"
action <- bind_rows(action, tmp)
tmp <- vz %>% getenum('action.social.variety')
tmp$action <- "Social"
action <- bind_rows(action, tmp)
tmp <- vz %>% getenum('action.physical.variety')
tmp$action <- "Physical"
action <- bind_rows(action, tmp)
tmp <- vz %>% getenum('action.misuse.variety')
tmp$action <- "Misuse"
action <- bind_rows(action, tmp)
tmp <- vz %>% getenum('action.error.variety')
tmp$action <- "Error"
action <- bind_rows(action, tmp)
tmp <- vz %>% getenum('action.environmental.variety')
tmp$action <- "Environmental"
action <- bind_rows(action, tmp)
action <- action %>% arrange(desc(x)) %>% select(-freq, -n)
action$n <- dim(vz)[1]
action$freq <- round(100 * action$x/action$n, 2)
action_52 <- action

# aggregate
action_52$Industry <- "52"
action_overall$Industry <- "All"
action <- bind_rows(action_52, action_overall)

# Filter & Plot
action <- action %>% filter(freq > 1)
ggplot(action2, aes(x=action, y=freq)) + geom_bar(aes(fill=Industry), position="dodge", stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))