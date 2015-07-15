DESCRIPTION <- "Filters to analyze breaches involving credentials, comparing theft of credentials and use of stolen credentials."


# Patterns
used_not_stolen <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == F & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("pattern")
print(simplebar(used_not_stolen, "Creds Used But Not Stolen Patterns"))
used_and_stolen <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("pattern")
print(simplebar(used_and_stolen, "Creds Used And Stolen Patterns"))
stolen_not_used <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == F) %>% getenum("pattern")
print(simplebar(stolen_not_used, "Creds Stolen But Not Used Patterns"))

# Action
used_not_stolen <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == F & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("action")
print(simplebar(used_not_stolen, "Creds Used But Not Stolen Action"))
used_and_stolen <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("action") 
print(simplebar(used_and_stolen, "Creds Used And Stolen Action"))
stolen_not_used <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == F) %>% getenum("action") 
print(simplebar(stolen_not_used, "Creds Stolen But Not Used Action"))

# Hacking
enums1 <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == F & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("action.hacking.variety") %>% filter(x != 0) %>% select(enum)
enums2 <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("action.hacking.variety") %>% filter(x != 0) %>% select(enum)
enums3 <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == F) %>% getenum("action.hacking.variety") %>% filter(x != 0) %>% select(enum)
enums <- unique(rbind(enums1, enums3, enums3))

used_not_stolen <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == F & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("action.hacking.variety") %>%  filter(enum %in% enums$enum)
print(simplebar(used_not_stolen, "Creds Used But Not Stolen Hacking"))
used_and_stolen <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("action.hacking.variety") %>%  filter(enum %in% enums$enum)
print(simplebar(used_and_stolen, "Creds Used And Stolen Hacking"))
stolen_not_used <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == F) %>% getenum("action.hacking.variety") %>% filter(enum %in% enums$enum)
print(simplebar(stolen_not_used, "Creds Stolen But Not Used Hacking"))

# Malware
enums1 <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == F & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("action.malware.variety") %>% filter(x != 0) %>% select(enum)
enums2 <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("action.malware.variety") %>% filter(x != 0) %>% select(enum)
enums3 <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == F) %>% getenum("action.malware.variety") %>% filter(x != 0) %>% select(enum)
enums <- unique(rbind(enums1, enums3, enums3))

used_not_stolen <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == F & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("action.malware.variety") %>%  filter(enum %in% enums$enum)
print(simplebar(used_not_stolen, "Creds Used But Not Stolen Malware"))
used_and_stolen <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("action.malware.variety") %>%  filter(enum %in% enums$enum)
print(simplebar(used_and_stolen, "Creds Used And Stolen Malware"))
stolen_not_used <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == F) %>% getenum("action.malware.variety") %>% filter(enum %in% enums$enum)
print(simplebar(stolen_not_used, "Creds Stolen But Not Used Malware"))

# Social
used_not_stolen <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == F & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("action.social.variety")
print(simplebar(used_not_stolen, "Creds Used But Not Stolen Social"))
used_and_stolen <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == T) %>% getenum("action.social.variety")
print(simplebar(used_and_stolen, "Creds Used And Stolen Social"))
stolen_not_used <- vz %>% filter(attribute.confidentiality.data.variety.Credentials == T & vz$"action.hacking.variety.Use of stolen creds" == F) %>% getenum("action.social.variety")
print(simplebar(stolen_not_used, "Creds Stolen But Not Used Social"))
