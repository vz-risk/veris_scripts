DESCRIPTION <- "List the top 20 actions."

load("./data.Rda")

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
action[1:20,]