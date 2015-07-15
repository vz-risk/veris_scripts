DESCRIPTION <- "Get top Motives, Data Types, Actions, Assets, and Patterns.  Potentially for a year and industry"

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(binom))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(verisr))
suppressPackageStartupMessages(library(pbapply))
suppressPackageStartupMessages(library(rjson))
source('./dbir2015-support.R')
opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE,
               results="markdown", prompt=FALSE, error=FALSE,
               fig.width=8, fig.height=5, cache=FALSE)

theme_set(theme_minimal() + 
            theme(panel.background=element_rect(fill="floralwhite", color="gray75"),
                  panel.grid.major=element_line(color="gray75", size=0.1),
                  axis.ticks=element_blank(),
                  title = element_text(face="italic", size=10)
            ))


# Import data
savefile <- "./data-full.rda"
load(savefile)

# Filter data
fullccc <- getccc(vz)
vz <- vz %>% filter(fullccc$complexity>7 | 
                      action.hacking.variety.DoS | 
                      attribute.confidentiality.data_disclosure.Yes) %>%
                      filter(!action.Unknown)

vz <- vz[ ,grep("targeted.Not Applicable", colnames(vz), invert=T), with=F]
vz <- vz[ ,grep("attribute.availability.duration.unit.Not applicable", colnames(vz), invert=T), with=F]

# Filter by a specific year
vz <- vz %>% filter(timeline.incident.year == "2012")
#vz <- vz %>% filter(timeline.incident.year == "2013")
#vz <- vz %>% filter(timeline.incident.year == "2014")


# Filter by a specific industry if desired
vz <- vz[grepl("^3[1-3].*", vz$victim.industry), ]


##  Motives ##
chunk <- vz
motive <- chunk %>% getenum('actor.external.motive')
tmp <- chunk %>% getenum('actor.internal.motive')
motive <- full_join(motive, tmp, by="enum")
motive$x <- motive$x.x + motive$x.y
motive <- motive %>% select(enum, x)
tmp <- chunk %>% getenum('actor.partner.motive')
motive <- full_join(motive, tmp, by="enum")
motive$x <- motive$x.x + motive$x.y
motive <- motive %>% select(enum, x)
motive$n <- dim(chunk)[1]
motive$percent <- motive$x/motive$n
motive$percent <- motive$percent * 100
motive$percent <- round(motive$percent, 2)
motive <- motive %>% arrange(desc(x))
names(motive) <- c("Motive", "Count", "Total", "Percent")
motive$Motive[motive$Motive == "NA"] <- "Accident"
motive



## Data Type ##
chunk <- vz
data_type <- chunk %>% getenum("attribute.confidentiality.data.variety")
data_type$freq <- round(data_type$freq * 100, 2)
data_type <- data_type %>% arrange(desc(x))
names(data_type) <- c("Data Type", "Count", "Total", "Percent")
data_type$`Data Type`[data_type$`Data Type` == "Personal"] <- "Personal (PII and SSN)"
data_type$`Data Type`[data_type$`Data Type` == "Credentials"] <- "Credentials (Login & Password)"
data_type$`Data Type`[data_type$`Data Type` == "Medical"] <- "Medical (PHI)"
data_type$`Data Type`[data_type$`Data Type` == "Payment"] <- "Payment (PCI and credit card)"
data_type


## Assets ##
chunk <- vz
assets <- chunk %>% getenum("asset.variety")
assets$freq <- round(assets$freq * 100, 2)
assets <- assets %>% arrange(desc(x))
names(assets) <- c("Asset", "Count", "Total", "Percent")
assets


## ACTIONS ##
chunk <- vz
action <- chunk %>% getenum('action.malware.variety')
action$action <- "Malware"
tmp <- chunk %>% getenum('action.hacking.variety')
tmp$action <- "Hacking"
action <- bind_rows(action, tmp)
tmp <- chunk %>% getenum('action.social.variety')
tmp$action <- "Social"
action <- bind_rows(action, tmp)
tmp <- chunk %>% getenum('action.physical.variety')
tmp$action <- "Physical"
action <- bind_rows(action, tmp)
tmp <- chunk %>% getenum('action.misuse.variety')
tmp$action <- "Misuse"
action <- bind_rows(action, tmp)
tmp <- chunk %>% getenum('action.error.variety')
tmp$action <- "Error"
action <- bind_rows(action, tmp)
tmp <- chunk %>% getenum('action.environmental.variety')
tmp$action <- "Environmental"
action <- bind_rows(action, tmp)
action <- action %>% arrange(desc(x)) %>% select(-freq, -n)
action$n <- dim(chunk)[1]
action$freq <- action$x/action$n
action$freq <- round(action$freq * 100, 2)
action <- action[,c(3, 1, 2, 4, 5)]
names(action) <- c("Action Type", "Action", "Count", "Total", "Percent")
action[1:20,]


## Patterns
chunk <- vz
patterns <- chunk %>% getenum("pattern")
patterns$freq <- round(patterns$freq * 100, 2)
patterns <- patterns %>% arrange(desc(x))
names(patterns) <-c("Pattern", "Count", "Total", "Percent")
patterns