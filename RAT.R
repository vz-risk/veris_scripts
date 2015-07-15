DESCRIPTION <- "Count the number of RAT breaches and the associated Record Count by Year"


# LOAD DATA
savefile <- "data-full.rda"
load(savefile)

# IMPORT
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
source('./dbir2015-support.R')  # Similar to https://github.com/jayjacobs/verisr/blob/verisr-addons/R/addons.R
opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE,
               results="markdown", prompt=FALSE, error=FALSE,
               fig.width=8, fig.height=5, cache=FALSE)

theme_set(theme_minimal() + 
            theme(panel.background=element_rect(fill="floralwhite", color="gray75"),
                  panel.grid.major=element_line(color="gray75", size=0.1),
                  axis.ticks=element_blank(),
                  title = element_text(face="italic", size=10)
            ))

# Filter data
fullccc <- getccc(vz)
vz <- vz %>% filter(fullccc$complexity>7 | 
                      action.hacking.variety.DoS | 
                      attribute.confidentiality.data_disclosure.Yes) %>%
  filter(!action.Unknown)

vz <- vz[ ,grep("targeted.Not Applicable", colnames(vz), invert=T), with=F]
vz <- vz[ ,grep("attribute.availability.duration.unit.Not applicable", colnames(vz), invert=T), with=F]

# Data Disclosure & User Desktop/Laptop & Backdoor/C2
vz <- vz %>% filter(attribute.confidentiality.data_disclosure.Yes) %>%
             filter(`asset.assets.variety.U - Desktop` | `asset.assets.variety.U - Laptop`) %>%
             filter(action.malware.variety.Backdoor | action.malware.variety.C2)

vz$timeline.incident.year <- vz$timeline.incident.year + 1

RAT <- vz %>% group_by(timeline.incident.year) %>%
              summarize(n = n(), record_count = sum(attribute.confidentiality.data.amount.Bank, 
                                                    attribute.confidentiality.data.amount.Credentials, 
                                                    attribute.confidentiality.data.amount.Medical, 
                                                    attribute.confidentiality.data.amount.Personal,
                                                    attribute.confidentiality.data.amount.Payment, na.rm=T)) %>%
              select(timeline.incident.year, n, record_count) %>%
              arrange(desc(timeline.incident.year))
write.csv(RAT, "./rat.csv")
