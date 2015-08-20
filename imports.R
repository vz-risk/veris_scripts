suppressPackageStartupMessages(library(pbapply))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(streamgraph))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(dygraphs))
suppressPackageStartupMessages(library(parcoords))
suppressPackageStartupMessages(library(taucharts))
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(slopegraph))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(binom))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(verisr))
suppressPackageStartupMessages(library(lazyeval))
source('/Volumes/verizon/Customer and Partner Data/DBIR/dbir2015-support.R')
opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE,
               results="markdown", prompt=FALSE, error=FALSE,
               fig.width=8, fig.height=5, cache=FALSE)

theme_set(theme_minimal() + 
            theme(panel.background=element_rect(fill="floralwhite", color="gray75"),
                  panel.grid.major=element_line(color="gray75", size=0.1),
                  axis.ticks=element_blank(),
                  title = element_text(face="italic", size=10)
            ))