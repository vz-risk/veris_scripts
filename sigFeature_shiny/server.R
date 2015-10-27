### SET VARIABLES
veris_scripts <- "/Users/v685573/Documents/Development/vzrisk/veris_scripts/"
options(shiny.maxRequestSize=30*1024^2) 
### LOAD PACKAGES
library(MASS)
library(binom)
library(gridExtra)
library(data.table)
library(verisr)
library(lazyeval)
library(reshape2)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
source(paste0(veris_scripts, "sigFeature.R"))

### LOAD FUNCTIONS
setjenum <- function(veris, enums, trim=10, unknowns=c("Unknown", " - Other")) { 
  ## cycle through each enum passed in
  chunk <- lapply(seq_along(enums), function(i) {
    thisenum <- getenum(veris, enums[i], add.freq=F, add.n=T, fillzero=F, exclusive=T)
    if(nrow(thisenum)) {
      ## look for unknowns
      unknowns <- c("Unknown| - Other")
      # save the index of the unknowns locations
      umatches <- grepl(unknowns, thisenum$enum)
      # assume no Unknowns are found
      ucount <- 0
      if(any(umatches)) {
        # set ucount for unknowns if they are found
        ucount <- sum(thisenum$x[umatches])
        # remove the matched unknowns from the enumdf
        thisenum <- filter(thisenum, !umatches)
      }
      # modify the n and set freq now that it's clear of unknowns
      thisenum <- thisenum %>% mutate(n=n-ucount, freq=x/n) %>% arrange(freq)
      
      # reduce length of enumdf it if trim is set
      if(!is.null(trim) && trim>0) thisenum <- tail(thisenum, trim)
      thisenum$enum <- factor(thisenum$enum, levels=thisenum$enum, ordered=T)
      # get a count of total records found
      thisn <- thisenum$n[1]
      # set the title
      title <- paste0(enums[i], " (n=", format(thisn, big.mark=",", scientific=F), ")\n",
                      "Unknowns=", format(ucount, big.mark=",", scientific=F), "/", 
                      format((thisn+ucount), big.mark=",", scientific=F),
                      " (", round(ucount/(thisn+ucount), 2)*100, "%)")
      ## get the height of 
      hts <- 0.85 + nrow(thisenum)/4 # /2 # (nrow(thisenum)*ht_mult)+ht_pad
    } else {  # empty df
      ucount <- 0
      title <- "not found"
      hts <- 1
    }
    list(ucount=ucount, enum=enums[i], df=thisenum, title=title, height=hts)
  })
  chunk$ptall <- max(sapply(chunk, function(x) x$height))
  # returns a list for each :
  #   "ucount" : count of unknown in enumeration
  #   "df" : the data frame returned by getenum()
  #   "title" : vector of titles for plots
  #   "enum" : the specific enum for this chunk
  #   "height" height of each plot
  #   "ptall" the max height the entire plot
  chunk
}

plotjchunk <- function(chunk) {
  maxht <- max(sapply(chunk, function(x) if(is.list(x)) { x$height } else { 0 }))
  plots <- lapply(chunk[sapply(chunk, is.list)], function(x) {
    if (nrow(x$df)==0) {
      textGrob(paste("No records with", x$enum, "defined.", sep="\n"))
    } else {
      rdif <- maxht-x$height
      simplejbar(x$df, x$title, plot.margin=unit(c(0,0,rdif,0), "inches"))
    }
  })
  if(length(plots)==1) {
    blank <- textGrob(" ")
    wth <- list(widths=c(0.05, 1, 0.05))
    plots <- list(blank, plots[[1]], blank)
    ret <- do.call(arrangeGrob, c(plots, nrow=1, wth))
  } else {
    ret <- do.call(arrangeGrob, c(plots, nrow=1))    
  }
  ret
}

simplejbar <- function(enumdf, title, ...) {
  enumdf <- cbind(enumdf, binom.confint(enumdf$x, enumdf$n, method="wilson"))
  enumdf$lbl <- paste0(enumdf$x, ' (', round(enumdf$freq*100, 1), "%)")
  yexp <- max(enumdf$upper)*1.42
  gg <- ggplot(enumdf, aes(x=enum, y=freq, label=lbl))
  gg <- gg + ggtitle(title)
  gg <- gg + geom_bar(width=0.90, stat="identity", fill="steelblue")
  gg <- gg + geom_errorbar(aes(ymin=lower, ymax=upper), color="lightsteelblue", alpha=2/3, size=5, width=0)
  gg <- gg + geom_text(hjust=-0.1,color="black", size=3) + coord_flip()
  gg <- gg + scale_y_continuous(expand = c(0, 0), limits=c(0, yexp), labels=percent)
  gg <- gg + xlab("") + ylab("") + theme(...)
  gg    
}


###
shinyServer(function(input, output) {
  button <- 0

  observe({
    if (input$submit == 0)
      return()
    
    if (input$submit > button) {
      button <- input$submit
      inFile <- input$file1
      if (is.null(inFile) | !exists(input$df_name))
        return(NULL)
      
      load(inFile$datapath)
      
      incidents <-  get(input$df_name) %>% 
                    select(timeline.incident.year,
                           contains("variety"), 
                           contains("vector"),
                           starts_with("attribute.confidentiality.data_disclosure"), 
                           starts_with("data_discovery"), 
                           starts_with("pattern."),
                           matches("^victim.(industry2.|employee_count|orgsize)"), 
                           matches("timeline.*.unit.*"),
                           starts_with("victim.country."))
    }
    
    output$incidentCount <- renderText({
      num_incidents <- nrow(incidents)
      num_breaches <- incidents %>% filter(attribute.confidentiality.data_disclosure.Yes) %>% nrow()
      paste(num_incidents, "incidents.", num_breaches, "breaches.")
    })

    isolate({
      # convert to breaches if desired
      if (input$breaches) {
        chunk <- incidents %>% filter(attribute.confidentiality.data_disclosure.Yes)
      }  else {
        chunk <- incidents
      }
      
      
      output$featuresO <- renderUI({
        selectInput('featureI', 
                    "Feature to investigate", 
                    setdiff(colnames(incidents), c("timeline.incident.year")))
      })

      # Sig Features
      output$sigFeaturesO <- renderUI({
        sf <- chunk %>% select(-timeline.incident.year) %>% sigFeatures(input$featureI)
        x_val <- strsplit(input$featureI, "[.](?!.*[.])", perl=T)[[1]][2]
        viz <- sf[x_val, ] %>% melt() %>% add_rownames() %>% arrange(desc(value))
        selectInput("sigFeatureI", "Choose a significant feature to enumerate.", paste0(viz$rowname, ", ", round(viz$value, 2)))
      })
    
      # Render a bar chart enumeration of the data w/ feature chosen in sigFeatures
      output$featureEnumO <- renderPlot({
        sigFeature <- c(strsplit(input$sigFeatureI, ",")[[1]][1])
        
        # Try doing it the Jay way
        chunk <- chunk[chunk[[input$featureI]] == TRUE, ] %>%  
          setjenum(sigFeature)
        print(plotjchunk(chunk))
      })
      
      # Render a bar chart enumeration of the data w/o feature chosen in sigFeatures
      output$notFeatureEnumO <- renderPlot({
        sigFeature <- c(strsplit(input$sigFeatureI, ",")[[1]][1])
        
        # Try doing it the Jay way
        chunk <- chunk[chunk[[input$featureI]] == FALSE, ] %>%  
          setjenum(sigFeature)
        print(plotjchunk(chunk))
      })  
      
      # Label under bar with feature
      output$withFeatureText <- renderText({
        paste("With", input$featureI)
      })
      
      # Label under bar without feature
      output$notFeatureText <- renderText({
        paste("Without", input$featureI)
      })
      
    })
  })
})