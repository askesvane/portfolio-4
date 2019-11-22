Assignment 4 - Heart rate, respiration and interpersonal coordination
=====================================================================

Physiological data (here heart rate \[variability\], and respiration)
are increasingly popular. Historically treated as pernicious noise to be
regressed out of neuro-imaging data, there is now increasing research on
how these signals tell us something important about cognition and beyond
being just a signal of cognitive processes also impact them in
interesting ways. Advanced sport science, and the quantified self
movement (closely followed by marketing and communication) have hailed
continuous physiological tracking as a powerful way to access and modify
attitudes, habits, and performance. Further, as team coordination (in
the military, in decision processes and organizational contexts) is more
and more in focus, research has attempted to measure how interpersonal
coordination between physiological systems might tell us something
important about e.g. emotional and cognitive coordination. See
references in the reading list for more on this.

In this assignment, you will learn to: - collect physiological data -
pre-process physiological data (and grow further your mad R skills) -
model the continuous interdependence between two signals (using a
multilevel model as proxy for a dynamical system approach) -
conservatively assess the presence of coordination between to signals in
a controlled context

This assignment has two parts. The first part familiarizes you with
heart rate, and respiration data and their preprocessing. The second
part explores how to analyze interpersonal coordination of these
signals.

These are the questions you need to be able to answer at the end of the
assignment (aka that you need to submit as part of the portfolio)

1.  How do you preprocess heart rate and respiration data? Describe the
    process. If any data needs to be excluded, list the excluded data
    and motivate the exclusion.

2.  Do you observe interpersonal coordination in heart rate and
    respiration? Describe your control baseline, the method used to
    quantify coordination, and the statistical models used to infer
    whether coordination was higher than in the baseline. Report the
    results of the models.

3.  Do you observe differences in coordination between conditions?
    Report the models and results.

4.  Is respiration coordination a likely driver of heart rate
    coordination? Describe how you would test for it. Bonus points if
    you actually run the tests and report methods and results.

N.B. to give you a bit more data I included data from previous years
(Study1, Study2 and Study 3). Note that synchronouns and turn-taking are
the same across both studies, but the third condition is different: in
the first year it was self-paced joint reading; in the second year it
was the tv-series conversation.

Let's get started
-----------------

### Exploring physiological signals

-   Choose one pair (one pair, three conditions)
-   Load the logs
-   Produce a plot of the participants' respiration signal and a
    different one of the participants' HR signal. N.B: remember the
    slides: artifacts, downsampling, scaling. N.B. The
    gridExtra::grid.arrange() function allows you to display the plots
    side by side. E.g. grid.arrange(plot1, plot2, plot3, ncol=3). There
    are also smarter packages, like cowplot and ggpubr.
-   Can you eye-ball which condition if any displays more physiological
    coordination?

### First we read one data file and identify the procedure

    # Load the libraries
    pacman::p_load(tidyverse,gridExtra,groupdata2,reshape2,lme4,lmerTest)

-   Load the file
-   correctly identify all columns
-   plot the data
-   deal with the artifacts
-   downsample the dat
-   Add a column for study, group, trial and condition

Try with just one file - in order to construct function
-------------------------------------------------------

    # Load the file
    one_pair <- read.csv("data/Study1_G1_T1_Synchronous.csv")

    # Plot
    plot1 <- ggplot(data = one_pair) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")
    plot1

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    ## Remove outliers

    ### Tip, check the function below
    removeOuts <- function(ts,threshold){
      ts[ts > (mean(ts,na.rm=T) +
                 (threshold*sd(ts,na.rm=T))) | 
           ts < (mean(ts,na.rm=T) -
                 (threshold*sd(ts,na.rm=T)))] = mean(ts,na.rm=T)
      return(ts)
    }

    # threshold=2.5 # Default value at 2.5 sds from the mean

    one_pair_extra <- one_pair

    # Remove outlieres in respiration
    # one_pair_extra$Resp1 <- removeOuts(one_pair_extra$Resp1,2.5)
    # one_pair_extra$Resp2 <- removeOuts(one_pair_extra$Resp2,2.5)
    # 
    # # Remove outlieres in heart rate
    # one_pair_extra$HR1 <- removeOuts(one_pair_extra$HR1,2.5)
    # one_pair_extra$HR2 <- removeOuts(one_pair_extra$HR2,2.5)

    one_pair_extra <-  one_pair_extra %>% mutate_at(vars(Resp1,Resp2,HR1,HR2), removeOuts, threshold = 2.5)


    # Run plot on outliere-removed data

    # for respiration
    plot_resp_outl <- ggplot(data = one_pair_extra) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "Time", y = "Respiration",title = "Respiration (with outlieres removed)") +
      theme(legend.position="bottom")
    plot_resp_outl

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-2-2.png)

    # for heartrate
    plot_HR_outl <- ggplot(data = one_pair_extra) +
      geom_path(aes(time, HR1, color = "P1")) +
      geom_path(aes(time, HR2, color = "P2")) +
      labs(x = "Time", y = "Heart Rate",title = "Heart Rate (with outlieres removed)") +
      theme(legend.position="bottom")
    plot_HR_outl

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-2-3.png)

    # Plot raw data againt those with the artiacts removed
    gridExtra::grid.arrange(plot_HR_outl,plot_resp_outl, plot1)

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-2-4.png)

    ## Scale
    ### Tip: if scale() gives some issues, try the one below

    # Scale function
    z_scale <- function(column){
      column_c <- (column - mean(column)) / sd(column)
    }

    scaled_data = one_pair_extra %>% mutate_at(vars(Resp1,Resp2,HR1,HR2), z_scale)


    # Plot again to check how scaled data look like

    # resp scaled
    plot_resp_scaled <- ggplot(data = scaled_data) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "Rime", y = "Respiration",title = "Respiration Scaled") +
      theme(legend.position="bottom")
    plot_resp_scaled

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-2-5.png)

    # HR scaled
    plot_HR_scaled <- ggplot(data = scaled_data) +
      geom_path(aes(time, HR1, color = "P1")) +
      geom_path(aes(time, HR2, color = "P2")) +
      labs(x = "Time", y = "Heart Rate", title = "Heart Rate Scaled") +
      theme(legend.position="bottom")
    plot_resp_scaled

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-2-6.png)

    # Plot all plots together
    gridExtra::grid.arrange(plot_resp_outl,plot_HR_outl,plot_resp_scaled,plot_HR_scaled,nrow = 2)

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-2-7.png)

    ## Downsample
    ### This is tricky, so you can have a look at my code  (relying on Ludvig's groupdata2) if you get stuck

    scaled_data$row <- seq.int(nrow(scaled_data))

    data_down = scaled_data %>%
      group(n = 100, method = 'greedy') %>%
      dplyr::summarise(
        time = mean(time,na.rm=T),
        HR1 = mean(HR1,na.rm=T),
        HR2 = mean(HR2,na.rm=T),
        Resp1 = mean(Resp1,na.rm=T),
        Resp2 = mean(Resp2,na.rm=T),
        rowname = row[1]) #the index we use to put them back together 

    ## Plot the downsampled data
    plot_downS <- ggplot(data = data_down) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")
    plot_downS

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-2-8.png)

    ## Now add the group, trial, condition to the cleaned up, scaled, downsampled data
    ## Tip the info is in the file name
    match <- str_match('Study1_G1_T1_Synchronous.csv', "Study([\\d]+)_G([\\d]+)_T([\\d]+)_([:alpha:]+)")
    x <- c("file", "study", "group", "trial", "condition")
    colnames(match) <- x

    # Adding the information from the filename to the dataframe
    data_down <- cbind(data_down,match)

Now we are ready to go to load and pre-process all files
--------------------------------------------------------

Go through all the files (with a function passed onto map\_df), check
which files should be excluded, if any, and save the pre-processed
time-series

A couple of tips: - looping is oh so slow. Making a function and using
Map/Map\_df is your salvation. - each study restarts the group
numbering, so you should make sure to change that (e.g. 100 \* Study +
Group) - you need to make sure all the data are meaningful or something
has to be removed. Plotting is your friend. E.g.
"Study1\_G1\_T1\_Synchronous" has one bad respiration signal. We could
replace it with NAs

    ### Outliere function
    removeOuts <- function(ts,threshold){
      ts[ts > (mean(ts,na.rm=T) +
                 (threshold*sd(ts,na.rm=T)))] = mean(ts,na.rm=T) + (threshold*sd(ts,na.rm=T))
      ts[ts < (mean(ts,na.rm=T) -
                 (threshold*sd(ts,na.rm=T)))] = mean(ts,na.rm=T) - (threshold*sd(ts,na.rm=T))
      return(ts)
    }

    ### Scale function
    z_scale <- function(column){
      column_c <- (column - mean(column)) / sd(column)
    }

    ### Preprocessing function (Define a function running the loading, artifact removal, scaling, downsampling, info adding)
    data_preprocess <- function(filename, threshold = 3){
      
      # to be filled in 
      file <- read.csv(filename) #file loading
      
      # change column names to be the same
      colnames(file)[which(names(file) == "min")] <- "time"
      
      # As numeric
      file <- file %>% mutate_at(c("Resp1","Resp2","HR1","HR2","time"), as.numeric)
      
      # removing outlieres based on the threshold in the 4 columns in the list
      d1 <- file %>% mutate_at(c("Resp1","Resp2","HR1","HR2"), removeOuts, threshold)
      
      # Scaling the same 4 columns
      d1 <- d1 %>% mutate_at(c("Resp1","Resp2","HR1","HR2"), z_scale)

      # Downsampling with code from slides
      d1$row <- seq.int(nrow(d1))
      
      d1 = d1 %>%
      group(n = 1000, method = 'greedy') %>%
      dplyr::summarise(
        time = mean(time,na.rm=T),
        HR1 = mean(HR1,na.rm=T),
        HR2 = mean(HR2,na.rm=T),
        Resp1 = mean(Resp1,na.rm=T),
        Resp2 = mean(Resp2,na.rm=T),
        rowname = row[1]) 
      
      match <- str_match(filename, "Study([\\d]+)_G([\\d]+)_T([\\d]+)_([:alpha:]+)")
      x <- c("file", "study", "group", "trial", "condition")
      colnames(match) <- x
      
      d1 <- cbind(d1,match)

      # Detecting what data is bad because the belt was loose
      detector1 <- ifelse(duplicated(d1$Resp1)==T,"duplicate","not-duplicate")
      detector2 <- ifelse(duplicated(d1$Resp2)==T,"duplicate","not-duplicate")
      
      if(sum(detector1=="duplicate") >= 8 | sum(detector2=="duplicate") >= 8) {
        d1$Resp1 <- NA
        d1$Resp2 <- NA
      } 

      return(d1)
      
    }


    #  Identify all files to be read
    phys_data = list.files(path = "C:/Users/askes/Dropbox/University/Cognitive Science/3. semester/Experimental Methods 3/portfolio-4/data",pattern = ".csv", full.names = T) %>% purrr::map_df(data_preprocess) 

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    # Making a unique pair ID
    phys_data$study <- as.numeric(phys_data$study)
    phys_data$group <- as.numeric(phys_data$group)
    phys_data$time <- as.numeric(phys_data$time)

    phys_data$uPairID <- (100 * phys_data$study + phys_data$group)

    #Normalizing the time values
    #Assuming that the large values are millisecond
    #choose 400 arbitrarily because it is above a reasonable minute count.
    phys_data[which(phys_data$time > 400),]$time <- phys_data[which(phys_data$time > 400),]$time / 1000 / 60
     
    #time since 0
    phys_data <- phys_data %>% group_by(uPairID, trial) %>% mutate(actual_time_min = time - min(time))

    # change timename
    colnames(phys_data)[2] <- "time_min"

    # Save the data
    write.csv(phys_data,"C:/Users/askes/Dropbox/University/Cognitive Science/3. semester/Experimental Methods 3/Portfolio-4/resp_data.csv", row.names = FALSE)

Now we need to run some analysis
--------------------------------

Let's start with a multilevel model that accounts for - stability (how
each signal is autocorrelated) - interpersonal dependence (each signal
is dependent from the previous state of the other signal)

The data needs to be further prepared, so we can analyze both
participants in the same model. We need to turn the data into a long
format: - a column indicating own hr and one own respiration - a column
indicating other hr and one other respiration - a column indicating
change in hr from previous round and one in respiration

We can then run an analysis where change is a function of one's previous
state (stability, see slides), and the other's previous state
(coupling). Make sure to: - set up the most interesting contrasts: how
do these parameters vary by condition? which condition should be
baseline? - set up the right random effects. - N.B. the model will be
slow. Make sure it works on a subset of the data first!

Bonus question: what if we include an additional layer? Is my heart rate
just adjusting to yours, or also to how much you are adjusting to mine?
- to start answering this we can add a column indicating the previous
change in hr in the other and one in respiration - we can then build on
the previous models by also adding the previous change in the other

Long format
===========

    # Import data from downloaded csv.
    phys_data <- read.csv("resp_data.csv")

    # Remove all data in study 3 longer than 3 min.
    phys_data$HR1 <- ifelse(phys_data$actual_time_min > 3 & phys_data$study == 3, NA, phys_data$HR1)
    phys_data$HR2 <- ifelse(phys_data$actual_time_min > 3 & phys_data$study == 3, NA, phys_data$HR2)
    phys_data$Resp1 <- ifelse(phys_data$actual_time_min > 3 & phys_data$study == 3, NA, phys_data$Resp1)
    phys_data$Resp2 <- ifelse(phys_data$actual_time_min > 3 & phys_data$study == 3, NA, phys_data$Resp2)

    ### Remove data for specific files in Heart Rate
    list <- c("Study2_G5_T1_Synchronous","Study2_G5_T2_TurnTaking","Study2_G5_T3_Conversation","Study2_G6_T1_TurnTaking","Study2_G6_T2_Conversation","Study2_G6_T3_Synchronous","Study2_G8_T1_TurnTaking","Study2_G8_T2_Synchronous","Study2_G8_T3_Conversation","Study2_G9_T1_Synchronous","Study3_G9_T1_Conversation","Study3_G9_T2_Synchronous","Study3_G9_T3_TurnTaking","Study3_G1_T3_Conversation","Study4_G3_T1_MovementGuided","Study4_G3_T2_MovementCoop","Study4_G3_T3_Synchronous","Study4_G5_T1_Synchronous","Study4_G5_T2_TurnTaking","Study4_G5_T3_Conversation","Study4_G6_T3_Conversation","Study4_G6_T1_TurnTaking","Study4_G6_T2_Synchronous","Study4_G7_T1_MovementGuided","Study4_G7_T2_MovementCoop","Study4_G7_T3_Synchronous","Study4_G8_T4_MovementCoop")

    # Apply removal list on data
    phys_data$HR1 <- ifelse(phys_data$file %in% list , NA, phys_data$HR1)
    phys_data$HR2 <- ifelse(phys_data$file %in% list , NA, phys_data$HR2)

    # Genearate a column for each: previous HR1, HR2, Resp1, Resp2. And genearate a column for each: change in HR1, HR2, Resp1, Resp2
    phys_data <- phys_data %>% group_by(group,study,trial) %>% 
      mutate(
        
        # For each previous
        HR1_lead = lead(HR1,1),
        HR2_lead = lead(HR2,1),
        Resp1_lead = lead(Resp1,1),
        Resp2_lead = lead(Resp2,1),
        
        # For each change
        change_HR1 = (HR1_lead - HR1),
        change_HR2 = (HR2_lead - HR2),
        change_Resp1 = (Resp1_lead - Resp1),
        change_Resp2 = (Resp2_lead - Resp2))

    # Make the data long, so we can analyze both participants at the same time 
    ## N.B. This is a bit tricky and you might have to do it in several steps

    ####Long format 

    d_hr_lead <- 
      gather(phys_data, # data        
             participant, HR_lead, # new vars
             HR1_lead, HR2_lead) %>% #old vars
      
      select( # drop irrelevant vars
        actual_time_min, HR_lead, participant, study, group, condition, trial) %>%
      
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))


    d_resp_lead <- 
      gather(phys_data, # data        
             participant, Resp_lead, # new vars
             Resp1_lead, Resp2_lead) %>% #old vars
      
      select( # drop irrelevant vars
        actual_time_min, Resp_lead, participant, study, group, condition, trial) %>%
      
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))


    d_hr_change_self <- 
      gather(phys_data, # data        
             participant, HR_change_self, # new vars
             change_HR1, change_HR2) %>% #old vars
      
      select( # drop irrelevant vars
        actual_time_min, HR_change_self, participant, study, group, condition, trial) %>%
      
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))


    d_resp_change_self <- 
      gather(phys_data, # data        
             participant, Resp_change_self, # new vars
             change_Resp1, change_Resp2) %>% #old vars
      
      select( # drop irrelevant vars
        actual_time_min, Resp_change_self, participant, study, group, condition, trial) %>%
      
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    d_hr_self <- 
      gather(phys_data, # data        
             participant, HR_self, # new vars
             HR1, HR2) %>% #old vars
      
      select( # drop irrelevant vars
        actual_time_min, HR_self, participant, study, group, condition, trial) %>%
      
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    d_resp_self <- 
      gather(phys_data, # data        
             participant, Resp_self, # new vars
             Resp1, Resp2) %>% #old vars
      
      select( # drop irrelevant vars
        actual_time_min, Resp_self, participant, study, group, condition, trial) %>%
      
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    phys_data_other <- phys_data %>% 
      rename(
        HR1 = "HR2",
        HR2 = "HR1",
        Resp1 = "Resp2",
        Resp2 = "Resp1",
        change_HR1 = "change_HR2",
        change_HR2 = "change_HR1",
        change_Resp1 = "change_Resp2",
        change_Resp2 = "change_Resp1"
      )

    d_hr_change_other <- 
      gather(phys_data_other, # data        
             participant, HR_change_other, # new vars
             change_HR1, change_HR2) %>% #old vars
      
      select( # drop irrelevant vars
        actual_time_min, HR_change_other, participant, study, group, condition, trial) %>%
      
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))


    d_resp_change_other <- 
      gather(phys_data_other, # data        
             participant, Resp_change_other, # new vars
             change_Resp1, change_Resp2) %>% #old vars
      
      select( # drop irrelevant vars
        actual_time_min, Resp_change_other, participant, study, group, condition, trial) %>%
      
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))


    d_hr_other <- 
      gather(phys_data_other, # data        
             participant, HR_other, # new vars
             HR1, HR2) %>% #old vars
      
      select( # drop irrelevant vars
        actual_time_min, HR_other, participant, study, group, condition, trial) %>%
      
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))


    d_resp_other <- 
      gather(phys_data_other, # data        
             participant, Resp_other, # new vars
             Resp1, Resp2) %>% #old vars
      
      select( # drop irrelevant vars
        actual_time_min, Resp_other, participant, study, group, condition, trial) %>%
      
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))


    dd <- cbind(d_hr_change_self,d_hr_change_other,d_resp_change_self,d_resp_change_other,d_hr_lead,d_resp_lead,d_hr_self,d_hr_other,d_resp_self,d_resp_other) 

    dd <- dd %>%  select(actual_time_min,participant,group,condition,trial,study,HR_self,Resp_self,HR_other,Resp_other,HR_lead,Resp_lead,HR_change_self,HR_change_other,Resp_change_self,Resp_change_other)

    #create unique participant ID
    dd$participant <- dd$participant + (dd$study * 1000)

    # Plot data
    phys_data %>% subset(study==4) %>%
      ggplot() + 
      geom_line(aes(actual_time_min,HR1),color="red") + 
      geom_line(aes(actual_time_min,HR2),color="blue") + 
      facet_grid(group ~ trial)

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-4-1.png)

     phys_data %>% subset(study==4) %>%
      ggplot() + 
      geom_line(aes(actual_time_min,Resp1),color="red") + 
      geom_line(aes(actual_time_min,Resp2),color="blue") + 
      facet_grid(group ~ trial)

    ## Warning: Removed 175 rows containing missing values (geom_path).

    ## Warning: Removed 175 rows containing missing values (geom_path).

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-4-2.png)

    # Save the data
    write.csv(dd,"C:/Users/askes/Dropbox/University/Cognitive Science/3. semester/Experimental Methods 3/Portfolio-4/dd_data.csv", row.names = FALSE)

    # Import data from downloaded csv.
    dd_data <- read.csv("dd_data.csv")

Contrast and models
===================

Set the most interesting contrast e.g. by defining synchronous or
conversation as the baseline.

Notes from class

why are we doing change and not future? - we are answering different
questions - allows us to investigate homoestasis (the change will tell
us the further we go away from the baseline, if the signal will tend to
go towards the baseline again) - tendency to stabilize vs the tendency
to run away - model says: if the other is excited , you should get more
excited no matter what - would make more sense to make a new variable:
HR\_other - HR\_self --&gt; then if the other is less excited than you,
you should not necessarily get more excited, but maybe get dragged down
(even though both are above baseline)

    # based on variable values
    dd_data4 <- dd_data[which(dd_data$study==4), ]

    # Relevel
    dd_data4$condition <- relevel(dd_data4$condition, ref = "Conversation")

    # As numeric
    dd_data4$study <- as.numeric(dd_data4$study)
    dd_data4$group <- as.numeric(dd_data4$group)
    dd_data4$trial <- as.numeric(dd_data4$trial)

    # Multiple participants
    model1 <- lmer(HR_change_self ~ 1 + condition + (HR_self + HR_other) * condition + 
        (1 + condition | participant) + 
        (1 + condition | group), data = dd_data4)

    ## boundary (singular) fit: see ?isSingular

    summary(model1)

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## HR_change_self ~ 1 + condition + (HR_self + HR_other) * condition +  
    ##     (1 + condition | participant) + (1 + condition | group)
    ##    Data: dd_data4
    ## 
    ## REML criterion at convergence: 15004.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.6542 -0.4877  0.0601  0.5585  6.6767 
    ## 
    ## Random effects:
    ##  Groups      Name                    Variance  Std.Dev.  Corr             
    ##  participant (Intercept)             0.000e+00 0.000e+00                  
    ##              conditionMovementCoop   4.905e-12 2.215e-06   NaN            
    ##              conditionMovementGuided 2.935e-12 1.713e-06   NaN -0.90      
    ##              conditionSynchronous    3.191e-14 1.786e-07   NaN -0.75  0.38
    ##              conditionTurnTaking     2.582e-12 1.607e-06   NaN -0.76  0.63
    ##  group       (Intercept)             0.000e+00 0.000e+00                  
    ##              conditionMovementCoop   1.771e-12 1.331e-06   NaN            
    ##              conditionMovementGuided 7.074e-12 2.660e-06   NaN  0.07      
    ##              conditionSynchronous    2.325e-11 4.822e-06   NaN  0.93  0.14
    ##              conditionTurnTaking     3.329e-11 5.770e-06   NaN -0.06 -0.61
    ##  Residual                            4.005e-01 6.329e-01                  
    ##       
    ##       
    ##       
    ##       
    ##       
    ##   0.69
    ##       
    ##       
    ##       
    ##       
    ##  -0.07
    ##       
    ## Number of obs: 7762, groups:  participant, 16; group, 8
    ## 
    ## Fixed effects:
    ##                                    Estimate Std. Error         df t value
    ## (Intercept)                       4.674e-03  1.362e-02  7.747e+03   0.343
    ## conditionMovementCoop             4.965e-03  2.429e-02  7.747e+03   0.204
    ## conditionMovementGuided          -5.280e-03  2.196e-02  7.747e+03  -0.240
    ## conditionSynchronous              6.987e-04  2.240e-02  7.747e+03   0.031
    ## conditionTurnTaking              -5.254e-03  1.965e-02  7.747e+03  -0.267
    ## HR_self                          -3.210e-01  1.518e-02  7.747e+03 -21.141
    ## HR_other                          1.241e-02  1.518e-02  7.747e+03   0.817
    ## conditionMovementCoop:HR_self     9.806e-02  2.710e-02  7.747e+03   3.618
    ## conditionMovementGuided:HR_self   3.560e-02  2.430e-02  7.747e+03   1.465
    ## conditionSynchronous:HR_self      5.006e-02  2.461e-02  7.747e+03   2.034
    ## conditionTurnTaking:HR_self       3.040e-02  2.169e-02  7.747e+03   1.402
    ## conditionMovementCoop:HR_other    3.358e-02  2.710e-02  7.747e+03   1.239
    ## conditionMovementGuided:HR_other  2.689e-02  2.430e-02  7.747e+03   1.107
    ## conditionSynchronous:HR_other    -2.264e-02  2.461e-02  7.747e+03  -0.920
    ## conditionTurnTaking:HR_other     -1.293e-02  2.169e-02  7.747e+03  -0.596
    ##                                  Pr(>|t|)    
    ## (Intercept)                      0.731425    
    ## conditionMovementCoop            0.838056    
    ## conditionMovementGuided          0.809988    
    ## conditionSynchronous             0.975117    
    ## conditionTurnTaking              0.789173    
    ## HR_self                           < 2e-16 ***
    ## HR_other                         0.413703    
    ## conditionMovementCoop:HR_self    0.000299 ***
    ## conditionMovementGuided:HR_self  0.143012    
    ## conditionSynchronous:HR_self     0.041951 *  
    ## conditionTurnTaking:HR_self      0.161081    
    ## conditionMovementCoop:HR_other   0.215330    
    ## conditionMovementGuided:HR_other 0.268526    
    ## conditionSynchronous:HR_other    0.357541    
    ## conditionTurnTaking:HR_other     0.551191    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 15 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    model2 <- lmer(HR_change_self ~ 0 + condition + (HR_self + HR_other) : condition + 
        (0 + condition | participant) + 
        (0 + condition | group), data = dd_data4)

    ## boundary (singular) fit: see ?isSingular

    summary(model2)

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## HR_change_self ~ 0 + condition + (HR_self + HR_other):condition +  
    ##     (0 + condition | participant) + (0 + condition | group)
    ##    Data: dd_data4
    ## 
    ## REML criterion at convergence: 15004.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.6542 -0.4877  0.0601  0.5585  6.6767 
    ## 
    ## Random effects:
    ##  Groups      Name                    Variance  Std.Dev.  Corr             
    ##  participant conditionConversation   2.289e-10 1.513e-05                  
    ##              conditionMovementCoop   1.096e-09 3.311e-05  1.00            
    ##              conditionMovementGuided 6.836e-10 2.615e-05  0.42  0.42      
    ##              conditionSynchronous    2.983e-09 5.461e-05 -0.22 -0.22 -0.97
    ##              conditionTurnTaking     1.762e-09 4.198e-05  0.85  0.85  0.54
    ##  group       conditionConversation   0.000e+00 0.000e+00                  
    ##              conditionMovementCoop   8.284e-09 9.102e-05   NaN            
    ##              conditionMovementGuided 7.295e-10 2.701e-05   NaN  0.10      
    ##              conditionSynchronous    9.117e-10 3.019e-05   NaN  0.19 -0.29
    ##              conditionTurnTaking     5.168e-10 2.273e-05   NaN  0.07  0.65
    ##  Residual                            4.005e-01 6.329e-01                  
    ##       
    ##       
    ##       
    ##       
    ##       
    ##  -0.38
    ##       
    ##       
    ##       
    ##       
    ##   0.39
    ##       
    ## Number of obs: 7762, groups:  participant, 16; group, 8
    ## 
    ## Fixed effects:
    ##                                    Estimate Std. Error         df t value
    ## conditionConversation             4.674e-03  1.362e-02  7.746e+03   0.343
    ## conditionMovementCoop             9.639e-03  2.011e-02  7.593e+03   0.479
    ## conditionMovementGuided          -6.056e-04  1.722e-02  7.731e+03  -0.035
    ## conditionSynchronous              5.373e-03  1.779e-02  7.685e+03   0.302
    ## conditionTurnTaking              -5.801e-04  1.417e-02  7.726e+03  -0.041
    ## conditionConversation:HR_self    -3.210e-01  1.518e-02  7.747e+03 -21.141
    ## conditionMovementCoop:HR_self    -2.230e-01  2.245e-02  7.747e+03  -9.931
    ## conditionMovementGuided:HR_self  -2.854e-01  1.898e-02  7.747e+03 -15.041
    ## conditionSynchronous:HR_self     -2.710e-01  1.937e-02  7.747e+03 -13.991
    ## conditionTurnTaking:HR_self      -2.906e-01  1.549e-02  7.747e+03 -18.759
    ## conditionConversation:HR_other    1.241e-02  1.518e-02  7.747e+03   0.817
    ## conditionMovementCoop:HR_other    4.600e-02  2.245e-02  7.747e+03   2.049
    ## conditionMovementGuided:HR_other  3.931e-02  1.898e-02  7.747e+03   2.071
    ## conditionSynchronous:HR_other    -1.023e-02  1.937e-02  7.747e+03  -0.528
    ## conditionTurnTaking:HR_other     -5.162e-04  1.549e-02  7.747e+03  -0.033
    ##                                  Pr(>|t|)    
    ## conditionConversation              0.7314    
    ## conditionMovementCoop              0.6318    
    ## conditionMovementGuided            0.9720    
    ## conditionSynchronous               0.7626    
    ## conditionTurnTaking                0.9673    
    ## conditionConversation:HR_self      <2e-16 ***
    ## conditionMovementCoop:HR_self      <2e-16 ***
    ## conditionMovementGuided:HR_self    <2e-16 ***
    ## conditionSynchronous:HR_self       <2e-16 ***
    ## conditionTurnTaking:HR_self        <2e-16 ***
    ## conditionConversation:HR_other     0.4137    
    ## conditionMovementCoop:HR_other     0.0405 *  
    ## conditionMovementGuided:HR_other   0.0384 *  
    ## conditionSynchronous:HR_other      0.5973    
    ## conditionTurnTaking:HR_other       0.9734    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 15 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    # We do not expect an interaction effect between HR_self and HR_other --> that is a conceptual choice, we could make the model i a different way

    # We expect that everyhting will chnage accordign to the structure of our data
            # by participant, group (and study)
            # we are telling the model that there is dependency between data points (same particioants)
            # (HR_self + HR_other) : Condition has been removed because otherwise it will run forever ..... 

    # just tell me what the slopes and intercepts are by condition (not the change of change)
    # this is the same model but it will calculate all estimates we will use for all conditions (including uncertainty)


    # The model will be the same 

Effects we are interested in:

structural effects (fixed effects) - the stability - coupling -
condition

Though we have different pairs of different individuals - we cannot
expect coupling to be the same across all participants - the same for
stability - if you are very engaged with a person, the condition might
influence how big the coupling will be

OUTPUT: - three intercepts - 6 slopes - some of them should be negative
(make physiological sense that after extreme heart rates, it will tend
to go towards baseline again)

Surrogate = probably the best baseline

Model change as a function of own and other previous state Bonus points:
Add to the previous model also change in the other to see whether my
adaptation is influenced by the other's adaptation.

Now we need to create control baselines.
----------------------------------------

First shuffled controls, then surrogate pairs.

combine persons from different groups - code in slides

type = surrogate vs. real

    groups <- as.numeric(as.character(unique(dd_data$group[dd_data$study == 4]))) # list all pairs
    surrogateList <- expand.grid(a = groups, b = groups) # Identify all combinations of 2 pairs
    surrogateList = subset(surrogateList, a != b) # exclude combinations with identical pairs
    surrogateList

    ##    a b
    ## 2  2 1
    ## 3  3 1
    ## 4  4 1
    ## 5  5 1
    ## 6  6 1
    ## 7  7 1
    ## 8  8 1
    ## 9  1 2
    ## 11 3 2
    ## 12 4 2
    ## 13 5 2
    ## 14 6 2
    ## 15 7 2
    ## 16 8 2
    ## 17 1 3
    ## 18 2 3
    ## 20 4 3
    ## 21 5 3
    ## 22 6 3
    ## 23 7 3
    ## 24 8 3
    ## 25 1 4
    ## 26 2 4
    ## 27 3 4
    ## 29 5 4
    ## 30 6 4
    ## 31 7 4
    ## 32 8 4
    ## 33 1 5
    ## 34 2 5
    ## 35 3 5
    ## 36 4 5
    ## 38 6 5
    ## 39 7 5
    ## 40 8 5
    ## 41 1 6
    ## 42 2 6
    ## 43 3 6
    ## 44 4 6
    ## 45 5 6
    ## 47 7 6
    ## 48 8 6
    ## 49 1 7
    ## 50 2 7
    ## 51 3 7
    ## 52 4 7
    ## 53 5 7
    ## 54 6 7
    ## 56 8 7
    ## 57 1 8
    ## 58 2 8
    ## 59 3 8
    ## 60 4 8
    ## 61 5 8
    ## 62 6 8
    ## 63 7 8

    HR_change ~ 
      (0 + (HR_self + HR_other) * Condition) : type + 
      (0 + condition | participant) + 
      (0 + condition | group)

    ## HR_change ~ (0 + (HR_self + HR_other) * Condition):type + (0 + 
    ##     condition | participant) + (0 + condition | group)

    # We are investigating if type matters (that pair are actually more coordinated than non-pairs)

### Creating controls: shuffled controls

Shuffled controls break the temporal dependencies of time-series by
shuffling the value within one time-series. This ensures the
"coordination" observed is not due to the actual values in the series
and not their sequence. Tip: sample() is your friend, but make sure to
shuffle things within participant/condition and not throughout the whole
dataset

    # Create a shuffled dataset

    # Create a shuffled dataset

    # Concatenate it to the original dataset (and remember to have a column telling you which is which)

    # Create the same models as in the previous chunk, but adding an interaction by shuffled vs. real

### TRICKY! Creating controls: surrogate pair controls

-   Per each real pair, identify at least one surrogate pair (matching
    one of the participants, with somebody doing the same task, but in a
    different pair)

Identify unique pairs within a given study (to keep things manageable)
and create list of possible surrogate pairs (e.g. individual 1 from pair
1 and individual 2 from pair 2) Starting from the wide format, create
"surrogate" dataset with the data from surrogate pairs

    #Generating a subset only with study 4
    phys_data4 <- subset(phys_data, study == 4)


    # Making empty dataframe    
    surrogate_data <- phys_data[0,]

    Groups <- as.numeric(as.character(unique(phys_data$group[phys_data$study==4]))) # List all pairs
    SurrogateList <- expand.grid(a = Groups, b = Groups) # Identify all possible combinations of 2 pairs
    SurrogateList = subset(SurrogateList, a != b)  # exclude combinations with identical pairs

    for (i in 1:nrow(SurrogateList)){  # loop through all combinations
      x <- subset(phys_data4, group == SurrogateList$a[i]) # subset data from the first pair    
      y <- subset(phys_data4, group != SurrogateList$a[i]) # subset data from the second pair   
      newPairID <- c(800 + ((1:4)*i)) # create new pair id
      
        for (co in c("Synchronous","TurnTaking", "Conversation")){ # loop through conditions
        if (co %in% unique(x$condition) & co %in% unique(y$condition)){      
          # check that both pairs have the data for that condition
          
          z1 <- subset(x, condition==co) # subset only that condtion from first pair
          z2 <- subset(y, condition==co) # subset only that condtion from second pair
          
          if (nrow(z1) > nrow(z2)) {    # make sure data have same length in both pairs
            z1<-z1[1:nrow(z2),]
          }
          
          if (nrow(z2) > nrow(z1)) { 
            z2<-z2[1:nrow(z1),]
          }
          
          w1 <- z1 %>% mutate(  # assemble new pair combining the 2 pairs
            HR2 = z2$HR2,
            Resp2 = z2$Resp2,
            HR2_lead = z2$HR2_lead, 
            Resp2_lead = z2$Resp2_lead, 
            change_HR2 = z2$change_HR2, 
            change_Resp2 = z2$change_Resp2)
          
           if (nrow(surrogate_data) == 0) {
            surrogate_data <- w1
            }
            else {
              surrogate_data <- rbind(surrogate_data,w1)
            }
          
          
          
        }}}
          

    # in surrogate_data there is 29204 obs.

Make it into long format

Create models as in chunks above, but adding an interaction with the
Real vs. Surrogate variable (exclude shuffled ones for simplicity)

### Effects of respiration coordination on heart rate coordination

-   describe how you would test those.
-   Optional: run the models and report them
