---
title: "Documentation of Household Survey to assess CC in Ethiopia"
author: "GMDAC"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
  bookdown::gitbook:
   split_by: none
   self_contained: true
   css: "style_gitbook.css"
   toc_depth: 6
   
---

<head>
<style>

@import url('https://fonts.googleapis.com/css2?family=Roboto&display=swap');
@import url('https://fonts.googleapis.com/css2?family=Open+Sans&display=swap');
@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Sans&display=swap');
@import url('https://fonts.googleapis.com/css2?family=Montserrat:ital&display=swap');

</style>


</head>


```{r include = F}
  knitr::opts_chunk$set(echo = F, warning = F, message = F)
  library(dlookr)


kebeles_admin <- rio::(file.path(dir_reference_clean, "kebeles_cc.rds"))




```


# Introduction

This gitbook intends to document all the steps conducted for the implementation of the Community Conversation Househod Survey (CCHS) in Ethiopia. 


# List of CC Kebeles

On July 17th 2021, CSA shared a clean list of the Kebeles where the CC has been implemented (sent by Daniel Nigatu). This list has been merged with oficial data (**add source here**) for the evauluation to be have a sampling frame. The list inlcudes `r nrow(kebeles_admin)` kebeles. Below is a brief description of this data.

```{r }


#Number of kebeles per zone
zones <- kebeles_admin %>% 
  tabyl(region, zone) %>%
  pivot_longer(-region,
               names_to = "zone",
               values_to = "obs") %>%
  filter(obs > 1)


knitr::kable(zones, caption = "Number of Kebeles where the CC has been implemented, by region and zone")
