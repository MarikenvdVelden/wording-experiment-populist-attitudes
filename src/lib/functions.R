library(here)
library(kableExtra)
library(tidyverse)
library(DeclareDesign)
library(ggpubr)
library(scales)
library(cobalt)
library(margins)
library(lme4)
#library(ggstatsplot)
library(ggrepel)
library(haven)
library(sjlabelled)
library(patchwork)
library(qualtRics)
library(lavaan)

fig_cols <- yarrr::piratepal(palette = "basel", 
             trans = .2)
fig_cols <- as.character(fig_cols[1:8])

api_key_fn <- here("data/raw-private/qualtrics_api_key.txt")
API <- read_file(api_key_fn) %>% trimws()

render_j2 = function(template, output, data, auto_unbox=TRUE, na="string") {
  data = jsonlite::toJSON(data, pretty=TRUE, auto_unbox=auto_unbox, na=na)
  system(glue::glue("env/bin/j2 --format json {template} -o {output}"), input=data)
}

regression <- function(df, a, ethnic){
  
  depVarList <- df %>% select(matches("POST_[234]"))
  indepVarList <- df %>% select(a, ethnic, D8) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * ethnic + D8,
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("POST_[234]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "a", at = list(ethnic = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, ethnic)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "a", at = list(ethnic = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, ethnic)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression2 <- function(df, a, ethnic){
  
  depVarList <- df %>% select(matches("POST_[234]"))
  indepVarList <- df %>% select(a, ethnic, D8) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * ethnic + D8,
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("POST_[234]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "ethnic", at = list(a = 1:5))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, a)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "ethnic", at = list(a = 1:5))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, a)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression3 <- function(df, a, ethnic){
  
  depVarList <- df %>% select(matches("POST_[234]"))
  indepVarList <- df %>% select(a, ethnic, D8) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * ethnic + D8,
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("POST_[234]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "ethnic", at = list(a = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.96 * SE),
               upper = AME + (1.96 * SE)) %>%
        select(AME, upper, lower, y, a)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "ethnic", at = list(a = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.96 * SE),
               upper = AME + (1.96 * SE)) %>%
        select(AME, upper, lower, y, a)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression4 <- function(df, ethnic){
  
  depVarList <- df %>% select(matches("POST_[234]"))
  indepVarList <- df %>% select(ethnic, D8, matches("POST_[156]")) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ ethnic + 
                                                   POST_1 + POST_5 + 
                                                   POST_6 + D8,
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("POST_[234]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(y = depVarList[i],
               lower = estimate - (1.96 * std.error),
               upper = estimate + (1.96 * std.error)) %>%
        filter(term == "ethnic") %>%
        select(estimate, upper, lower, y)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(y = depVarList[i],
               lower = estimate - (1.96 * std.error),
               upper = estimate + (1.96 * std.error)) %>%
        filter(term == "ethnic") %>%
        select(estimate, upper, lower, y)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression5 <- function(df, a, ethnic){
  
  depVarList <- df %>% select(matches("POST_[234]"))
  indepVarList <- df %>% select(a, ethnic, D8) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ factor(a) * ethnic + D8,
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("POST_[234]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "a", at = list(ethnic = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.96 * SE),
               upper = AME + (1.96 * SE)) %>%
        select(AME, upper, lower, y, ethnic, factor) %>%
        filter(ethnic == 1) 
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "a", at = list(ethnic = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.96 * SE),
               upper = AME + (1.96 * SE)) %>%
        select(AME, upper, lower, y, ethnic, factor) %>%
        filter(ethnic == 1)
      m <- m %>%
        add_case(tmp) %>%
        mutate(factor = recode(factor,
                           `aBavaria` = "Bavaria",
                           `aBerlin` = "Berlin",
                           `aBrandenburg` = "Brandenburg \n (former East Germany)",
                           `aBremen` = "Bremen",
                           `aHamburg` = "Hamburg",
                           `aHessen` = "Hessen",
                           `aLower Saxony` = "Lower Saxony",
                           `aMecklenburg-Western Pomerania` = "Mecklenburg-Western Pomerania \n (former East Germany)",
                           `aNorth Rhine-Westphalia` = "North Rhine-Westphalia",
                           `aSaarland` = "Saarland",
                           `aSaxony` = "Saxony \n (former East Germany)",
                           `aSaxony-Anholt` = "Saxony-Anholt \n (former East Germany)",
                           `aSchleswig-Holstein` = "Schleswig-Holstein",
                           `aThuringia` = "Thuringia  \n (former East Germany)"))
      m <- tibble(m)
      
    }
  }
  return(m)
}
