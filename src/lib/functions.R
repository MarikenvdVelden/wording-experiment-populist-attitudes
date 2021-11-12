library(here)
library(kableExtra)
library(tidyverse)
library(DeclareDesign)
library(ggpubr)
library(scales)
library(cobalt)
library(margins)
library(lme4)
library(ggstatsplot)
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
