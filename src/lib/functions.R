library(here)
library(kableExtra)
library(tidyverse)
library(DeclareDesign)
library(ggpubr)
library(scales)
library(cobalt)
library(margins)
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
  
  depVarList <- df %>% select(matches("POST_[23]"))
  indepVarList <- df %>% select(a, ethnic, D8) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * ethnic + D8,
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("POST_[23]")) %>% colnames()
  
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
  
  depVarList <- df %>% select(matches("POST_[23]"))
  indepVarList <- df %>% select(a, ethnic, D8) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * ethnic + D8,
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("POST_[23]")) %>% colnames()
  
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
  
  depVarList <- df %>% select(matches("POST_[23]"))
  indepVarList <- df %>% select(a, ethnic, D8) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * ethnic + D8,
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("POST_[23]")) %>% colnames()
  
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
  
  depVarList <- df %>% select(matches("POST_[23]"))
  indepVarList <- df %>% select(ethnic, D8, matches("POST_[156]")) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ ethnic + 
                                                   POST_1 + POST_5 + 
                                                   POST_6 + D8,
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("POST_[23]")) %>% colnames()
  
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
  
  depVarList <- df %>% select(matches("POST_[23]"))
  indepVarList <- df %>% select(a, ethnic, D8) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ factor(a) * ethnic + D8,
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("POST_[23]")) %>% colnames()
  
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

regression6 <- function(df, a, ethnic){
  
  depVarList <- df %>% select(matches("POST_[23]"))
  indepVarList <- df %>% select(a, ethnic, D8) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * ethnic + D8,
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("POST_[23]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "ethnic", at = list(a = 0:5))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, a)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "ethnic", at = list(a = 0:5))) %>%
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

regression7 <- function(df, scale){
  
  depVarList <- df %>% dplyr::select(pop_vote, Afd_vote, Linke_vote)
  indepVarList <- df %>% dplyr::select(scale, D8, PT8)
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ scale + D8 + PT8,
                                                 data= indepVarList))
  depVarList <- df %>% dplyr::select(pop_vote, Afd_vote, Linke_vote) %>%
    colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        filter(term == "scale") %>%
        mutate(y = depVarList[i],
               lower = estimate - (1.96 * std.error),
               upper = estimate + (1.96 * std.error)) %>%
        dplyr::select(estimate, upper, lower, y)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        filter(term == "scale") %>%
        mutate(y = depVarList[i],
               lower = estimate - (1.96 * std.error),
               upper = estimate + (1.96 * std.error)) %>%
        dplyr::select(estimate, upper, lower, y)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

theme_ipsum <- function(base_family="Arial Narrow", base_size = 11.5,
                        plot_title_family=base_family, plot_title_size = 18,
                        plot_title_face="bold", plot_title_margin = 10,
                        subtitle_family=base_family, subtitle_size = 12,
                        subtitle_face = "plain", subtitle_margin = 15,
                        strip_text_family = base_family, strip_text_size = 12,
                        strip_text_face = "plain",
                        caption_family = base_family, caption_size = 9,
                        caption_face = "italic", caption_margin = 10,
                        axis_text_size = base_size,
                        axis_title_family = subtitle_family, axis_title_size = 9,
                        axis_title_face = "plain", axis_title_just = "rt",
                        plot_margin = margin(30, 30, 30, 30),
                        grid_col = "#cccccc", grid = TRUE,
                        axis_col = "#cccccc", axis = FALSE, ticks = FALSE) {
  
  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)
  
  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())
  
  if (inherits(grid, "character") | grid == TRUE) {
    
    ret <- ret + theme(panel.grid=element_line(color=grid_col, size=0.2))
    ret <- ret + theme(panel.grid.major=element_line(color=grid_col, size=0.2))
    ret <- ret + theme(panel.grid.minor=element_line(color=grid_col, size=0.15))
    
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
    }
    
  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }
  
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color="#2b2b2b", size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }
  
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  
  ret <- ret + theme(axis.text.x=element_text(size=axis_text_size, margin=margin(t=0)))
  ret <- ret + theme(axis.text.y=element_text(size=axis_text_size, margin=margin(r=0)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y.right=element_text(hjust=yj, size=axis_title_size, angle=90,
                                                     family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face, family=strip_text_family))
  ret <- ret + theme(panel.spacing=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family, face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family, face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family, face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)
  
  ret
  
}
