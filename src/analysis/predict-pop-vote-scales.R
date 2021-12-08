detach_package <- function(pkg, character.only = FALSE){
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}
packages <- c( "ltm", "GPArotation", "Hmisc", "mirt",
               "lattice","lordif", "semPlot",
               "semTools", "nFactors", "corrr")
for(i in length(packages)){
  detach_package(packages[i], TRUE)
}

Ethnic <- c(0,1) 

for(i in 1:length(Ethnic)){
    df <- d %>%
    filter(ethnic == Ethnic[i]) %>%
    mutate(pop_vote = ifelse(D6 == "AfD", 1,
                      ifelse(D6 == "Left", 1, 0)),
           Linke_vote = ifelse(D6 == "Left", 1,0)) %>%
  rename(scale = cfa_pa) 

if(i==1){
  exp <- tidy(lm(pop_vote ~ scale + D8 + PT8, df)) %>%
    mutate(id = "cfa_pa",
           ethnic = Ethnic[i],
           y = "pop_vote") 
  tmp <- tidy(lm(Afd_vote ~ scale + D8 + PT8, df)) %>%
                 mutate(id = "cfa_pa",
                        ethnic = Ethnic[i],
                        y = "Afd_vote") 
  exp <- exp %>% add_case(tmp)
  tmp <- tidy(lm(Linke_vote ~ scale + D8 + PT8, df)) %>%
               mutate(id = "cfa_pa",
                      ethnic = Ethnic[i],
                      y = "Linke_vote")
  exp <- exp %>% add_case(tmp)}
else{
  exp <- tidy(lm(pop_vote ~ scale + D8 + PT8, df)) %>%
    mutate(id = "cfa_pa",
           ethnic = Ethnic[i],
           y = "pop_vote") 
  tmp <- tidy(lm(Afd_vote ~ scale + D8 + PT8, df)) %>%
    mutate(id = "cfa_pa",
           ethnic = Ethnic[i],
           y = "Afd_vote") 
  exp <- exp %>% add_case(tmp)
  tmp <- tidy(lm(Linke_vote ~ scale + D8 + PT8, df)) %>%
    mutate(id = "cfa_pa",
           ethnic = Ethnic[i],
           y = "Linke_vote")
  exp <- exp %>% add_case(tmp)}
}

for(i in 1:length(Ethnic)){
  df <- d %>%
    dplyr::filter(ethnic == Ethnic[i]) %>%
    mutate(pop_vote = ifelse(D6 == "AfD", 1,
                      ifelse(D6 == "Left", 1, 0)),
           Linke_vote = ifelse(D6 == "Left", 1,0))  %>%
    rename(scale = add_pa) 
  
  if(i==1){
    tmp <- tidy(lm(pop_vote ~ scale + D8 + PT8, df)) %>%
      mutate(id = "add_pa",
             ethnic = Ethnic[i],
             y = "pop_vote") 
    exp <- exp %>% add_case(tmp)
    tmp <- tidy(lm(Afd_vote ~ scale + D8 + PT8, df)) %>%
      mutate(id = "add_pa",
             ethnic = Ethnic[i],
             y = "Afd_vote") 
    exp <- exp %>% add_case(tmp)
    tmp <- tidy(lm(Linke_vote ~ scale + D8 + PT8, df)) %>%
      mutate(id = "add_pa",
             ethnic = Ethnic[i],
             y = "Linke_vote")
    exp <- exp %>% add_case(tmp)}
  else{
    tmp <- tidy(lm(pop_vote ~ scale + D8 + PT8, df)) %>%
      mutate(id = "add_pa",
             ethnic = Ethnic[i],
             y = "pop_vote") 
    exp <- exp %>% add_case(tmp)
    tmp <- tidy(lm(Afd_vote ~ scale + D8 + PT8, df)) %>%
      mutate(id = "add_pa",
             ethnic = Ethnic[i],
             y = "Afd_vote") 
    exp <- exp %>% add_case(tmp)
    tmp <- tidy(lm(Linke_vote ~ scale + D8 + PT8, df)) %>%
      mutate(id = "add_pa",
             ethnic = Ethnic[i],
             y = "Linke_vote")
    exp <- exp %>% add_case(tmp)}
}

for(i in 1:length(Ethnic)){
  df <- d %>%
    dplyr::filter(ethnic == Ethnic[i]) %>%
    mutate(pop_vote = ifelse(D6 == "AfD", 1,
                             ifelse(D6 == "Left", 1, 0)),
           Linke_vote = ifelse(D6 == "Left", 1,0))  %>%
    rename(scale = wuttke_pa) 
  
  if(i==1){
    tmp <- tidy(lm(pop_vote ~ scale + D8 + PT8, df)) %>%
      mutate(id = "wuttke_pa",
             ethnic = Ethnic[i],
             y = "pop_vote") 
    exp <- exp %>% add_case(tmp)
    tmp <- tidy(lm(Afd_vote ~ scale + D8 + PT8, df)) %>%
      mutate(id = "wuttke_pa",
             ethnic = Ethnic[i],
             y = "Afd_vote") 
    exp <- exp %>% add_case(tmp)
    tmp <- tidy(lm(Linke_vote ~ scale + D8 + PT8, df)) %>%
      mutate(id = "wuttke_pa",
             ethnic = Ethnic[i],
             y = "Linke_vote")
    exp <- exp %>% add_case(tmp)}
  else{
    tmp <- tidy(lm(pop_vote ~ scale + D8 + PT8, df)) %>%
      mutate(id = "wuttke_pa",
             ethnic = Ethnic[i],
             y = "pop_vote") 
    exp <- exp %>% add_case(tmp)
    tmp <- tidy(lm(Afd_vote ~ scale + D8 + PT8, df)) %>%
      mutate(id = "wuttke_pa",
             ethnic = Ethnic[i],
             y = "Afd_vote") 
    exp <- exp %>% add_case(tmp)
    tmp <- tidy(lm(Linke_vote ~ scale + D8 + PT8, df)) %>%
      mutate(id = "wuttke_pa",
             ethnic = Ethnic[i],
             y = "Linke_vote")
    exp <- exp %>% add_case(tmp)}
}

for(i in 1:length(Ethnic)){
  df <- d %>%
    dplyr::filter(ethnic == Ethnic[i]) %>%
    mutate(pop_vote = ifelse(D6 == "AfD", 1,
                             ifelse(D6 == "Left", 1, 0)),
           Linke_vote = ifelse(D6 == "Left", 1,0))  %>%
  rename(scale = irt_pa) 

if(i==1){
  tmp <- tidy(lm(pop_vote ~ scale + D8 + PT8, df)) %>%
    mutate(id = "irt_pa",
           ethnic = Ethnic[i],
           y = "pop_vote") 
  exp <- exp %>% add_case(tmp)
  tmp <- tidy(lm(Afd_vote ~ scale + D8 + PT8, df)) %>%
    mutate(id = "irt_pa",
           ethnic = Ethnic[i],
           y = "Afd_vote") 
  exp <- exp %>% add_case(tmp)
  tmp <- tidy(lm(Linke_vote ~ scale + D8 + PT8, df)) %>%
    mutate(id = "irt_pa",
           ethnic = Ethnic[i],
           y = "Linke_vote")
  exp <- exp %>% add_case(tmp)}
else{
  tmp <- tidy(lm(pop_vote ~ scale + D8 + PT8, df)) %>%
    mutate(id = "irt_pa",
           ethnic = Ethnic[i],
           y = "pop_vote") 
  exp <- exp %>% add_case(tmp)
  tmp <- tidy(lm(Afd_vote ~ scale + D8 + PT8, df)) %>%
    mutate(id = "irt_pa",
           ethnic = Ethnic[i],
           y = "Afd_vote") 
  exp <- exp %>% add_case(tmp)
  tmp <- tidy(lm(Linke_vote ~ scale + D8 + PT8, df)) %>%
    mutate(id = "irt_pa",
           ethnic = Ethnic[i],
           y = "Linke_vote")
  exp <- exp %>% add_case(tmp)}
}

exp1 <- exp %>%
  dplyr::mutate(lower = estimate - (1.96 * std.error),
         upper = estimate + (1.96 * std.error),
         y = dplyr::recode(y,
                           `Afd_vote` = "Voted: AfD",
                           `Linke_vote` = "Voted: Left",
                           `pop_vote` = "Voted: Populist Party"),
         id = dplyr::recode(id,
                            `add_pa` = "Addative Scale",
                            `cfa_pa` = "CFA Scaling",
                            `irt_pa` = "IRT Scaling",
                            `wuttke_pa` = "Wüttke et al. Approach"),
         ethnic = dplyr::recode(ethnic,
                                `1` = "Ethnic Conception",
                                `0` = "Civic Conception")) %>%
  filter(term == "scale") %>%
  ggplot(aes(x = y, 
             y = estimate,
             color = id,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Regresstion Effects for Populist Attitudes",
       title = "Explanatory Power: Predicting Populist Vote") +
  facet_grid(.~ethnic) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()