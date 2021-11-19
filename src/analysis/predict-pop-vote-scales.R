Ethnic <- c(0,1) 

for(i in 1:length(Ethnic)){
df <- d %>%
    dplyr::filter(ethnic == Ethnic[i]) %>%
    mutate(pop_vote = ifelse(D6 == "AfD", 1,
                             ifelse(D6 == "Left", 1, 0)),
           Linke_vote = ifelse(D6 == "Left", 1,0),
           scale = cfa_pa)
if(i==1){
  exp <- regression7(df, scale) %>%
    mutate(id = "cfa_pa",
           ethnic = Ethnic[i])}
else{
  tmp <- regression7(df, scale) %>%
    mutate(id = "cfa_pa",
           ethnic = Ethnic[i])
  exp <- exp %>%
    add_case(tmp)}
}

for(i in 1:length(Ethnic)){
  df <- d %>%
    dplyr::filter(ethnic == Ethnic[i]) %>%
    mutate(pop_vote = ifelse(D6 == "AfD", 1,
                             ifelse(D6 == "Left", 1, 0)),
           Linke_vote = ifelse(D6 == "Left", 1,0),
           scale = add_pa)
  if(i==1){
    tmp <- regression7(df, scale) %>%
      mutate(id = "add_pa",
             ethnic = Ethnic[i])}
  else{
    tmpp <- regression7(df, scale) %>%
      mutate(id = "add_pa",
             ethnic = Ethnic[i])
    tmp <- tmp %>%
      add_case(tmpp)}
}
exp <- exp %>%
  add_case(tmp)

for(i in 1:length(Ethnic)){
  df <- d %>%
    dplyr::filter(ethnic == Ethnic[i]) %>%
    mutate(pop_vote = ifelse(D6 == "AfD", 1,
                             ifelse(D6 == "Left", 1, 0)),
           Linke_vote = ifelse(D6 == "Left", 1,0),
           scale = wuttke_pa)
  if(i==1){
    tmp <- regression7(df, scale) %>%
      mutate(id = "wuttke_pa",
             ethnic = Ethnic[i])}
  else{
    tmpp <- regression7(df, scale) %>%
      mutate(id = "wuttke_pa",
             ethnic = Ethnic[i])
    tmp <- tmp %>%
      add_case(tmpp)}
}
exp <- exp %>%
  add_case(tmp)

for(i in 1:length(Ethnic)){
  df <- d %>%
    dplyr::filter(ethnic == Ethnic[i]) %>%
    mutate(pop_vote = ifelse(D6 == "AfD", 1,
                             ifelse(D6 == "Left", 1, 0)),
           Linke_vote = ifelse(D6 == "Left", 1,0),
           scale = irt_pa)
  if(i==1){
    tmp <- regression7(df, scale) %>%
      mutate(id = "irt_pa",
             ethnic = Ethnic[i])}
  else{
    tmpp <- regression7(df, scale) %>%
      mutate(id = "irt_pa",
             ethnic = Ethnic[i])
    tmp <- tmp %>%
      add_case(tmpp)}
}

exp <- exp %>%
  add_case(tmp) %>%
  dplyr::mutate(y = dplyr::recode(y,
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
