# Alternative specification
df <- d 
exp1 <- tidy(lm(POST_2 ~ ethnic + PT7 + D8, df))
exp1 <- exp1 %>%
  add_case(
    tidy(lm(POST_3 ~ ethnic + PT7 + D8, df))
  ) %>%
  filter(term == "ethnic") %>%
  mutate(y = c("POST_2", "POST_3"),
         lower = estimate - (1.65 * std.error),
         upper = estimate + (1.65 * std.error)) %>%
  select(estimate, upper, lower, y)

pe1 <- exp1 %>%
  mutate(y = recode(y,
                    `POST_2` = "DV: People, not politicians, \n should make the most \n important political decisions",
                    `POST_3` = "DV: MPs should follow \n the will of the people")) %>%
  ggplot(aes(x = y, 
             y = estimate,
             color = y,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  theme(legend.position="none") +
  labs(x = "", y = "Effect of Ethnic Conception of the People",
       title = "Alternative Specification: Ethnic Conception hypothesis") +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

# Interaction with Age (pe2)
df <- d %>%
  mutate(a = D2)
exp2 <- regression5(df, a, ethnic) 

pe2 <- exp2 %>%
  mutate(y = recode(y,
                    `POST_2` = "DV: People, not politicians, \n should make the most \n important political decisions",
                    `POST_3` = "DV: MPs should follow \n the will of the people",
                    `POST_4` = "DV: Differences between elites and \n the people are bigger than \n differences between the people"),
         factor = recode(factor,
                         `a30-39` = "Age: 30 - 39",
                         `a40-49` = "Age: 40 - 49",
                         `a50-59` = "Age: 50 - 59", 
                         `a60-74` = "Age: 60 - 74")) %>%
  ggplot(aes(x = factor, 
             y = AME,
             color = y,
             fill = y,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", 
       y = "Average Marginal Effects for Ethnic Conception of the People",
       title = "") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="right",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

#Interaction with Region
df <- d %>%
  mutate(a = D9,
         a = recode(a,
                    `Baden-Würtemberg` = "West Germany",
                    `Bavaria` = "West Germany (Bavaria)",
                    `Berlin` = "Berlin",
                    `Brandenburg` = "Former East Germany",
                    `Bremen` = "West Germany",
                    `Hamburg` = "West Germany",
                    `Hessen` = "West Germany",
                    `Lower Saxony` = "West Germany",
                    `Mecklenburg-Western Pomerania` = "Former East Germany",
                    `North Rhine-Westphalia` = "West Germany",
                    `Rhineland-Palatinate` = "West Germany",
                    `Saarland` = "West Germany",
                    `Saxony` = "Former East Germany",
                    `Saxony-Anholt` = "Former East Germany",
                    `Schleswig-Holstein` = "West Germany",
                    `Thuringia` = "Former East Germany"))
exp3 <- regression5(df, a, ethnic) 

pe3 <- exp3 %>%
  mutate(y = recode(y,
                    `POST_2` = "DV: People, not politicians, \n should make the most \n important political decisions",
                    `POST_3` = "DV: MPs should follow \n the will of the people",
                    `POST_4` = "DV: Differences between elites and \n the people are bigger than \n differences between the people"),
         factor = recode(factor,
                         `aFormer East Germany` = "Former East Germany",
                         `aWest Germany` = "West Germany",
                         `aWest Germany (Bavaria)` = "West Germany (Bavaria)")) %>%
  ggplot(aes(x = factor, 
             y = AME,
             color = y,
             fill = y,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", 
       y = "Average Marginal Effects for Ethnic Conception of the People",
       title = "Exploration: Region") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

#Interaction with Ideology
df <- d %>%
  mutate(a = PT8)
exp4 <- regression3(df, a, ethnic) 

pe4 <- exp4 %>%
  mutate(y = recode(y,
                    `POST_2` = "DV: People, not politicians, \n should make the most \n important political decisions",
                    `POST_3` = "DV: MPs should follow \n the will of the people",
                    `POST_4` = "DV: Differences between elites and \n the people are bigger than \n differences between the people")) %>%
  ggplot(aes(x = a, 
             y = AME,
             color = y,
             fill = y,
             ymin = lower,
             ymax = upper)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  #geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "Ideological Left-Right Placement \n (0 = Left, 10 = Right)", 
       y = "Average Marginal Effects for Ethnic Conception of the People",
       title = "Exploration: Ideology") +
  facet_grid(.~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")

#Interaction with POST_8
df <- d %>%
  mutate(a = POST_8)
exp5 <- regression6(df, a, ethnic) 

pe5 <- exp5 %>%
  mutate(y = recode(y,
                    `POST_2` = "DV: People, not politicians, \n should make the most \n important political decisions",
                    `POST_3` = "DV: MPs should follow \n the will of the people")) %>%
  ggplot(aes(x = a, 
             y = AME,
             color = y,
             fill = y,
             ymin = lower,
             ymax = upper)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  #geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "Attitudes towards Political Pluralism \n (0 = Negative, 5 = Positive)", 
       y = "Average Marginal Effects for Ethnic Conception of the People",
       title = "Exploration: Attitudes towards Political Pluralism") +
  facet_grid(.~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")
