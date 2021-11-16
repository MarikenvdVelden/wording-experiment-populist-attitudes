# Alternative specification
df <- d 
exp1 <- regression4(df, ethnic) 

pe1 <- exp1 %>%
  mutate(y = recode(y,
                    `POST_2` = "DV: People, not politicians, \n should make the most \n important political decisions",
                    `POST_3` = "DV: MPs should follow \n the will of the people",
                    `POST_4` = "DV: Differences between elites and \n the people are bigger than \n differences between the people")) %>%
  ggplot(aes(x = y, 
             y = estimate,
             color = y,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Effect of Ethnic Conception of the People",
       title = "Alternative Specification: Ethnic Conception hypothesis") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

# Interaction with Age (pe2)


#Interaction with Region
df <- d %>%
  mutate(a = D9)
exp3 <- regression5(df, a, ethnic) 

pe3 <- exp3 %>%
  mutate(y = recode(y,
                    `POST_2` = "DV: People, not politicians, \n should make the most \n important political decisions",
                    `POST_3` = "DV: MPs should follow \n the will of the people",
                    `POST_4` = "DV: Differences between elites and \n the people are bigger than \n differences between the people")) %>%
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
