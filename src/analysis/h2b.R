df <- d %>%
  mutate(a = Afd_vote)
h2b <- regression(df, a, ethnic) 

p2b <- h2b %>%
  mutate(y = recode(y,
                    `POST_2` = "DV: People, not politicians, \n should make the most \n important political decisions",
                    `POST_3` = "DV: MPs should follow \n the will of the people",
                    `POST_4` = "DV: Differences between elites and \n the people are bigger than \n differences between the people"),
         ethnic = recode(ethnic,
                         `1` = "Ethnic Conception",
                         `0` = "Civic Conception")) %>%
  filter(ethnic == "Ethnic Conception") %>%
  ggplot(aes(x = y, 
             y = AME,
             color = y,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Average Marginal Effects of Voting for a Far Right Party",
       title = "Far Right Party ID Hypothesis") +
  facet_grid(.~ethnic) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

p2c <- h2b %>%
  mutate(y = recode(y,
                    `POST_2` = "DV: People, not politicians, \n should make the most \n important political decisions",
                    `POST_3` = "DV: MPs should follow \n the will of the people",
                    `POST_4` = "DV: Differences between elites and \n the people are bigger than \n differences between the people"),
         ethnic = recode(ethnic,
                         `1` = "Ethnic Conception",
                         `0` = "Civic Conception")) %>%
  filter(ethnic == "Ethnic Conception") %>%
  ggplot(aes(x = y, 
             y = AME,
             color = y,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_minimal() +
  labs(x = "", y = "Average Marginal Effects of \n Voting for a Far Right Party",
       title = "Far Right Party ID Hypothesis") +
  facet_grid(.~ethnic) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()
