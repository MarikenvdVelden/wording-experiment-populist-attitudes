df <- d %>%
  mutate(a = HT4)
h2a <- regression2(df, a, ethnic) 
  
p2a <- h2a %>%
  mutate(y = recode(y,
                    `POST_2` = "DV: People, not politicians, should make the most \n important political decisions",
                    `POST_3` = "DV: MPs should follow the will of the \n people",
                    `POST_4` = "DV: Differences between elites an the people are bigger than \n differences between the people")) %>%
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
  labs(x = "Exclusive National Identity \n (1 = Low, 5 = High)", 
       y = "Average Marginal Effects for\n Ethnic Conception of the People") +
  facet_grid(.~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")
