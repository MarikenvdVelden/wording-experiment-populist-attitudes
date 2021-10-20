df <- d %>%
  mutate(a = HT4)
h2a <- regression(df, a, ethnic) 
  
p2a <- h2a %>%
  mutate(y = recode(y,
                    `POST_2` = "DV: Item 2",
                    `POST_3` = "DV: Item 3",
                    `POST_4` = "DV: Item 4"),
         ethnic = recode(ethnic,
                             `1` = "Ethnic Conception",
                             `0` = "Civic Conception")) %>%
  ggplot(aes(x = y, 
             y = AME,
             color = y,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_minimal() +
  labs(x = "", y = "Average Marginal Effects of Levels of Exclusive National Identity",
       title = "National Identity Hypothesis") +
  facet_grid(.~ethnic) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()
