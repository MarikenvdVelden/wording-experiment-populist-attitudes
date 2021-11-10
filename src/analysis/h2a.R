df <- d %>%
  mutate(a = HT4)
h2a <- regression2(df, a, ethnic) 
  
p2a <- h2a %>%
  mutate(y = recode(y,
                    `POST_2` = "DV: Item 2",
                    `POST_3` = "DV: Item 3",
                    `POST_4` = "DV: Item 4")) %>%
  ggplot(aes(x = a, 
             y = AME,
             color = y,
             fill = y,
             ymin = lower,
             ymax = upper)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  #geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_minimal() +
  labs(x = "Exclusive National Identity \n (1 = Low, 5 = High)", 
       y = "Average Marginal Effects of Levels of Ethnic Conception of the People",
       title = "National Identity Hypothesis") +
  facet_grid(.~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")
