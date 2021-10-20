df <- d %>%
  mutate(a = Afd_vote)
h2b <- regression(df, a, ethnic) 

p2b <- h2b %>%
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
