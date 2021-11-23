descr <- d %>%
  select(D7:D8, pol_know:POST_6, POST_8, -POST_1) %>%
  pivot_longer(cols = D7:POST_8,
               names_to = "Variables") %>%
  group_by(Variables) %>%
  summarise(`Mean Value` = mean(value, na.rm = T),
            `St. Dev` = sd(value, na.rm = T),
            `Min. Value` = min(value, na.rm = T),
            `Max. Value` = max(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Variables = recode(Variables,
                            `D7` = "Employment",
                            `D8` = "Income",
                            `pol_know`  = "Political Knowledge",
                            `PT7`  = "Political Interest",
                            `PT8`  = "Ideology",
                            `POST_1` = "Rep. by ordinary citizen",
                            `POST_2`  = "DV: People, not politicians, \n should make the most \n important political decisions",
                            `POST_3` = "DV: MPs should follow \n the will of the people",
                            `POST_4` = "Differences between elites and \n the people are bigger than \n differences between the people",
                            `POST_5` = "Too much talk, no action",
                            `POST_6` = "Compromise is selling out",
                            `POST_8` = "Attitude towards Pluralism",
                            `HT4`  = "National Identity",
                            `Afd_vote` = "Far Right Party ID",
                            `ethnic` = "Treatment: Ethnic Conception"),
         Variables = factor(Variables,
                            levels = c("DV: People, not politicians, \n should make the most \n important political decisions",
                                       "DV: MPs should follow \n the will of the people",
                                       "Rep. by ordinary citizen",
                                       "Differences between elites and \n the people are bigger than \n differences between the people",
                                       "Too much talk, no action","Compromise is selling out",
                                       "Attitude towards Pluralism",
                                       "Treatment: Ethnic Conception",
                                       "National Identity",
                                       "Far Right Party ID",
                                       "Ideology", "Political Knowledge",
                                       "Political Interest",
                                       "Employment","Income")))

