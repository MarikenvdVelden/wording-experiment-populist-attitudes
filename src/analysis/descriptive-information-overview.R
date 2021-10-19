descr <- d %>%
  select(D7:D8, pol_know:POST_4, -POST_1) %>%
  pivot_longer(cols = D7:POST_4,
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
                            `POST_2`  = "DV: Item 2",
                            `POST_3`  = "DV: Item 3",
                            `POST_4`  = "DV: Item 4",
                            `HT4`  = "National Identity",
                            `Afd_vote` = "Far Right Party ID",
                            `ethnic` = "Treatment: Ethnic Conception"),
         Variables = factor(Variables,
                            levels = c("DV: Item 2","DV: Item 3", "DV: Item 4",
                                       "Treatment: Ethnic Conception",
                                       "National Identity",
                                       "Far Right Party ID",
                                       "Ideology", "Political Knowledge",
                                       "Political Interest",
                                       "Employment","Income")))

