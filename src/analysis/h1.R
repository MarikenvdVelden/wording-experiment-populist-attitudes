df <- d %>% 
  mutate(ethnic = recode(ethnic, `1` = "Ethnic Conception",  `0` = "Civic Conception")) %>%
  select(POST_2, POST_3, ethnic) %>%
  pivot_longer(cols = POST_2:POST_3) %>%
  group_by(ethnic, name) %>%
  summarise(value = list(value)) %>%
  spread(ethnic, value) %>%
  group_by(name) %>%
  mutate(p_value = t.test(unlist(`Civic Conception`), unlist(`Ethnic Conception`))$p.value,
         t_value = t.test(unlist(`Civic Conception`), unlist(`Ethnic Conception`))$statistic,
         means_civic = t.test(unlist(`Civic Conception`))$estimate,
         means_ethnic = t.test(unlist(`Ethnic Conception`))$estimate,
         means_difference = means_civic - means_ethnic,
         lower = t.test(unlist(`Civic Conception`), unlist(`Ethnic Conception`))$conf.int[1],
         higher = t.test(unlist(`Civic Conception`), unlist(`Ethnic Conception`))$conf.int[2],
         name = recode(name,
                       `POST_2` = "DV: People, not politicians, \n should make the most \n important political decisions",
                       `POST_3` = "DV: MPs should follow \n the will of the people")) %>%
  select(name, p_value, t_value, means_difference, lower, higher) 

p1 <- df %>%
  ggplot(aes(x = means_difference, 
             y = name,
             xmin = lower, 
             xmax = higher,
             color = name)) +
  geom_point() + 
  geom_errorbar(width = 0) +
  theme_ipsum() +
  labs(y="", x= "Means Differences Test \n t statistic") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none") +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0.0, linetype = "dashed", size = .5, color = "gray80")
  
  
