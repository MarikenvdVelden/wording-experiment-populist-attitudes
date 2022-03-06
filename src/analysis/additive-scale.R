as_e <- d %>%
  filter(ethnic == 1) %>%
  mutate(add_pa = (POST_1 + POST_2 + POST_3 + 
                          POST_5 + POST_6)/5) %>%
  select(id, add_pa)


as_df <- d %>%
  filter(ethnic == 0) %>%
  mutate(add_pa = (POST_1 + POST_2 + POST_3 + 
                          POST_5 + POST_6)/5) %>%
  select(id, add_pa) %>% 
  add_case(as_e)
  

add1 <- d %>%
  left_join(as_df) %>%
  select(add_pa, ethnic) %>%
  mutate(ethnic = recode(ethnic, `1` = "Ethnic Conception",  `0` = "Civic Conception")) %>%
  group_by(ethnic) %>%
  summarise(add_pa = list(add_pa)) %>%
  spread(ethnic, add_pa) %>%
  mutate(p_value = t.test(unlist(`Civic Conception`), unlist(`Ethnic Conception`))$p.value,
         t_value = t.test(unlist(`Civic Conception`), unlist(`Ethnic Conception`))$statistic,
         means_civic = t.test(unlist(`Civic Conception`))$estimate,
         means_ethnic = t.test(unlist(`Ethnic Conception`))$estimate,
         means_difference = means_civic - means_ethnic,
         lower = t.test(unlist(`Civic Conception`), unlist(`Ethnic Conception`))$conf.int[1],
         higher = t.test(unlist(`Civic Conception`), unlist(`Ethnic Conception`))$conf.int[2],
         name = "Differences in Means") %>%
  select(means_civic, means_ethnic, means_difference, p_value) %>%
  pivot_longer(cols = means_civic:p_value,
               values_to = "Values",
               names_to = "Variables") %>%
  mutate(Variables = recode(Variables,
                            `means_civic` = "Average of PCA Constructed Scale for Civic Conception of the People",
                            `means_ethnic` = "Average of PCA Constructed Scale for Ethnic Conception of the People",
                            `means_difference` = "Average of Civic Conception - Average of Ethnic Conception",
                            `p_value` = "p-Value for Differences in Means"))


add2 <- d %>%
  left_join(as_df) %>%
  mutate(ethnic = recode(ethnic,
                         `0` = "Civic Conception",
                         `1` = "Ethnic Conception")) %>%
  ggplot(aes(x = add_pa, color = ethnic, fill = ethnic)) +
  labs(y= "Density", x = "Distribution CFA Construct") +
  geom_histogram(binwidth=1) +
  facet_grid(.~ethnic) +
  theme_ipsum()  +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position="none",
        legend.title = element_blank())

d <- left_join(x = d, y = as_df, by = "id") %>%
  distinct()
