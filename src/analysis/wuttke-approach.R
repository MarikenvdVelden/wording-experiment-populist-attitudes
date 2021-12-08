w_e <- d %>%
  dplyr::filter(ethnic == 1) %>%
  dplyr::select(id, matches("POST_[12356]")) %>%
  rowwise() %>% 
  dplyr::mutate(wuttke_pa = min(POST_1, POST_2, POST_3,
                     POST_5, POST_6)) %>%
  dplyr::select(id, wuttke_pa)


w_df <- d %>%
  dplyr::filter(ethnic == 0) %>%
  dplyr::select(id, matches("POST_[12356]")) %>%
  rowwise() %>% 
  dplyr::mutate(wuttke_pa = min(POST_1, POST_2, POST_3,
                     POST_5, POST_6)) %>%
  dplyr::select(id, wuttke_pa) %>%
  add_case(w_e)

wa1 <- d %>%
  left_join(w_df) %>%
  dplyr::select(wuttke_pa, ethnic) %>%
  dplyr::mutate(ethnic = recode(ethnic, `1` = "Ethnic Conception",  `0` = "Civic Conception")) %>%
  group_by(ethnic) %>%
  summarise(wuttke_pa = list(wuttke_pa)) %>%
  spread(ethnic, wuttke_pa) %>%
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


wa2 <- d %>%
  left_join(w_df) %>%
  dplyr::mutate(ethnic = recode(ethnic,
                         `0` = "Civic Conception",
                         `1` = "Ethnic Conception")) %>%
  ggplot(aes(x = wuttke_pa, color = ethnic, fill = ethnic)) +
  labs(y= "Density", x = "Distribution CFA Construct") +
  geom_histogram(binwidth=1) +
  facet_grid(.~ethnic) +
  theme_ipsum()  +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position="none",
        legend.title = element_blank())

d <- inner_join(x = d, y = w_df, by = "id") %>%
  distinct()

