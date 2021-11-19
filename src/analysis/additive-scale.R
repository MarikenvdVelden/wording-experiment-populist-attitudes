as_e <- d %>%
  filter(ethnic == 1) %>%
  mutate(scale = round((POST_1 + POST_2 + POST_3 + 
                          POST_5 + POST_6)/5,0)) 
as_e <- as_e$scale
  
as_c <- d %>%
  filter(ethnic == 0) %>%
  mutate(scale = round((POST_1 + POST_2 + POST_3 + 
                          POST_5 + POST_6)/5,0)) 
as_c <- as_c$scale

add1 <- d %>%
  mutate(scale = ifelse(ethnic == 1, as_e, as_c)) %>%
  select(scale, ethnic) %>%
  mutate(ethnic = recode(ethnic, `1` = "Ethnic Conception",  `0` = "Civic Conception")) %>%
  group_by(ethnic) %>%
  summarise(scale = list(scale)) %>%
  spread(ethnic, scale) %>%
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
  mutate(scale = ifelse(ethnic == 1, as_e, as_c),
         ethnic = recode(ethnic,
                         `0` = "Civic Conception",
                         `1` = "Ethnic Conception")) %>%
  ggplot(aes(x = scale, color = ethnic, fill = ethnic)) +
  labs(y= "Density", x = "Distribution CFA Construct") +
  geom_histogram(binwidth=1) +
  facet_grid(.~ethnic) +
  theme_ipsum()  +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position="none",
        legend.title = element_blank())

tmp <- d %>%
  mutate(add_pa = ifelse(ethnic == 1,as_e, as_c)) %>%
  select(add_pa)

d <- d %>%
  add_column(tmp)
