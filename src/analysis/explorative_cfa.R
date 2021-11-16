pa_model <- 'pa_scale = ~ POST_1 + POST_2 + POST_3 + POST_4 + POST_5 + POST_6
  POST_3 ~~ POST_2
  POST_2 ~~ POST_6
'

de <- d %>%
  filter(ethnic == 1)
cfa_ethnic <- cfa(model = pa_model, data = de)
#summary(cfa_ethnic, fit.measures = TRUE, standardized = TRUE)

dc <- d %>%
  filter(ethnic == 0)
cfa_civic <- cfa(model = pa_model, data = dc)
#summary(cfa_civic, fit.measures = TRUE, standardized = TRUE)

cfa1 <- d %>%
  mutate(cfa_pa = ifelse(ethnic == 1, predict(cfa_ethnic), predict(cfa_civic))) %>%
  select(cfa_pa, ethnic) %>%
  mutate(ethnic = recode(ethnic, `1` = "Ethnic Conception",  `0` = "Civic Conception")) %>%
  group_by(ethnic) %>%
  summarise(cfa_pa = list(cfa_pa)) %>%
  spread(ethnic, cfa_pa) %>%
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


         
