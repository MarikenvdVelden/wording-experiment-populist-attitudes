pa_model <- 'pa_scale = ~ POST_1 + POST_2 + POST_3 + POST_5 + POST_6
  POST_3 ~~ POST_2
  POST_2 ~~ POST_6
'

de <- d %>%
  filter(ethnic == 1)
cfa_ethnic <- cfa(model = pa_model, data = de)
#summary(cfa_ethnic, fit.measures = TRUE, standardized = TRUE)[[1]]

dc <- d %>%
  filter(ethnic == 0)
cfa_civic <- cfa(model = pa_model, data = dc)
#summary(cfa_civic, fit.measures = TRUE, standardized = TRUE)

pa_model <- 'pa_scale = ~ POST_1 + POST_2 + POST_3 + POST_4 + POST_5 + POST_6
  POST_3 ~~ POST_2
  POST_2 ~~ POST_6
'
dc <- d %>%
  filter(ethnic == 0)
cfa_civic6 <- cfa(model = pa_model, data = dc)

m_cfa <- tibble(
  `Fit Statistics` = c("CFI", "AIC", "BIC", "RMSEA", "p-value RMSEA", "SRMR"),
  `Ethnic Conception` = c(fitMeasures(cfa_ethnic)[9], AIC(cfa_ethnic), BIC(cfa_ethnic),
                          fitMeasures(cfa_ethnic)[23], fitMeasures(cfa_ethnic)[26],
                          fitMeasures(cfa_ethnic)[29]),
  `Civic Conception` = c(fitMeasures(cfa_civic)[9], AIC(cfa_civic), BIC(cfa_civic),
                         fitMeasures(cfa_civic)[23], fitMeasures(cfa_civic)[26],
                         fitMeasures(cfa_civic)[29]),
  `Civic Conception (6 Items)` = c(fitMeasures(cfa_civic6)[9], AIC(cfa_civic6), BIC(cfa_civic6),
                         fitMeasures(cfa_civic6)[23], fitMeasures(cfa_civic6)[26],
                         fitMeasures(cfa_civic6)[29]))

cfa_e <- d %>%
  filter(ethnic == 1) %>%
  mutate(cfa_pa = as.numeric(predict(cfa_ethnic))) %>%
  select(id, cfa_pa)

cfa_df <- d %>%
  filter(ethnic == 0) %>%
  mutate(cfa_pa = as.numeric(predict(cfa_civic))) %>%
  select(id, cfa_pa) %>%
  add_case(cfa_e)
             
cfa1 <- d %>%
  left_join(cfa_df, by = "id") %>%
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


cfa2 <- d %>%
  left_join(cfa_df, by = "id")  %>%
  mutate(ethnic = recode(ethnic,
                         `0` = "Civic Conception",
                         `1` = "Ethnic Conception"))  %>%
  ggplot(aes(x = cfa_pa, color = ethnic, fill = ethnic)) +
  labs(y= "Density", x = "Distribution CFA Construct") +
  geom_histogram(binwidth=.5) +
  facet_grid(.~ethnic) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme_ipsum()  +
  theme(legend.position="none",
        legend.title = element_blank())

d <- full_join(x = d, y = cfa_df, by = "id") %>%
  distinct()
  
