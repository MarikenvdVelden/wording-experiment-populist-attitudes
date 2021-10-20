pa_model <- 'pa_scale = ~ POST_1 + POST_2 + POST_3 + POST_4 + POST_5 + POST_6'

de <- d %>%
  filter(ethnic == 1)
cfa_ethnic <- cfa(model = pa_model, data = de)
#summary(cfa_ethnic, fit.measures = TRUE, standardized = TRUE)

dc <- d %>%
  filter(ethnic == 0)
cfa_civic <- cfa(model = pa_model, data = dc)
#summary(cfa_civic, fit.measures = TRUE, standardized = TRUE)

d <- d %>%
  mutate(cfa_pa = ifelse(ethnic == 1, predict(cfa_ethnic), predict(cfa_civic)))

