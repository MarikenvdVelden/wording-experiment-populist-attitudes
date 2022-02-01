covs <- d %>%
  select(ethnic, D1:D4, D7:PT8) %>%
  filter(ethnic != "NA")

balanced <-bal.tab(ethnic ~ factor(D1) +  factor(D2) + factor(D3) + 
                     factor(D4) + D7 + D8 + factor(D9) +
                     factor(D10) + pol_know + PT7 + PT8,
                   data = covs,
                   thresholds = c(m = 0.05))[[1]] 
df <- balanced %>%
  mutate(variable = c("Male", "Age: 18 - 29", "Age: 30 - 39", "Age: 40 - 49",
                      "Age: 50 - 59", "Age: 60 - 74", "High Levels of Education",
                      "Low Levels of Education", "Medium Levels of Education",
                      "Urbanness: Big City", "Urbanness: Countryside", "Urbanness: Middlesized City", 
                          "Urbanness: Rural Village", "Urbanness: Suburb",
                          "Employment", "Income", "Living Place: Badem-Würtemberg",
                          "Living Place: Bavaria", "Living Place: Berlin",
                          "Living Place: Brandenburg", "Living Place: Bremen",
                          "Living Place: Hamburg", "Living Place: Hessen",
                          "Living Place: Lower Saxony", "Living Place: Mecklenburg-Western Pomerania",
                          "Living Place: North Rhine-Westphalia", "Living Place: Rhineland-Palatinate",
                          "Living Place: Saarland", "Living Place: Saxony", "Living Place: Saxony-Anholt",
                          "Living Place: Schleswig-Holstein", "Living Place: Thuringia",
                          "Birth Place: Badem-Würtemberg",
                          "Birth Place: Bavaria", "Birth Place: Berlin",
                          "Birth Place: Brandenburg", "Birth Place: Bremen",
                          "Birth Place: Hamburg", "Birth Place: Hessen",
                          "Birth Place: Lower Saxony", "Birth Place: Mecklenburg-Western Pomerania",
                          "Birth Place: North Rhine-Westphalia", "Birth Place: Not born in Germany", 
                      "Birth Place: Rhinelad-Palatinate", 
                          "Birth Place: Saarland", "Birth Place: Saxony", "Birth Place: Saxony-Anholt",
                          "Birth Place: Schleswig-Holstein", "Birth Place: Thuringia",
                          "Political Knowledge", "Political Interest",
                          "Ideology"),
         variable = factor(variable,
                           levels = c("Male", "Age: 18 - 29", "Age: 30 - 39", "Age: 40 - 49",
                                      "Age: 50 - 59", "Age: 60 - 74", "High Levels of Education",
                                      "Low Levels of Education", "Medium Levels of Education",
                                      "Urbanness: Big City", "Urbanness: Countryside", "Urbanness: Middlesized City", 
                                      "Urbanness: Rural Village", "Urbanness: Suburb",
                                      "Employment", "Income", "Living Place: Badem-Würtemberg",
                                      "Living Place: Bavaria", "Living Place: Berlin",
                                      "Living Place: Brandenburg", "Living Place: Bremen",
                                      "Living Place: Hamburg", "Living Place: Hessen",
                                      "Living Place: Lower Saxony", "Living Place: Mecklenburg-Western Pomerania",
                                      "Living Place: North Rhine-Westphalia", "Living Place: Rhineland-Palatinate",
                                      "Living Place: Saarland", "Living Place: Saxony-Anholt", "Living Place: Saxony",
                                      "Living Place: Schleswig-Holstein", "Living Place: Thuringia",
                                      "Birth Place: Badem-Würtemberg",
                                      "Birth Place: Bavaria", "Birth Place: Berlin",
                                      "Birth Place: Brandenburg", "Birth Place: Bremen",
                                      "Birth Place: Hamburg", "Birth Place: Hessen",
                                      "Birth Place: Lower Saxony", "Birth Place: Mecklenburg-Western Pomerania",
                                      "Birth Place: North Rhine-Westphalia", "Birth Place: Rhinelad-Palatinate",
                                      "Birth Place: Saarland", "Birth Place: Saxony", "Birth Place: Saxony-Anholt",
                                      "Birth Place: Schleswig-Holstein", "Birth Place: Thuringia",
                                      "Birth Place: Not born in Germany",
                                      "Political Knowledge", "Political Interest",
                                      "Ideology")),
         difference = Diff.Un,2) %>%
  select(variable, difference) %>%
  mutate(type = if_else(difference <= -.05, "below",
                        if_else(difference >= .05, "below", "above"))) %>%
  ggplot(aes(x = variable, y = difference, color = type)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("above"=fig_cols[3],
                                "below"=fig_cols[2])) +
  
  theme_bw() +
  labs(x="", y= "Standardized Mean Differences") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  geom_hline(yintercept = -0.05, linetype = "dashed") +
  coord_flip()

rm(covs, balanced)
