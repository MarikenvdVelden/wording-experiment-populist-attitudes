d <- d %>%
  mutate(D1 = recode(D1, 
                     `1` = "Female",
                     `2` = "Male", .default = "NA"),
         D2 = recode(D2, 
                     `1` = "17 or younger",
                     `2` = "18-29",
                     `3` = "30-39",
                     `4` = "40-49",
                     `5` = "50-59",
                     `6` = "60-74", .default = "NA"),
         D3 = recode(D3, 
                     `1` = "Low Levels of Education",
                     `2` = "Medium Levels of Education",  
                     `3` =  "High Levels of Education",.default = "NA"),
         D4 = recode(D4, 
                     `1` = "Big City",
                     `2` = "Suburb",
                     `3` = "Middlesized City",
                     `4` = "Rural Village",
                     `5` = "Countryside", .default = "NA"),
         D6 = recode(D6,
                     `1` = "CDU/CSU",
                     `2` = "SPD",
                     `3` = "AfD",
                     `4` = "Left",
                     `5` = "Greens",
                     `6` = "Other party", .default = "NA"),
        Afd_vote = if_else(D6 == "AfD", 1, 0),
        D7 = na_if(D7, 12),
        D8 = na_if(D8, 14),
        D9 = recode(D9,
                    `1` = "Baden-Würtemberg",
                    `17` = "Bavaria",
                    `18` = "Berlin",
                    `19` = "Brandenburg",
                    `20` = "Bremen",
                    `21` = "Hamburg",
                    `22` = "Hessen",
                    `23` = "Lower Saxony",
                    `24` = "Mecklenburg-Western Pomerania",
                    `25` = "North Rhine-Westphalia",
                    `26` = "Rhineland-Palatinate",
                    `27` = "Saarland",
                    `28` = "Saxony",
                    `29` = "Saxony-Anholt",
                    `30` = "Schleswig-Holstein",
                    `31` = "Thuringia",
                    .default = "NA"),
        D10 = recode(D10,
                    `1` = "Baden-Würtemberg",
                    `17` = "Bavaria",
                    `18` = "Berlin",
                    `19` = "Brandenburg",
                    `20` = "Bremen",
                    `21` = "Hamburg",
                    `22` = "Hessen",
                    `23` = "Lower Saxony",
                    `24` = "Mecklenburg-Western Pomerania",
                    `25` = "North Rhine-Westphalia",
                    `26` = "Rhineland-Palatinate",
                    `27` = "Saarland",
                    `28` = "Saxony",
                    `29` = "Saxony-Anholt",
                    `30` = "Schleswig-Holstein",
                    `31` = "Thuringia",
                    `32` = "Not born in Germany", .default = "NA"),
        HT4 = round((HT4_1 + HT4_2 + HT4_3 + HT4_4)/4, 0),
        HT4 = 6 - HT4,
        PT4 = if_else(PT4 == 3, 1, 0),
        PT5 = if_else(PT5 == 5, 1, 0),
        PT6 = if_else(PT6==1, 1, 0),
        pol_know = PT4 + PT5 + PT6,
        PT7 =  na_if(PT7, 7),
        ethnic = if_else(populism=="Volk", 1, 0),
        POST_1 = as_numeric(POST_1),
        POST_1 = POST_1 - 1,
        POST_2 = as_numeric(POST_2),
        POST_2 = POST_2 - 1,
        POST_3 = as_numeric(POST_3),
        POST_3 = POST_3 - 1,
        POST_4 = as_numeric(POST_4),
        POST_4 = POST_4 - 1,
        POST_5 = as_numeric(POST_5),
        POST_5 = POST_5 - 1,
        POST_6 = as_numeric(POST_6),
        POST_6 = POST_6 - 1,
        POST_8 = as_numeric(POST_8),
        POST_8 = POST_8 - 1,) %>%
  select(id, D1, D2, D3, D4:D10, pol_know, PT7:PT8, HT4, Afd_vote, ethnic, POST_1:POST_8) %>%
  mutate(D1 = if_else(D1 == "NA", median(D1, na.rm = T), D1),
         D3 = if_else(D3 == "NA", median(D3, na.rm = T), D3),
         D4 = if_else(D4 == "NA", median(D4, na.rm = T), D4),
         D7 = if_else(is.na(D7), round(mean(D7, na.rm = T),0), D7),
         D8 = if_else(is.na(D8), round(mean(D8, na.rm = T),0), D8),
         D9 = if_else(D9 == "NA", median(D9, na.rm = T), D9),
         D10 = if_else(D10 == "NA", median(D10, na.rm = T), D10)) %>%
  drop_na(ethnic)