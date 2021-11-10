# Tidy Qualtrics Data

# Clean demographics
dem <- d %>%
  filter(Consent == 4, AC2_1 ==1, AC2_4 == 1,
         tic != "359766d9041e7d8c345c0c2838781095", 
         tic != "daabba41247f457b637205e066bbe39b",
         tic != "f6b4160f5ab5dee05b7e925d50e5414") %>%
  select(matches("D\\d"), id = tic) %>%
  select(-matches("D6_7_TEXT")) %>%
  select(id, D4:D10)%>%
  drop_na()

pret <- d %>%
  filter(Consent == 4, AC2_1 ==1, AC2_4 == 1,
         tic != "359766d9041e7d8c345c0c2838781095", 
         tic != "daabba41247f457b637205e066bbe39b",
         tic != "f6b4160f5ab5dee05b7e925d50e5414") %>%
  select(matches("PT\\d"), matches("HT4_\\d+"), id = tic) %>%
  select(-matches("PT8_NPS_GROUP"), -matches("PT[13]"), -matches("PT[13]_\\d+")) %>%
  select(id, PT4, PT5 = PT5_1, PT6:HT4_4)%>%
  drop_na()

treat <- d %>%
  filter(Consent == 4, AC2_1 ==1, AC2_4 == 1,
         tic != "359766d9041e7d8c345c0c2838781095", 
         tic != "daabba41247f457b637205e066bbe39b",
         tic != "f6b4160f5ab5dee05b7e925d50e5414") %>%
  select(id = tic, populism) %>%
  drop_na()

post <- d %>%
  filter(Consent == 4, AC2_1 ==1, AC2_4 == 1,
         tic != "359766d9041e7d8c345c0c2838781095", 
         tic != "daabba41247f457b637205e066bbe39b",
         tic != "f6b4160f5ab5dee05b7e925d50e5414") %>%
  select(id = tic, matches("POST1[ab]_[123456]")) %>%
  unite(POST_1, matches("POST1[ab]_1"), na.rm = TRUE) %>%
  unite(POST_2, matches("POST1[ab]_2"), na.rm = TRUE) %>%
  unite(POST_3, matches("POST1[ab]_3"), na.rm = TRUE) %>%
  unite(POST_4, matches("POST1[ab]_4"), na.rm = TRUE) %>%
  unite(POST_5, matches("POST1[ab]_5"), na.rm = TRUE) %>%
  unite(POST_6, matches("POST1[ab]_6"), na.rm = TRUE) %>%
  drop_na()

d <- left_join(dem, pret, by = "id")
d <- left_join(d, treat, by = "id")
d <- left_join(d, post, by = "id") %>%
  drop_na()

rm(dem, pret, treat, post)
