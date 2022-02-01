# Tidy Qualtrics Data

# Clean demographics
dem <- d %>%
  filter(Consent == 4, AC2_1 ==1, AC2_4 == 1,
         tic != "359766d9041e7d8c345c0c2838781095", 
         tic != "daabba41247f457b637205e066bbe39b",
         tic != "f6b4160f5ab5dee05b7e925d50e5414") %>%
  select(matches("D\\d"), id = tic) %>%
  select(-matches("D6_7_TEXT")) %>%
  select(id, D4:D10) %>%
  mutate(D9 = if_else(is.na(D9), round(mean(D9, na.rm = T),0), D9),
         D10 = if_else(is.na(D10), round(mean(D10, na.rm = T),0), D10))

pret <- d %>%
  filter(Consent == 4, AC2_1 ==1, AC2_4 == 1,
         tic != "359766d9041e7d8c345c0c2838781095", 
         tic != "daabba41247f457b637205e066bbe39b",
         tic != "f6b4160f5ab5dee05b7e925d50e5414") %>%
  select(matches("PT\\d"), matches("HT4_\\d+"), id = tic) %>%
  select(-matches("PT8_NPS_GROUP"), -matches("PT[13]"), -matches("PT[13]_\\d+")) %>%
  select(id, PT4, PT5 = PT5_1, PT6:HT4_4) %>%
  mutate(HT4_1 = if_else(is.na(HT4_1), round(mean(HT4_1, na.rm = T),0), HT4_1),
         HT4_2 = if_else(is.na(HT4_2), round(mean(HT4_2, na.rm = T),0), HT4_2),
         HT4_3 = if_else(is.na(HT4_3), round(mean(HT4_3, na.rm = T),0), HT4_3),
         HT4_4 = if_else(is.na(HT4_4), round(mean(HT4_4, na.rm = T),0), HT4_4))


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
  select(id = tic, matches("POST1[ab]_[1234568]")) %>%
  unite(POST_1, matches("POST1[ab]_1"), na.rm = TRUE) %>%
  unite(POST_2, matches("POST1[ab]_2"), na.rm = TRUE) %>%
  unite(POST_3, matches("POST1[ab]_3"), na.rm = TRUE) %>%
  unite(POST_4, matches("POST1[ab]_4"), na.rm = TRUE) %>%
  unite(POST_5, matches("POST1[ab]_5"), na.rm = TRUE) %>%
  unite(POST_6, matches("POST1[ab]_6"), na.rm = TRUE) %>%
  unite(POST_8, matches("POST1[ab]_8"), na.rm = TRUE) 

d <- left_join(dem, pret, by = "id")
d <- left_join(d, treat, by = "id")
d <- left_join(d, post, by = "id") 

rm(dem, pret, treat, post)

dem <- read_delim(here("data/raw-private/14651 data matched final.csv"), 
                  delim = ";") %>%
  select(id = tic, D2 = age, D1 = gender, D3 = education)

d <- left_join(d, dem, by = "id") %>%
  mutate(D1 = if_else(is.na(D1), round(mean(D1, na.rm = T),0), D1),
         D2 = if_else(is.na(D2), round(mean(D2, na.rm = T),0), D2),
         D3 = if_else(is.na(D3), round(mean(D3, na.rm = T),0), D3))

rm(dem)