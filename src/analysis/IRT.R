packages <- c( "ltm", "GPArotation", "Hmisc", "mirt",
              "lattice","lordif", "semPlot",
              "semTools", "nFactors", "corrr")
for (p in packages) {
  if (p %in% installed.packages()[,1]) require(p, character.only=T)
  else {
    install.packages(p)
    library(p, character.only=T)
  }
}

#Subset for all populist items
pop_e <- d %>%
  dplyr::filter(ethnic == 1) %>%
  dplyr::select(matches("POST_[12356]"))
  
pop_c <- d  %>%
  dplyr::filter(ethnic == 0) %>%
  dplyr::select(matches("POST_[12356]"))

#Descriptive Statistics for all populism items
stats_e <- psych::describe(pop_e)
stats_c <- psych::describe(pop_c)

#polychoric correlations of all items
#psych::polychoric(pop_e)
#psych::polychoric(pop_c)

##Correlation Matrix, Screeplot, Bifactor Model
#cor(pop_e)
#cor(pop_c)

#psych::omega(pop_e)
#psych::omega(pop_c)

#psych::alpha(pop_e)
#psych::alpha(pop_c)

# Screeplot 
ev_e <- eigen(cor(pop_e)) # get eigenvalues
ev_c <- eigen(cor(pop_c)) # get eigenvalues

ap_e <- parallel(subject=nrow(pop_e),var=ncol(pop_e),
                rep=100,cent=.05)
ap_c <- parallel(subject=nrow(pop_c),var=ncol(pop_c),
                 rep=100,cent=.05)

nS_e <- nScree(x=ev_e$values, aparallel=ap_e$eigen$qevpea)
nS_c <- nScree(x=ev_c$values, aparallel=ap_c$eigen$qevpea)

#plotnScree(nS_e, main="", xlab="Factors")
#plotnScree(nS_c, main="", xlab="Factors")

# Further tests for dimensionality:
#nfactors(pop_e)
#nfactors(pop_c)

# Exploratory Factor Analysis 
pop_e_EFA <- factanal(pop_e, 1, rotation="varimax", covmat = NULL)
pop_c_EFA <- factanal(pop_c, 1, rotation="varimax", covmat = NULL)

# To estimates of reliability: Cronbach's alpha, Revelles beta, and Guttman's Lambda 6.
tmp_e <- cov(pop_e)
tmp_c <- cov(pop_c)

#ic.out_e <- iclust(tmp_e)
#summary(ic.out_e)

#ic.out_c <- iclust(tmp_c)
#summary(ic.out_c)

# Mean Inter-item correlation
#https://www.r-bloggers.com/five-ways-to-calculate-internal-consistency/

#inter_pop_e <- pop_e %>% correlate() %>%
  #dplyr::select(-term) %>% 
  #colMeans(na.rm = TRUE)
#mean(inter_pop_e)

#inter_pop_c <- pop_c %>% correlate() %>%
  #dplyr::select(-term) %>% 
  #colMeans(na.rm = TRUE)
#mean(inter_pop_c)

# Mean item-total correlation
pop_e_C <- pop_e
pop_e_C$score <- rowMeans(pop_e_C)
item_total_e <- pop_e_C %>% correlate() %>% focus(score)
#mean(item_total_e$score)

pop_c_C <- pop_c
pop_c_C$score <- rowMeans(pop_c_C)
item_total_c <- pop_c_C %>% correlate() %>% focus(score)
#mean(item_total_c$score)

##Graded Response Model
fit_e <- grm(pop_e)
#fit_e
pattern_e<-factor.scores(fit_e, resp.pattern=pop_e)

fit_c <- grm(pop_c)
#fit_c
pattern_c<-factor.scores(fit_c, resp.pattern=pop_c)

# Plot Figure 1
#https://www.dropbox.com/s/xacryjt5kakm6fn/IRT%20measurement%20populism%20%5BPolitics%5D%20replication%20materials.zip?dl=0&file_subpath=%2FVanHauwaertEtAl.2019_R_Script_Replication.R
vals_e <- plot(fit_e, type = "IIC", items = 0, plot = FALSE, zrange = c(-5,5)) %>%
  as_tibble() %>%
  mutate(id = "Ethnic Conception",
         se = z / sqrt(test.info))
  
vals_c <- plot(fit_e, type = "IIC", items = 0, plot = FALSE, zrange = c(-5,5))  %>%
  as_tibble() %>%
  mutate(id = "Civic Conception",
         se = z / sqrt(test.info))



irt1 <- vals_e %>% add_case(vals_c) %>%
  ggplot() +
  geom_line(aes(x = z, y = test.info, color = id),position = position_dodge(.5)) +
  geom_line(aes(x = z, y = se/10, color = id), linetype = "dotted",
            position = position_dodge(.5)) +
  labs(x = "\u03b8") +
  scale_color_manual(values = fig_cols) +
  scale_y_continuous(
    # Features of the first axis
    name = "Information(\u03b8)",
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans = ~ . * 10, name="SE(\u03b8)")) +
  theme_ipsum() +
  theme(legend.position="bottom",
        legend.title = element_blank())


## Figure 2 
#Extract IIC for each of the 6 items & convert them into dataframe:
df_e <- tibble(
  z = plot(fit_e, type = "IIC", items = 1, plot = T, zrange = c(-5,5))[,1],
  item1 = plot(fit_e, type = "IIC", items = 1, plot = T, zrange = c(-5,5))[,2],
  item2 = plot(fit_e, type = "IIC", items = 2, plot = T, zrange = c(-5,5))[,2],
  item3 = plot(fit_e, type = "IIC", items = 3, plot = T, zrange = c(-5,5))[,2],
  item4 = plot(fit_e, type = "IIC", items = 4, plot = T, zrange = c(-5,5))[,2],
  item5 = plot(fit_e, type = "IIC", items = 5, plot = T, zrange = c(-5,5))[,2]) %>%
  pivot_longer(cols = item1:item5) %>% # Reshaping dataset for graphing purposes
  dplyr::mutate(name = dplyr::recode(name,
                       `item1` = "Item 1 - Compromise is selling out",
                       `item2` = "Item 2 - The people must decide",
                       `item3` = "Item 3 - Follow will of the peopl",
                       `item4` = "Item 5 - Too much talk, no action",
                       `item5` = "Item 6 - Rep. by ordinary citizen"),
         id  = "Ethnic Conception")

df_c <- tibble(
  z = plot(fit_c, type = "IIC", items = 1, plot = T, zrange = c(-5,5))[,1],
  item1 = plot(fit_c, type = "IIC", items = 1, plot = T, zrange = c(-5,5))[,2],
  item2 = plot(fit_c, type = "IIC", items = 2, plot = T, zrange = c(-5,5))[,2],
  item3 = plot(fit_c, type = "IIC", items = 3, plot = T, zrange = c(-5,5))[,2],
  item4 = plot(fit_c, type = "IIC", items = 4, plot = T, zrange = c(-5,5))[,2],
  item5 = plot(fit_c, type = "IIC", items = 5, plot = T, zrange = c(-5,5))[,2]) %>%
  pivot_longer(cols = item1:item5) %>% # Reshaping dataset for graphing purposes
  dplyr::mutate(name = dplyr::recode(name,
                                     `item1` = "Item 1 - Compromise is selling out",
                                     `item2` = "Item 2 - The people must decide",
                                     `item3` = "Item 3 - Follow will of the peopl",
                                     `item4` = "Item 5 - Too much talk, no action",
                                     `item5` = "Item 6 - Rep. by ordinary citizen"),
                id  = "Civic Conception")

irt2 <- df_e %>%
  add_case(df_c) %>%
  ggplot(aes(x = z,
             y = value,
             color = name)) +
  geom_line() +
  labs(x = "\u03b8", y = "I(\u03b8)") +
  facet_grid(.~id) +
  theme_ipsum() +
  scale_color_manual(values = fig_cols) +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

irt_e <- pattern_e$score.dat$z1
irt_c <- pattern_c$score.dat$z1

tmp <- d %>%
  dplyr::mutate(irt_pa = ifelse(ethnic == 1, irt_e,irt_c)) %>%
  dplyr::select(irt_pa)

d <- d %>%
  add_column(tmp)
