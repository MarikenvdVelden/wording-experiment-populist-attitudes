# Data, Measurement & Method
To investigate whether a civic or ethnic conception of the people influences the populist attitude constructs, we conducted a survey experiment in Germany between the 13th of October 2021 and the 11th of November 2021.
The sample, recruited through [Respondi](https://www.respondi.com/EN/), consists of 8,000 participants of 18 years and older.
Respondi works with opt-in respondents, so we have implemented quota on age, gender, and education.
Moreover, we measured some more demographic background variables.
Balance checks are conducted to demonstrate whether certain categories are over represented in a certain experimental group (see [the Methods Section](#method), Figure \ref{fig:balance}).
  The study has been approved by the [Research Ethics Review Committee](https://fsw.vu.nl/nl/onderzoek/research-ethics-review/index.aspx) of the \textit{Vrije Universiteit Amsterdam} (see the approval [here](https://github.com/MarikenvdVelden/wording-experiment-populist-attitudes/blob/master/docs/pap/pap.pdf)).
  To ensure good quality of our data, two attention checks are included.
  All variables and their operationalization have been described in our [pre-analysis plan](https://github.com/MarikenvdVelden/wording-experiment-populist-attitudes/blob/master/docs/pap/pap.pdf).
  
  The dependent variables under study are the three of the six items measuring populist attitudes that mention _the people_: 1) _The people, not politicians, should make the most important political decisions_; 2) _The politicians in the German Bundestag need to follow the will of the people_; and 3) _The political differences between elites and the people are greater than the differences among citizens_.
We rely on those three items in the populism scale, see Table \ref{tab:dv} in the [Appendix](# Appendix A. Wording of the questionaire) for the exact phrasing of the question.
  Each item is measured on a 5-point likert scale: from `very much disagree` (value of `0`) till `very much agree` (value of `5`).
  The average for each of the items lies between 3 and 4 (with a standard deviation around 1, as shown in Table \ref{tab:descr-dv}), meaning that people are neutral till positive about these statements. 
  
  ```{r descriptive-table, echo = FALSE}
  rm(list=ls())
  here::i_am("report/draft.Rmd")
  source(here::here("src/lib/functions_draft.R"))
  load(here("data/intermediate/cleaned_experiment.RData"))
  source(here("src/analysis/data-for-analyses.R"))
  
  descr <- d %>%
    select(matches("POST_[234]")) %>%
    pivot_longer(cols = everything()) %>%
    mutate(Variable = recode(name,
                             `POST_2` = "DV: People, not politicians, \n should make the most \n important political decisions",
                             `POST_3` = "DV: MPs should follow \n the will of the people",
                             `POST_4` = "DV: Differences between elites and \n the people are bigger than \n differences between the people")) %>%
    group_by(Variable) %>%
    summarise(`Mean Value` = round(mean(value, na.rm = T),2),
              `St.dev` = round(sd(value, na.rm = T), 2),
              `Min. Value` = min(value),
              `Max. Value` = max(value))
  
  kbl(descr, booktabs =T, caption = "\\label{tab:descr-dv}Descriptive Information of Dependent Variables",
      digits = 2) %>%
    kable_styling(latex_options = c("striped", "hold_position"),
                  full_width = F, fixed_thead = T, position = "center")  %>%
    column_spec(1, width = "5.5cm") %>%
    column_spec(2, width = "2cm") %>%
    column_spec(3, width = "1.5cm") %>%
    column_spec(4, width = "2cm") %>%
    column_spec(5, width = "2cm")
  rm(descr)  
  ```
  
  Given the historical context of Germany, we randomize whether _the people_ is translated into German using either the civic conception (i.e. _Bürger_) or the ethnic conception (i.e. _Volk_).
  `50.2\%` of the respondents (3347 in total) got the civic conception treatment, and `49.8\%` of the respondents (3317 in total) was randomized into the ethnics conception treatment.
  
  To measure the heterogenous treatment effects, we measure _Exclusive national identity_ with four statements (_being born in Germany_, _having German ancestors_, _being able to speak German_, _adhering to German traditions and customs_) on a 5-point Likert-scale from `very much disagree` (score of `1`) to `very much agree` (score of `5`). 
  Becasue $Cronbach's \alpha \geq 0.8$, we have constructed an additive scale for this measure.
The constructed scale has a mean of 3.61 (with a standard deviation of .89), meaning that people are neutral till agreeing with a exclusive view on national identity. 
We measured voting for a far right party by asking whether people had voted for the _Alternative für Deutschland_ (AfD) in the recent elections.
About `10\%` of the respondents recalled to have voted for the AfD.

We have also measured background information on age, gender, educational level, geographical region, vote recall, emplyment, and income.
In our [online compendium](https://github.com/MarikenvdVelden/wording-experiment-populist-attitudes), we demonstrate all the descriptive information.
As detailed in our pre-analysis plan, we only include variables as covariates that are unbalanced. 
As Figure \ref{fig:balance} shows, only the income covariate is unbalanced accross the experimental groups.
We therefore add this covariate to the regression analyses (H2a and H2b).
Income is measured based on respondents' monthly income in bins of 500 Euro, ranging from $\leq$ `500` to $\geq$ `7501`.
  
  ```{r balance, fig.align = 'center', out.width = "70%", fig.cap = "\\label{fig:balance}Balance Checks"}
  knitr::include_graphics(here::here("report/figures", "balance-checks-1.png"))
  ```
  
  
  # Wording Effects of Ethnic or Civic Conception of _the People_
  
  To test whether the historical context of Germany affects citizens' populists attitudes when use the civic conception or the ethnic conception (H1), we first conducted a difference in means for the group who received the ethnic and the civic question wording, for all populist items containing a reference to _the people_.
Figure \ref{fig:results-h1} demonstrates the differences between the scores on the three items of the populist attitudes scale that use the word _the people_.
Negative values indicate that respondents who saw the ethnic conception of the people on average scored higher on the item compared to respondents in the civic conception condition.
Positive values indicate the reverse. 
Our pre-registered hypothesis stated that respondents in the ethnic conception condition would agree less (i.e. lower scores) than those in the civic condition.
Figure \ref{fig:results-h1} demonstrates that only four the item ``The political differences between elites and the people are greater than the differences among citizens'' we find support for this hypothesis. 
For the other two items -- a) The people, not politicians, should make the most important political decisions, and b) The politicians in the German Bundestag need to follow the will of the people -- we find statstically significant results in the opposite direction.
For these two items, we find that people who saw the ethnic conception of the people scored higher than the other group.
This could mean either that the historical notion of the ethnic conception is fading in the German context, or that nativism is a strong element of German populism.
[WE THEREFORE HAVE RUN TWO ANALYSIS TO EXPLORE THIS. FIRST, WE CHECK WHETHER THE DIFFERENCES BETWEEN THE CIVIC AND ETHNIC CONCEPTION ARE AGE DEPENDENT. SECONDLY, WE EXAMINE IF THE EFFECT IS CONDITIONAL ON GEOGRAPHICAL REGIONS].



Our second set of pre-registered hypotheses (H2a and H2b), are directed at the heterogeneous treatment effects: Are the diifferences between the civic and ethnic conception of the people conditional upon having an exclusive national identity (_H2a_) and/or voting for a far right party (_H2b_).
To test this, we use linear multivariate regressions, with an interaction between the treatment conditions and the variable of interest in the heterogenous treatment (i.e. respectively having a exclusive national identity and voting for a far right party).
First, we hypothesized that the more one adheres to an exclusive national identity, they likely score higher on the items when they are in the ethnic conception condition.
The left-panel of Figure \ref{fig:results-h2} demonstrates that this only holds for the item ``The people, not politicians, should make the most important political decisions''.
For this item, Figure \ref{fig:results-h2} shows a positive and statistically significant effect as the adherence to an exclusive national identity increases.
Compared to not adhering to an exclusive national identity, the average score of fully adhering to an exclusive national identity is 0.2 higher on a 5-point scale.
Compared the standard deviation of this item (1.17, see Table \ref{tab:descr-dv}), this is a small effect.
For the other two items, the lines are almost flat over the various levels of adherence to an exclusive national identity and not statistically significant.
Testing whether voting for a far right party conditions the scores on the populist attitude items, the right-panel of \ref{fig:results-h2} shows that the average score on all the items is higher for an ethnic conception of the people than for the civic conception.
Hence, this indicates support for H2b. 
As the right panel of \ref{fig:results-h2} demonstrates, the effect for the item The people, not politicians, should make the most important political decisions is the largest: Compared to voting for all other parties, when voting for the AfD and being in the ethnic conception of the people condition, one scores on average a full point higher on a 5-point scale (i.e. about a full standard deviation).
For the items a) MPs should follow the will of the people and b) Differences between elites and the people are bigger than differences between the people the average effect is half a point on a 5-point scale (i.e. about half a standard deviation).



[EXPLORATIVE: regressions]

```{r results-explorative, out.width = "100%", fig.align = 'center', fig.cap = "\\label{fig:results-explorative}Results: Explorative"}
knitr::include_graphics(here::here("report/figures", "explorative-1.png"))
```

[EXPLORATIVE: PCA]