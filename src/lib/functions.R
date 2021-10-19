library(here)
library(kableExtra)
library(tidyverse)
library(DeclareDesign)
library(ggpubr)
library(scales)
library(cobalt)
library(margins)
library(lme4)
library(ggstatsplot)
library(ggrepel)
library(haven)
library(sjlabelled)
library(patchwork)
library(qualtRics)

fig_cols <- yarrr::piratepal(palette = "basel", 
             trans = .2)
fig_cols <- as.character(fig_cols[1:8])

api_key_fn <- here("data/raw-private/qualtrics_api_key.txt")
API <- read_file(api_key_fn) %>% trimws()

render_j2 = function(template, output, data, auto_unbox=TRUE, na="string") {
  data = jsonlite::toJSON(data, pretty=TRUE, auto_unbox=auto_unbox, na=na)
  system(glue::glue("env/bin/j2 --format json {template} -o {output}"), input=data)
}
