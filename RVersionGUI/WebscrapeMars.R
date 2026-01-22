library(rvest)
library(dplyr)

url_mars <- "http://www.braeunig.us/space/atmmars.htm"

df <- read_html(url_mars) %>% 
    html_table() %>% 
    .[[3]] %>% 
    .[-2,]

colnames(df) <- df[1,]
df <- df[-1,]
