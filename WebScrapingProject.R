library(rvest)
url <- "https://en.wikipedia.org/wiki/Odds"
odds <- url %>%
  html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/table') %>%
  html_table()
odds_df <- as.data.frame(odds)

library(dplyr)
odds_df <- odds_df[-1, ]
colnames(odds_df) <- c("Odds Ratio", "Odds in Favor", "Odds Against", "Success Probability", "Failure Probability")
final_df <- odds_df[-8, ]
