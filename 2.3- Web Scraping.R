library(rvest)
library(dplyr)
library(tidyverse)

url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")

html_text(nodes[[8]])

html_table(nodes[[8]])

for (i in 1:4) {
    print(html_table(nodes[[i]]))
}

for (i in 0:2) {
    print(html_table(nodes[[length(nodes)-i]]))
}

tab_1 <- nodes[[10]] %>% html_table %>% .[-1, -1] %>% setNames(c("Team", "Payroll", "Average"))
tab_2 <- nodes[[19]] %>% html_table %>% .[-1,] %>% setNames(c("Team", "Payroll", "Average")) %>% full_join(tab_1, by="Team")
tab_1
tab_2
dim(tab_2)

url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

tab <- read_html(url) %>% html_nodes("table")
length(tab)

for (i in 1:10) {
  print(html_table(tab[[i]], fill=TRUE))
}
