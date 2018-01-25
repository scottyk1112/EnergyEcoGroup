"I was gonna put this Excel reading library and the code I used in another project to upload data from
multipe worksheets. Then as I tried to make it comprehensible, I thought I might as well get the data up.
I was thinking that if we're working with the Excel, we should probably put it up on Github,
so we can each run the script, so that's what I did
I also started this as an R script, but copy and paste this if it's useful but you want to start a Shiny App,
or a markdown file instead.
I don't think it works to write Shiny in markdown, but I'd be happy to be proven wrong - Eli [2018-01-24]"

library("readxl")
library("dplyr")

file <- "NFA_2017_CLUM.xlsx" 

CLUM_percap <- read_excel(file, sheet = 11)

#Or if we want to go through and get data from multiple sheets
"
sheets <- excel_sheets(file)

for (i in sheets[11]){
  assign(i, read_excel(file, sheet = i))
}  

for (i in sheets[-1:-2]){
  assign(i, read_excel(file, sheet = i)[2,1:6])
}
"
CLUM_2011 <- filter(CLUM_percap, year==2011)
