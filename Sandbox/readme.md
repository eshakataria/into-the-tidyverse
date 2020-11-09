# Introduction
This folder is deliberately left empty, and Jae will never add new materials here. Here are three rules you need to follow to ensure everything goes smoothly.

## Rule 1:
Basically, don't touch anything in the Code and Data folders, and you'll be alright. For those of you following along in realtime (i.e., Jae hasn't finished writing/developing all of the materials yet), this lets you avoid merge conflicts. In short, a merge conflict emerges when my version of the file looks different from your version of the same file. So if you take Jae's code and you use it to play around by adding/deleting code, your file is now different from Jae's file. When you try to "sync" new materials from Jae's repository to your forked clone, this creates a merge conflict that you're forced to resolve before you're allowed to download Jae's new materials.

## Rule 2:
Any time you write new code, do it in this Sandbox folder. The point of a sandbox is that you can do anything you want, and the (potentially destructive) consequences of your actions will never leave the sandbox. This is important because Jae is going to encourage you very, very, very strongly never to simply run the existing scripts in the Code folder (or equivalently, simply copy/pasting existing code). So when you're typing code from scratch and need a place to save your scripts, save them in the Sandbox.

## Rule 3:
On that note, you should be typing code from scratch. This sandbox should be full of code, whether it works or not. You learn a lot from typing things from scratch, especially when you're a beginner. This is because you'll inevitably type things incorrectly when you do it from scratch, and then you'll have to self-diagnose why your code doesn't work. That is invaluable experience for learning how to code. And it will also help with your muscle memory. It takes virtually zero time for Jae to write a pipe (%>%) because Jae has typed thousands and thousands and thousands of pipes. You cannot get good at writing code quickly without building muscle memory. If you simply run existing code, or copy/paste existing code, you are hurting your own learning. There are no shortcuts to learning code. You must do it the hard way.

library(tidyverse)

here("Data", "2020_Property_Tax_Roll.csv")

here("2020_Property_Tax_Roll.csv") %>%
  read.csv() %>%
  head()
  
  
base_read <- here("2020_Property_Tax_Roll.csv") %>%
  read.csv()
  
  base_read %>%
  str()
  
  tidy_read <- here("2020_Property_Tax_Roll.csv") %>%
  read_csv()
  
tidy_read_mod <- here("2020_Property_Tax_Roll.csv") %>%
  read_csv(col_types = cols(ZIP_POSTAL = col_character(),plat = col_character()))
  
tidy_read %>%
  str()
  
covid_usa <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")


bank_branches<-read.delim("BankBranchesData.txt")