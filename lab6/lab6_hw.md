---
title: "dplyr Superhero"
date: "2024-02-20"
output:
  html_document: 
    theme: spacelab
    toc: true
    toc_float: true
    keep_md: true
---

## Learning Goals  
*At the end of this exercise, you will be able to:*    
1. Develop your dplyr superpowers so you can easily and confidently manipulate dataframes.  
2. Learn helpful new functions that are part of the `janitor` package.  

## Instructions
For the second part of lab today, we are going to spend time practicing the dplyr functions we have learned and add a few new ones. This lab doubles as your homework. Please complete the lab and push your final code to GitHub.  

## Load the libraries

```r
library("tidyverse")
```

```
## Warning: package 'dplyr' was built under R version 4.2.3
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library("janitor")
```

```
## 
## Attaching package: 'janitor'
## 
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

## Load the superhero data
These are data taken from comic books and assembled by fans. The include a good mix of categorical and continuous data.  Data taken from: https://www.kaggle.com/claudiodavi/superhero-set  

Check out the way I am loading these data. If I know there are NAs, I can take care of them at the beginning. But, we should do this very cautiously. At times it is better to keep the original columns and data intact.  

```r
#superhero_info <- read_csv("data/heroes_information.csv", na = c("", "-99", "-"))
#superhero_powers <- read_csv("data/super_hero_powers.csv", na = c("", "-99", "-"))
```

## Data tidy
1. Some of the names used in the `superhero_info` data are problematic so you should rename them here. Before you do anything, first have a look at the names of the variables. You can use `rename()` or `clean_names()`.    

## `tabyl`
The `janitor` package has many awesome functions that we will explore. Here is its version of `table` which not only produces counts but also percentages. Very handy! Let's use it to explore the proportion of good guys and bad guys in the `superhero_info` data.  

```r
#tabyl(superhero_info, alignment)
```

1. Who are the publishers of the superheros? Show the proportion of superheros from each publisher. Which publisher has the highest number of superheros?  

2. Notice that we have some neutral superheros! Who are they? List their names below.  

## `superhero_info`
3. Let's say we are only interested in the variables name, alignment, and "race". How would you isolate these variables from `superhero_info`?

## Not Human
4. List all of the superheros that are not human.

## Good and Evil
5. Let's make two different data frames, one focused on the "good guys" and another focused on the "bad guys".

6. For the good guys, use the `tabyl` function to summarize their "race".

7. Among the good guys, Who are the Vampires?

8. Among the bad guys, who are the male humans over 200 inches in height?

9. Are there more good guys or bad guys with green hair?  

10. Let's explore who the really small superheros are. In the `superhero_info` data, which have a weight less than 50? Be sure to sort your results by weight lowest to highest.  

11. Let's make a new variable that is the ratio of height to weight. Call this variable `height_weight_ratio`.    

12. Who has the highest height to weight ratio?  

## `superhero_powers`
Have a quick look at the `superhero_powers` data frame.  

13. How many superheros have a combination of agility, stealth, super_strength, stamina?

## Your Favorite
14. Pick your favorite superhero and let's see their powers!  

15. Can you find your hero in the superhero_info data? Show their info!  

## Push your final code to GitHub!
Please be sure that you check the `keep md` file in the knit preferences.  
