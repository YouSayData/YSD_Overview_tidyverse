tidyverse: Key Tips and Libraries for Existing R Users
========================================================
author: Dr. Thomas Koentges, You Say Data Limited
date: 23 Nov 2021
font-family: 'Montserrat'
autosize: true



To Whom It May Concern
========================================================
type: sub-section

1. Rbase-savvy users

2. Former R-users

3. Novel R-users

What is the tidyverse?
========================================================
type: section

- The `tidyverse` is an opinionated framework for working with data in R.

- Designed for Data Science

- It is a collection of libraries

Designed for
========================================================
type: sub-section
source: readability.R

- Code readability

- Convenience

1. Core Libraries
========================================================
type: section


```r
install.packages("tidyverse")
library(tidyverse)
```


```
ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
```

1.1. baseR vs. stringr
========================================================
source: strings.R

## baseR
- `grep()`
- `paste()`
- `nchar()`
- ...

***

## tidyverse

- `str_[...]`


1.2. baseR vs. purrr
========================================================
source: loops.R

## baseR
- `apply()`
- `[slvmt]apply`
- `for()`

***

## tidyverse

- `map`, `map2`, `pmap`
- `map_[...]`
- `walk`

1.2. Advantages of purrr
========================================================

- Less scary for beginners
- Consistent API
- More helpful error messages, instead:


```
Error in match.fun(FUN) :
 argument "FUN" is missing, with no default
```

- Negative: `apply` often faster 

2. tidyverse includes: Additional Import Libraries
========================================================
type: section


```r
install.packages("tidyverse")
library(DBI)
library(haven)
library(httr)
library(readxl)
library(googlesheets4)
library(googledrive)
library(rvest)
library(jsonlite)
library(xml2)
```

2.1 dbplyr and DBI
========================================================
source: dbi_example.R

It just works.

3. tidyverse includes: Additional Wrangle and Programming Libraries
========================================================
type: section


```r
install.packages("tidyverse")
library(lubridate)
library(hms)

library(blob)
library(glue)
library(magritter)
```

3.1. Kommienezuspadt or working with time and dates
========================================================
source: dates.R

1. Does every year have 365 days?
2. Does every day have 24 hours?
3. What is 9am?

4. tidyverse Satellites 
========================================================
type: section


```r
install.packages("tidymodels")
install.packages("patchwork")
install.packages("gt")

library(tidymodels)
library(patchwork)
library(gt)
```

4.1. patchwork
========================================================
source: patchwork_plots.R

![](https://raw.githubusercontent.com/allisonhorst/stats-illustrations/master/rstats-artwork/patchwork_1.jpg)

5. More tidyverse tricks 
========================================================
type: section
source: tricks.R

Ngā mihi nui!
========================================================
type: prompt
title: false

# Ngā mihi nui!

## Special offer

If you book a one-on-one session at [https://yousaydata.com](https://yousaydata.com), use the code __YOUSAYR__ to get 35% off.





