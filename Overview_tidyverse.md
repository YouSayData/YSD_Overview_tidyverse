<style>
#logo{
    position: absolute;
    bottom: 0%;
    left: 0%;
    width: 400px;
    height: 100px;
}
</style>
tidyverse and beyond: Key Tips and Libraries for Existing R Users
========================================================
author: Dr. Thomas Koentges, You Say Data Limited
date: 28 Jun 2023
font-family: 'Montserrat'
autosize: true

<div id='logo'>
<svg viewBox="0 0 600 100" fill="none" xmlns="http://www.w3.org/2000/svg">
<rect fill="#FEFFFE" />
<path d="M44.95 50.5642V42.9131L0 24.1953V30.8217L39.4849 46.7386L0 62.6556V69.2819L44.95 50.5642ZM74.9942 50.5642V42.9131L30.0444 24.1953V30.8217L69.5294 46.7386L30.0444 62.6556V69.2819L74.9942 50.5642Z" fill="#7186D1" />
<path d="M133.916 30.5996L113.764 70.2895C110.348 76.9842 106.318 79.2385 100.306 79.2385C98.0519 79.2385 96.1932 78.7518 94.827 78.0686V72.7317C96.5348 73.4831 98.1886 73.9101 100.033 73.9101C103.449 73.9101 106.454 72.6805 108.367 68.2401L109.46 65.8491L91.084 30.5996H97.9153L112.671 59.6327L127.222 30.5996H133.916Z" fill="#FEFFFE">
<animate attributeType="XML" attributeName="fill" values="#FEFFFE;#10284D" begin="0.2s" dur="0.1s" fill="freeze" calcMode="discrete" />
</path>
<path d="M154.084 30C142.607 30 134 37.5828 134 48.5813C134 59.5114 142.607 67.1625 154.084 67.1625C165.424 67.1625 174.1 59.5114 174.1 48.5813C174.1 37.5828 165.424 30 154.084 30ZM154.084 61.8341C146.023 61.8341 139.875 56.6423 139.875 48.5813C139.875 40.452 146.023 35.3285 154.084 35.3285C162.009 35.3285 168.157 40.452 168.157 48.5813C168.157 56.6423 162.009 61.8341 154.084 61.8341Z" fill="#FEFFFE">
<animate attributeType="XML" attributeName="fill" values="#FEFFFE;#10284D" begin="0.3s" dur="0.1s" fill="freeze" calcMode="discrete" />
</path>
<path d="M209.866 30.4517H216.014V66.1794H209.866V60.0312C207.338 63.7201 202.966 66.9308 196.135 66.9308C186.298 66.9308 180.765 60.3045 180.765 50.604V30.4517H186.913V49.9209C186.913 56.7522 190.328 61.2608 197.706 61.2608C204.811 61.2608 209.866 56.4789 209.866 48.8962V30.4517Z" fill="#FEFFFE">
<animate attributeType="XML" attributeName="fill" values="#FEFFFE;#10284D" begin="0.4s" dur="0.1s" fill="freeze" calcMode="discrete" />
</path>
<path d="M264.217 66.9308C255.2 66.9308 247.549 63.7885 244.065 57.7769L248.574 54.0197C251.784 59.3481 257.249 61.7391 264.149 61.7391C270.365 61.7391 275.147 59.9629 275.147 55.9324C275.147 51.697 270.912 51.5604 262.168 50.604C253.287 49.5793 246.251 48.1448 246.251 40.8353C246.251 34.2772 252.877 29.7686 262.441 29.7686C271.049 29.7686 277.47 33.1842 280.339 37.7612L276.172 41.3818C273.508 37.1464 268.521 34.892 262.236 34.892C255.883 34.892 252.194 37.0097 252.194 40.2888C252.194 43.8307 255.794 44.2485 263.399 45.1313L263.466 45.139C273.03 46.232 281.227 47.0518 281.227 55.4543C281.227 62.9687 273.098 66.9308 264.217 66.9308Z" fill="#FEFFFE">
<animate attributeType="XML" attributeName="fill" values="#FEFFFE;#10284D" begin="0.6s" dur="0.1s" fill="freeze" calcMode="discrete" />
</path>
<path d="M300.925 66.93C307.687 66.93 313.767 64.5392 316.977 60.4406L317.046 60.7822C317.661 64.949 321.349 67.4764 328.18 66.1786V61.2603C324.218 62.1483 322.442 61.3969 322.442 58.7329V42.6121C322.442 34.3467 316.294 29.77 305.98 29.77C297.646 29.77 290.2 33.6636 287.946 39.6748L293.069 41.9973C294.572 37.8988 299.764 35.0298 305.843 35.0298C312.811 35.0298 316.499 38.0354 316.499 42.5437V43.4318L300.72 45.2078C292.318 46.1641 286.58 49.5795 286.58 56.2055C286.58 63.2413 292.933 66.93 300.925 66.93ZM316.499 51.902C316.499 58.5963 308.439 61.8751 301.403 61.8751C296.075 61.8751 292.591 59.8259 292.591 56.1372C292.591 52.0387 296.28 50.6725 301.608 50.0577L316.499 48.35V51.902Z" fill="#FEFFFE">
<animate attributeType="XML" attributeName="fill" values="#FEFFFE;#10284D" begin="0.7s" dur="0.1s" fill="freeze" calcMode="discrete" />
</path>
<path d="M370.878 30.5996L350.726 70.2895C347.31 76.9842 343.28 79.2385 337.268 79.2385C335.014 79.2385 333.155 78.7518 331.789 78.0686V72.7317C333.497 73.4831 335.15 73.9101 336.995 73.9101C340.411 73.9101 343.416 72.6805 345.329 68.2401L346.422 65.8491L328.046 30.5996H334.877L349.633 59.6327L364.183 30.5996H370.878Z" fill="#FEFFFE">
<animate attributeType="XML" attributeName="fill" values="#FEFFFE;#10284D" begin="0.8s" dur="0.1s" fill="freeze" calcMode="discrete" />
</path>
<path d="M424.893 59.889V66.1731H431.04V20H424.83L424.893 36.7335C422.229 32.9084 416.969 29.7664 409.865 29.7664C397.844 29.7664 390.33 37.963 390.33 48.3454C390.33 58.6595 397.844 66.9245 409.865 66.9245C416.969 66.9245 422.229 63.7824 424.893 59.889ZM425.166 48.0722V48.5504C425.166 56.6787 418.13 61.665 410.617 61.665C402.625 61.665 396.204 56.6104 396.204 48.3454C396.204 40.0122 402.625 34.9576 410.617 34.9576C418.13 34.9576 425.166 40.0122 425.166 48.0722Z" fill="#FEFFFE">
<animate attributeType="XML" attributeName="fill" values="#FEFFFE;#10284D" begin="1s" dur="0.1s" fill="freeze" calcMode="discrete" />
</path>
<path d="M452.045 66.93C458.808 66.93 464.887 64.5392 468.098 60.4406L468.166 60.7822C468.781 64.949 472.469 67.4764 479.3 66.1786V61.2603C475.338 62.1483 473.562 61.3969 473.562 58.7329V42.6121C473.562 34.3467 467.414 29.77 457.1 29.77C448.766 29.77 441.321 33.6636 439.066 39.6748L444.19 41.9973C445.692 37.8988 450.884 35.0298 456.963 35.0298C463.931 35.0298 467.619 38.0354 467.619 42.5437V43.4318L451.84 45.2078C443.438 46.1641 437.7 49.5795 437.7 56.2055C437.7 63.2413 444.053 66.93 452.045 66.93ZM467.619 51.902C467.619 58.5963 459.559 61.8751 452.523 61.8751C447.195 61.8751 443.711 59.8259 443.711 56.1372C443.711 52.0387 447.4 50.6725 452.728 50.0577L467.619 48.35V51.902Z" fill="#FEFFFE">
<animate attributeType="XML" attributeName="fill" values="#FEFFFE;#10284D" begin="1.1s" dur="0.1s" fill="freeze" calcMode="discrete" />
</path>
<path d="M501.62 66.5895C494.105 66.5895 488.094 63.5837 488.094 54.9763V35.712H479.418V30.4519H488.094V20H494.174V30.4519H510.705V35.712H494.174V54.4298C494.174 59.485 497.179 61.0562 502.576 61.0562C505.582 61.0562 508.314 60.373 510.91 59.4166L512.704 64.4006C509.972 65.4936 505.24 66.5895 501.62 66.5895Z" fill="#FEFFFE">
<animate attributeType="XML" attributeName="fill" values="#FEFFFE;#10284D" begin="1.2s" dur="0.1s" fill="freeze" calcMode="discrete" />
</path>
<path d="M530.257 66.9308C537.02 66.9308 543.1 64.5399 546.31 60.4411L546.379 60.7827C546.994 64.9498 550.683 67.4774 557.514 66.1794V61.2609C553.552 62.1489 551.775 61.3975 551.775 58.7333V42.6114C551.775 34.3455 545.627 29.7686 535.312 29.7686C526.978 29.7686 519.532 33.6624 517.277 39.6739L522.401 41.9966C523.904 37.8978 529.096 35.0287 535.175 35.0287C542.143 35.0287 545.832 38.0344 545.832 42.5431V43.4312L530.052 45.2073C521.649 46.1637 515.911 49.5793 515.911 56.2057C515.911 63.2419 522.264 66.9308 530.257 66.9308ZM545.832 51.902C545.832 58.5967 537.771 61.8757 530.735 61.8757C525.407 61.8757 521.923 59.8263 521.923 56.1374C521.923 52.0386 525.612 50.6724 530.94 50.0575L545.832 48.3497V51.902Z" fill="#FEFFFE">
<animate attributeType="XML" attributeName="fill" values="#FEFFFE;#10284D" begin="1.3s" dur="0.1s" fill="freeze" calcMode="discrete" />
</path>
<path d="M577.594 72.1512H564.294V66.8911H577.594H586.701H600V72.1512H586.701H577.594Z" fill="#FEFFFE">
<animate attributeType="XML" attributeName="fill" values="#FEFFFE;#7186D1" begin="0.8s" dur="1.5s" calcMode="discrete" repeatCount="indefinite" />
</path>
</svg>
</div>



The talk's audience
========================================================
type: sub-section

1. Rbase-savvy users

2. Former R-users

3. Novice R-users

What is the tidyverse?
========================================================
type: section

- The `tidyverse` is an opinionated framework for working with data in R

- Designed for data science

- It is a collection of libraries

Designed for
========================================================
type: sub-section
source: readability.R

- Code readability

- Convenience

- Consistency

1. Core libraries
========================================================
type: section


```r
install.packages("tidyverse")
library(tidyverse)
```


```
ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate
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

2. tidyverse includes: additional import libraries
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

3. tidyverse includes: additional wrangle and programming libraries
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

3.1. Working with time and dates
========================================================
source: dates.R

1. Does every year have 365 days?
2. Does every day have 24 hours?
3. What is 9am?

4. tidyverse satellites 
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

Feel free to book a one-on-one session at [https://yousaydata.com](https://yousaydata.com) or get in touch on [LinkedIn: Thomas Koentges](https://www.linkedin.com/in/thomas-koentges/).






