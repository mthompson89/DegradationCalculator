# Degradation Calculator
## Overview
This is an R Shiny application used to calculate an approximate degradation level based off of Gunn et. al. (2019) degradation framework. This app also implaments Ducey and Knapp (2018) relative density equation to calculate the A-line, B-line and C-line in a mixed species stocking chart. 

## Description
Gunn et al. (2019) determined that, due to past silvicultural practices, much of new england forests are comprised of species of low desirability (American beech, balsam fir, grey birch etc.) and what desirable species remained were of unacceptable growing stick (i.e. lacked 8ft saw logs). This determination lead the group to build a degradation framework wherein a stand was classifed in one of five categories using a Gingrich stocking chart.


-	Category 1: (least degraded): The stand must remain above the C-line in a Gingrich stocking chart when only including quality timber trees (i.e., straight trees with no defects) from the most desirable species (i.e., those with the greatest economic value).

-	Category 2: The stand must remain above the C-line in a Gingrich stocking chart when quality timber trees of both the most desirable and second most desirable species are included.

-	Category 3: The stand must remain above the C-line in a Gingrich stocking chart when quality timber trees from the first, second, and third most desirable species are included.

-	Category 4: The stand must remain above the C-line in a Gingrich stocking chart while including all trees in the stand regardless of species or quality.

-	Category 5: (most degraded): The stand fails to meet the C-line in a Gingrich stocking chart when all trees, regardless of species and quality, are included.
