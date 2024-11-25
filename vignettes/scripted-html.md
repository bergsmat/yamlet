---
title: "Scripted HTML"
author: "Tim Bergsma"
date: "2024-11-25"
output: 
  html_document:
    keep_md: true
    toc: FALSE
vignette: >
  %\VignetteIndexEntry{Scripted HTML}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  chunk_output_type: console
---

The point of this exercise is to demonstrate flexible rendering of 
subscripts and superscripts.  We want to write expressions for 
column labels and units that are fairly readable as they are, and
yet can be easily rendered with equivalent results in 
plotmath, html, or pdf.

First we load some packages.




``` r
library(magrittr)
library(ggplot2)
library(tablet)
library(yamlet)
library(dplyr)
library(kableExtra)
```

We create some example data.


``` r
x <- data.frame(
  time = 1:10, 
  work = (1:10)^1.5, 
  group = 1:2, 
  set = c(rep('delta',5), rep('gamma', 5))
)
x %<>% decorate('
 time: [ Time_cum.^alpha, h ]
 work: [ Work_total_obs\\n, kg*m^2/s^2 ]
 group: [ Group, [ Second\\nGroup^\\*: 2, First\\nGroup^#: 1 ]]
 set: [ Set, [ gamma, delta ]]
')
x %>% decorations
```

```
## - time
##  - label: Time_cum.^alpha
##  - guide: h
## - work
##  - label: Work_total_obs\n
##  - guide: kg*m^2/s^2
## - group
##  - label: Group
##  - guide
##   - Second\nGroup^\*: 2
##   - First\nGroup^#: 1
## - set
##  - label: Set
##  - guide: gamma, delta
```

The label for column ```work``` has nested subscripts suggesting
$\sf{Work_{total_{obs}}}$. The label for column ```time``` suggests
$\sf{Time_{cum}{}^{\alpha}}$.  The dot closes the subscript to distinguish
this from $\sf{Time_{cum^{\alpha}}}$. Backslash-n requests a line break.


How does this look when we plot it?

``` r
x %>% 
  resolve %>% 
  ggplot(aes(time, work, color = group, shape = set)) + 
  geom_point()
```

<img src="C:/project/devel/yamlet/vignettes/scripted-html_files/figure-html/unnamed-chunk-4-1.png" width="50%" />

By default, we get verbatim labels and units as substitutes for column names.

Next, we use ```enscript()``` instead of ```resolve()``` to indicate 
that the labels should be understood as
potentially having subscripts and superscripts.
For this to work well, units should be constructed
using *, /, and ^ (even though the "units"
package supports other encodings).


``` r
x %>% 
  enscript %>% 
  ggplot(aes(time, work, color = group, shape = set)) + 
  geom_point()
```

<img src="C:/project/devel/yamlet/vignettes/scripted-html_files/figure-html/unnamed-chunk-5-1.png" width="50%" />

In the background, ```enscript()``` is writing __expression__ and __plotmath__ attributes
(consumed by ```ggplot()``` ) and __title__ attributes (consumed by ```tablet()``` ).
We illustrate the latter.


``` r
x %>% 
  enscript %>% 
  group_by(group, set) %>%
  tablet %>%
  as_kable
```

<table>
 <thead>
<tr>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Second<br>Group<sup> *</sup>
</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">First<br>Group<sup>#</sup>
</div></th>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> γ<br>(N = 3) </th>
   <th style="text-align:left;"> δ<br>(N = 2) </th>
   <th style="text-align:left;"> γ<br>(N = 2) </th>
   <th style="text-align:left;"> δ<br>(N = 3) </th>
   <th style="text-align:left;"> All<br>(N = 10) </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="2"><td colspan="6" style="border-bottom: 1px solid;"><strong>Time<sub>cum</sub><sup>α</sup> (h)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:left;"> 8 (2) </td>
   <td style="text-align:left;"> 3 (1.41) </td>
   <td style="text-align:left;"> 8 (1.41) </td>
   <td style="text-align:left;"> 3 (2) </td>
   <td style="text-align:left;"> 5.5 (3.03) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:left;"> 8 (6, 10) </td>
   <td style="text-align:left;"> 3 (2, 4) </td>
   <td style="text-align:left;"> 8 (7, 9) </td>
   <td style="text-align:left;"> 3 (1, 5) </td>
   <td style="text-align:left;"> 5.5 (1, 10) </td>
  </tr>
  <tr grouplength="2"><td colspan="6" style="border-bottom: 1px solid;"><strong>Work<sub>total<sub>obs<br></sub></sub> (kg·m<sup>2</sup>/s<sup>2</sup>)</strong></td></tr>
<tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Mean (SD) </td>
   <td style="text-align:left;"> 23 (8.47) </td>
   <td style="text-align:left;"> 5.41 (3.66) </td>
   <td style="text-align:left;"> 22.8 (6) </td>
   <td style="text-align:left;"> 5.79 (5.12) </td>
   <td style="text-align:left;"> 14.3 (10.5) </td>
  </tr>
  <tr>
   <td style="text-align:left;padding-left: 2em;" indentlevel="1"> Median (range) </td>
   <td style="text-align:left;"> 22.6 (14.7, 31.6) </td>
   <td style="text-align:left;"> 5.41 (2.83, 8) </td>
   <td style="text-align:left;"> 22.8 (18.5, 27) </td>
   <td style="text-align:left;"> 5.2 (1, 11.2) </td>
   <td style="text-align:left;"> 12.9 (1, 31.6) </td>
  </tr>
</tbody>
</table>


In summary, we have decorated our data with labels and 
units containing markup for subscripts and superscripts.
If everything goes well, these render similarly 
in figures and tables.  They also render similarly in
html and pdf.  Please see the pdf version of this document.

