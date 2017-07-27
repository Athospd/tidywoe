# tidywoe <img src="tidywoe_hex.png" align="right" />

Package to put WoE versions of your variables in a tidy way.



# Installation

Currently is available only on Github. To install it, run:


```r
# install.packages("devtools")
devtools::install_github("athospd/tidywoe")
```

# Using tidywoe 

There is tow functions that matter:

- `add_woe()`
- `woe_dictionary()`

## add_woe()

To simply add woe version of your variables from your sample data, just run the follow:


```r
library(dplyr)
library(tidywoe)

# install.packages("FactoMineR")
data(tea, package = "FactoMineR")

tea %>%
  select(breakfast, how, where, price) %>% 
  add_woe(breakfast) %>%
  head
```



|breakfast     |how     |where       |price           |    how_woe|  where_woe|  price_woe|
|:-------------|:-------|:-----------|:---------------|----------:|----------:|----------:|
|breakfast     |tea bag |chain store |p_unknown       | -0.0377403| -0.0451204| -0.2564295|
|breakfast     |tea bag |chain store |p_variable      | -0.0377403| -0.0451204|  0.1872882|
|Not.breakfast |tea bag |chain store |p_variable      | -0.0377403| -0.0451204|  0.1872882|
|Not.breakfast |tea bag |chain store |p_variable      | -0.0377403| -0.0451204|  0.1872882|
|breakfast     |tea bag |chain store |p_variable      | -0.0377403| -0.0451204|  0.1872882|
|Not.breakfast |tea bag |chain store |p_private label | -0.0377403| -0.0451204| -0.0152675|

## woe_dictionary()

The function `add_woe()` uses a dictionary to map the categories with the respective woe value of each explanatory variable.


```r
tea %>%
  select(breakfast, how, where, price) %>% 
  woe_dictionary(breakfast) %>%
  head
```



|variable |explanatory          | n_tot| n_breakfast| n_Not.breakfast| p_breakfast| p_Not.breakfast|        woe|
|:--------|:--------------------|-----:|-----------:|---------------:|-----------:|---------------:|----------:|
|how      |tea bag              |   170|          80|              90|   0.5555556|       0.5769231| -0.0377403|
|how      |tea bag+unpackaged   |    94|          50|              44|   0.3472222|       0.2820513|  0.2078761|
|how      |unpackaged           |    36|          14|              22|   0.0972222|       0.1410256| -0.3719424|
|where    |chain store          |   192|          90|             102|   0.6250000|       0.6538462| -0.0451204|
|where    |chain store+tea shop |    78|          42|              36|   0.2916667|       0.2307692|  0.2341934|
|where    |tea shop             |    30|          12|              18|   0.0833333|       0.1153846| -0.3254224|

## Using a custom woe_dictionary

One might want to tweak some woe values to fix the order of the effects of a given variable. That's the why `add_woe()` can receive a custom dictionary through the `.woe_dictionary` argument.

The easy way to do this is to make a previous dictionary with the `woe_dictionary()` as the start point and tweak it to achieve the fixes needed. See below:


```r
# biuld a initial dictionary
tea_woe_dic <- tea %>%
  select(breakfast, how, where, price) %>% 
  woe_dictionary(breakfast)

# tweak it a little bit
tea_woe_dic_tweaked <- tea_woe_dic %>% mutate(woe = if_else(variable == "price" & explanatory == "p_unknown", 0, woe))

# pass it as an argument of add_woe()
tea %>%
  select(breakfast, how, where, price) %>% 
  add_woe(breakfast, .woe_dictionary = tea_woe_dic_tweaked) %>%
  head
```



|breakfast     |how     |where       |price           |    how_woe|  where_woe|  price_woe|
|:-------------|:-------|:-----------|:---------------|----------:|----------:|----------:|
|breakfast     |tea bag |chain store |p_unknown       | -0.0377403| -0.0451204|  0.0000000|
|breakfast     |tea bag |chain store |p_variable      | -0.0377403| -0.0451204|  0.1872882|
|Not.breakfast |tea bag |chain store |p_variable      | -0.0377403| -0.0451204|  0.1872882|
|Not.breakfast |tea bag |chain store |p_variable      | -0.0377403| -0.0451204|  0.1872882|
|breakfast     |tea bag |chain store |p_variable      | -0.0377403| -0.0451204|  0.1872882|
|Not.breakfast |tea bag |chain store |p_private label | -0.0377403| -0.0451204| -0.0152675|
