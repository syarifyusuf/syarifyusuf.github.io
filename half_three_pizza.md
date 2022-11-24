Half Three Pizza
================
Syarif Yusuf Effendi

# Summary

A year’s worth of sales from a fictitious pizza place, including the
date and time of each order and the pizzas served, with additional
details on the type, size, quantity, price, and ingredients.

# ASK

#### Key objective:

- How many customers do we have each day? Are there any peak hours?
- How many pizzas are typically in an order? Do we have any bestsellers?
- How much money did we make this year? Can we identify any seasonality
  in the sales?
- Are there any pizzas we should take of the menu, or any promotions we
  could leverage?

# PREPARE

#### Key objective:

1.  Determine the credibility of data:

- This data is obtained from the [Maven
  Analytics](https://www.mavenanalytics.io/data-playground) page. The
  data contains several tables related to sales, types, and other
  information. Each table has a relationship with each other

2.  Join the whole table into a dataset
3.  Install and load necessary packages:

- `tidyverse`
- `lubridate`
- `dplyr`

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
```

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

4.  Import data to RStudio

``` r
order_details <- read_csv("D:/Dataset/Pizza Sales/order_details.csv")
```

    ## Rows: 48620 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): pizza_id
    ## dbl (3): order_details_id, order_id, quantity
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
orders <- read_csv("D:/Dataset/Pizza Sales/orders.csv")
```

    ## Rows: 21350 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (1): order_id
    ## date (1): date
    ## time (1): time
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pizzas <- read_csv("D:/Dataset/Pizza Sales/pizzas.csv")
```

    ## Rows: 96 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): pizza_id, pizza_type_id, size
    ## dbl (1): price
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pizza_types <- read_csv("D:/Dataset/Pizza Sales/pizza_types.csv")
```

    ## Rows: 32 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): pizza_type_id, name, category, ingredients
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

5.  Merge all tables

``` r
pizza.order <- inner_join(order_details, orders, by="order_id")
pizza.type <- inner_join(pizza_types, pizzas, by="pizza_type_id")
pizza.dataset <- inner_join(pizza.order, pizza.type, by="pizza_id")
```

``` r
head(pizza.dataset)
```

    ## # A tibble: 6 × 12
    ##   order_deta…¹ order…² pizza…³ quant…⁴ date       time     pizza…⁵ name  categ…⁶
    ##          <dbl>   <dbl> <chr>     <dbl> <date>     <time>   <chr>   <chr> <chr>  
    ## 1            1       1 hawaii…       1 2015-01-01 11:38:36 hawaii… The … Classic
    ## 2            2       2 classi…       1 2015-01-01 11:57:40 classi… The … Classic
    ## 3            3       2 five_c…       1 2015-01-01 11:57:40 five_c… The … Veggie 
    ## 4            4       2 ital_s…       1 2015-01-01 11:57:40 ital_s… The … Supreme
    ## 5            5       2 mexica…       1 2015-01-01 11:57:40 mexica… The … Veggie 
    ## 6            6       2 thai_c…       1 2015-01-01 11:57:40 thai_c… The … Chicken
    ## # … with 3 more variables: ingredients <chr>, size <chr>, price <dbl>, and
    ## #   abbreviated variable names ¹​order_details_id, ²​order_id, ³​pizza_id,
    ## #   ⁴​quantity, ⁵​pizza_type_id, ⁶​category

# Process

#### Key objective:

1.  Clean and prepare the data for analysis

- Now that all the data is in one location, we can begin to remove any
  mistakes like NA. In order to speed up our research and produce more
  insightful results, we will also make some adjustments to the data,
  adding relevant new columns based on calculations of previously
  existing columns

``` r
glimpse(pizza.dataset)
```

    ## Rows: 48,620
    ## Columns: 12
    ## $ order_details_id <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16…
    ## $ order_id         <dbl> 1, 2, 2, 2, 2, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 9, 9,…
    ## $ pizza_id         <chr> "hawaiian_m", "classic_dlx_m", "five_cheese_l", "ital…
    ## $ quantity         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ date             <date> 2015-01-01, 2015-01-01, 2015-01-01, 2015-01-01, 2015…
    ## $ time             <time> 11:38:36, 11:57:40, 11:57:40, 11:57:40, 11:57:40, 11…
    ## $ pizza_type_id    <chr> "hawaiian", "classic_dlx", "five_cheese", "ital_supr"…
    ## $ name             <chr> "The Hawaiian Pizza", "The Classic Deluxe Pizza", "Th…
    ## $ category         <chr> "Classic", "Classic", "Veggie", "Supreme", "Veggie", …
    ## $ ingredients      <chr> "Sliced Ham, Pineapple, Mozzarella Cheese", "Pepperon…
    ## $ size             <chr> "M", "M", "L", "L", "M", "L", "M", "L", "M", "M", "S"…
    ## $ price            <dbl> 13.25, 16.00, 18.50, 20.75, 16.00, 20.75, 16.50, 20.7…

``` r
summary(pizza.dataset)
```

    ##  order_details_id    order_id       pizza_id            quantity   
    ##  Min.   :    1    Min.   :    1   Length:48620       Min.   :1.00  
    ##  1st Qu.:12156    1st Qu.: 5337   Class :character   1st Qu.:1.00  
    ##  Median :24311    Median :10682   Mode  :character   Median :1.00  
    ##  Mean   :24311    Mean   :10701                      Mean   :1.02  
    ##  3rd Qu.:36465    3rd Qu.:16100                      3rd Qu.:1.00  
    ##  Max.   :48620    Max.   :21350                      Max.   :4.00  
    ##       date                time          pizza_type_id          name          
    ##  Min.   :2015-01-01   Length:48620      Length:48620       Length:48620      
    ##  1st Qu.:2015-03-31   Class1:hms        Class :character   Class :character  
    ##  Median :2015-06-28   Class2:difftime   Mode  :character   Mode  :character  
    ##  Mean   :2015-06-29   Mode  :numeric                                         
    ##  3rd Qu.:2015-09-28                                                          
    ##  Max.   :2015-12-31                                                          
    ##    category         ingredients            size               price      
    ##  Length:48620       Length:48620       Length:48620       Min.   : 9.75  
    ##  Class :character   Class :character   Class :character   1st Qu.:12.75  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :16.50  
    ##                                                           Mean   :16.49  
    ##                                                           3rd Qu.:20.25  
    ##                                                           Max.   :35.95

#### Create new columns

``` r
pizza.dataset$weekday <- weekdays(pizza.dataset$date)
pizza.dataset$month <- month(pizza.dataset$date)
pizza.dataset$hour <- hour(pizza.dataset$time)
pizza.dataset$revenue <- pizza.dataset$quantity * pizza.dataset$price
```

``` r
glimpse(pizza.dataset)
```

    ## Rows: 48,620
    ## Columns: 16
    ## $ order_details_id <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16…
    ## $ order_id         <dbl> 1, 2, 2, 2, 2, 2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 9, 9,…
    ## $ pizza_id         <chr> "hawaiian_m", "classic_dlx_m", "five_cheese_l", "ital…
    ## $ quantity         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ date             <date> 2015-01-01, 2015-01-01, 2015-01-01, 2015-01-01, 2015…
    ## $ time             <time> 11:38:36, 11:57:40, 11:57:40, 11:57:40, 11:57:40, 11…
    ## $ pizza_type_id    <chr> "hawaiian", "classic_dlx", "five_cheese", "ital_supr"…
    ## $ name             <chr> "The Hawaiian Pizza", "The Classic Deluxe Pizza", "Th…
    ## $ category         <chr> "Classic", "Classic", "Veggie", "Supreme", "Veggie", …
    ## $ ingredients      <chr> "Sliced Ham, Pineapple, Mozzarella Cheese", "Pepperon…
    ## $ size             <chr> "M", "M", "L", "L", "M", "L", "M", "L", "M", "M", "S"…
    ## $ price            <dbl> 13.25, 16.00, 18.50, 20.75, 16.00, 20.75, 16.50, 20.7…
    ## $ weekday          <chr> "Thursday", "Thursday", "Thursday", "Thursday", "Thur…
    ## $ month            <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ hour             <int> 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 1…
    ## $ revenue          <dbl> 13.25, 16.00, 18.50, 20.75, 16.00, 20.75, 16.50, 20.7…

``` r
summary(pizza.dataset)
```

    ##  order_details_id    order_id       pizza_id            quantity   
    ##  Min.   :    1    Min.   :    1   Length:48620       Min.   :1.00  
    ##  1st Qu.:12156    1st Qu.: 5337   Class :character   1st Qu.:1.00  
    ##  Median :24311    Median :10682   Mode  :character   Median :1.00  
    ##  Mean   :24311    Mean   :10701                      Mean   :1.02  
    ##  3rd Qu.:36465    3rd Qu.:16100                      3rd Qu.:1.00  
    ##  Max.   :48620    Max.   :21350                      Max.   :4.00  
    ##       date                time          pizza_type_id          name          
    ##  Min.   :2015-01-01   Length:48620      Length:48620       Length:48620      
    ##  1st Qu.:2015-03-31   Class1:hms        Class :character   Class :character  
    ##  Median :2015-06-28   Class2:difftime   Mode  :character   Mode  :character  
    ##  Mean   :2015-06-29   Mode  :numeric                                         
    ##  3rd Qu.:2015-09-28                                                          
    ##  Max.   :2015-12-31                                                          
    ##    category         ingredients            size               price      
    ##  Length:48620       Length:48620       Length:48620       Min.   : 9.75  
    ##  Class :character   Class :character   Class :character   1st Qu.:12.75  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :16.50  
    ##                                                           Mean   :16.49  
    ##                                                           3rd Qu.:20.25  
    ##                                                           Max.   :35.95  
    ##    weekday              month             hour          revenue     
    ##  Length:48620       Min.   : 1.000   Min.   : 9.00   Min.   : 9.75  
    ##  Class :character   1st Qu.: 3.000   1st Qu.:13.00   1st Qu.:12.75  
    ##  Mode  :character   Median : 6.000   Median :16.00   Median :16.50  
    ##                     Mean   : 6.451   Mean   :15.91   Mean   :16.82  
    ##                     3rd Qu.: 9.000   3rd Qu.:18.00   3rd Qu.:20.50  
    ##                     Max.   :12.000   Max.   :23.00   Max.   :83.00

# ANALYZE

We now have completed data frame with all of the information we need and
then export the data frame into a csv file and perform analysis and
visualization through Tableau Public

``` r
write.csv(pizza.dataset, "D:/Dataset/Pizza Sales/pizza.dataset.csv", row.names = FALSE)
```

Click
[here](https://public.tableau.com/views/PizzaDashboard_16691223876820/SalesDashboard?:language=en-US&:display_count=n&:origin=viz_share_link)
to see the result of my Tableau Dashboard

# SHARE

#### Conclusion

1.  The average daily customers are 60 customers. There is a peak hours
    every lunchtime on weekdays, and dinnertime on weekends.
2.  Up to 67% of customers placed orders for 1-2 pizzas, and the
    remaining clients placed orders for 3 or more pizzas. A popular
    pizza is the Big Meat Pizza in small portions.
3.  The restaurant made \$817.8K in sales for the entire year. With Q2
    having the highest average income.
4.  The Greek Pizza size XXL needs to be removed from the menu and an
    additional Brie Carre Pizza size option added.

#### Recommendation

1.  Lunchtime on weekdays and dinnertime on weekends are peak sales
    periods. At these situations, the staff must prioritize preparation.
2.  Alternate Operational Hours: 11.00 - 21.59. Potential to reduce the
    weight of variable costs while just 3%, or \$24.3K, of annual
    revenue is lost.
3.  Increasing the price of the top five pizzas or increasing the pizza
    size to XL on pizzas with a maximum size of L can be selected to
    boost revenue.
4.  The Brie Carre pizza has the second greatest revenue in the small
    pizza group. It is important to provide a larger size option and
    offer a discount to promote the new size.
5.  The Greek pizza sells better at the XL size, so the XXL version can
    be removed from the menu. The most popular pizzas can be made in XXL
    size as a replacement.
6.  n raise the quantity of daily pizza sales to boost revenue in the
    upcoming year. Giving discounts on some items, particularly at
    breakfast and at midnight, might increase daily sales
