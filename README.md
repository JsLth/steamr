
<!-- README.md is generated from README.Rmd. Please edit that file -->

# steamr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`steamr` is an R interface to the official [Web
API](https://steamcommunity.com/dev) of the Steam game store. The
package also provides access to Steam’s [undocumented storefront
API](https://github.com/Revadike/InternalSteamWebAPI). The package aims
to provide a way to easily perform data collection based on Steam data.
It thus only implements API methods are can be used to retrieve data
sets for data analysis.

## Installation

You can install the development version of steamr like so:

``` r
pak::install("jslth/steamr")
```

## Example

`steamr` implements most important data collection functions from both
the official Web API and the storefront API of Steam. Many functions
operate around a so-called application ID or `appid`. One way to find
the app ID for a game is to search for it programmatically:

``` r
library(steamr)
apps <- search_apps("team fortress")
appid <- apps$appid
apps
#> # A tibble: 4 × 2
#>   appid   name                 
#>   <chr>   <chr>                
#> 1 440     Team Fortress 2      
#> 2 20      Team Fortress Classic
#> 3 1551180 TF Visualizer        
#> 4 860080  lilGunBois
```

Many functions need an app ID to work, for example the following
function which retrieves user review data from an application:

``` r
get_app_reviews(appid[1])
#> # A tibble: 20 × 22
#>    recommendationid language review      timestamp_created   timestamp_updated  
#>    <chr>            <chr>    <chr>       <dttm>              <dttm>             
#>  1 163194606        english  "This is t… 2024-04-19 23:59:16 2024-04-21 16:42:49
#>  2 164958145        english  "⠄⠄⠄⠄⠄⠄⠄⠄⠄… 2024-05-09 02:54:22 2024-05-09 02:54:22
#>  3 163137168        spanish  "Pootis"    2024-04-19 03:00:32 2024-04-19 03:00:32
#>  4 163606488        russian  "Злой Укра… 2024-04-26 09:55:26 2024-04-26 09:55:26
#>  5 163518138        french   "better th… 2024-04-24 21:08:07 2024-04-24 21:08:07
#>  6 163476377        english  "There is … 2024-04-24 03:49:47 2024-05-09 03:05:13
#>  7 164126486        english  "Team fort… 2024-05-03 14:23:14 2024-05-03 14:23:14
#>  8 163882648        spanish  ":>"        2024-04-30 04:55:27 2024-04-30 04:55:27
#>  9 162913647        latam    "Bots scri… 2024-04-15 07:39:58 2024-04-15 07:39:58
#> 10 163406476        turkish  "I hate th… 2024-04-22 23:58:24 2024-04-22 23:58:24
#> 11 165050195        english  "this game… 2024-05-10 11:42:29 2024-05-10 11:42:29
#> 12 162990766        koreana  "내 대학 …  2024-04-16 17:09:57 2024-04-16 17:10:14
#> 13 165013937        english  "Best shit… 2024-05-09 22:07:00 2024-05-09 22:07:00
#> 14 163485360        english  "this game… 2024-04-24 08:25:09 2024-04-24 08:25:09
#> 15 163250342        russian  "ого"       2024-04-20 18:49:00 2024-04-20 18:49:00
#> 16 163019223        english  "spent too… 2024-04-17 03:21:56 2024-04-17 03:21:56
#> 17 164015287        russian  "Хорошо бы… 2024-05-01 22:34:41 2024-05-01 22:34:41
#> 18 163993700        english  "It is abs… 2024-05-01 17:05:14 2024-05-01 17:05:14
#> 19 164032513        english  "official … 2024-05-02 05:03:08 2024-05-02 05:03:08
#> 20 164018124        english  "Everyone … 2024-05-01 23:27:25 2024-05-01 23:27:25
#> # ℹ 17 more variables: voted_up <lgl>, votes_up <int>, votes_funny <int>,
#> #   weighted_vote_score <dbl>, comment_count <int>, steam_purchase <lgl>,
#> #   received_for_free <lgl>, written_during_early_access <lgl>,
#> #   hidden_in_steam_china <lgl>, steam_china_location <chr>,
#> #   author.steamid <chr>, author.num_games_owned <int>,
#> #   author.num_reviews <int>, author.playtime_forever <int>,
#> #   author.playtime_last_two_weeks <int>, author.playtime_at_review <int>, …
```

Steam can also use the official Web API given a valid API key. One
function offered by the Web API allows us to query details on a Steam
application:

``` r
get_items(appid)
#> # A tibble: 4 × 21
#>   item_type      id success visible name     store_url_path  appid  type is_free
#>       <int>   <int>   <int> <lgl>   <chr>    <chr>           <int> <int> <lgl>  
#> 1         0     440       1 TRUE    Team Fo… app/440/Team_… 4.4 e2     0 TRUE   
#> 2         0      20       1 TRUE    Team Fo… app/20/Team_F… 2   e1     0 FALSE  
#> 3         0 1551180       1 TRUE    TF Visu… app/1551180/T… 1.55e6     6 TRUE   
#> 4         0  860080       1 TRUE    lilGunB… app/860080/li… 8.60e5     0 FALSE  
#> # ℹ 12 more variables: content_descriptorids <list>,
#> #   categories.supported_player_categoryids <list>,
#> #   categories.feature_categoryids <list>,
#> #   categories.controller_categoryids <list>,
#> #   best_purchase_option.packageid <int>,
#> #   best_purchase_option.purchase_option_name <chr>,
#> #   best_purchase_option.final_price_in_cents <chr>, …
```
