
<!-- README.md is generated from README.Rmd. Please edit that file -->

# steamr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/JsLth/steamr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JsLth/steamr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/JsLth/steamr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/JsLth/steamr?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/jslth/steamr/badge)](https://www.codefactor.io/repository/github/jslth/steamr)
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

## Overview

The Steam API is highly inconsistent and uses not only different ways of
authentication but entirely different APIs.

The Steam Web API is Steam’s “documented” API introduced on its official
[developer website](https://steamcommunity.com/dev?l=german).
Unfortunately, only a handful of API methods are documented on the
[Valve community
wiki](https://developer.valvesoftware.com/wiki/Steam_Web_API) and the
[Team Fortress wiki](https://wiki.teamfortress.com/wiki/WebAPI). There
is a considerable number of API methods that simply remain undocumented.
SteamDB founder xPaw provides an automatically generated list of all API
methods in the unofficial [Steam Web API
Documentation](https://steamapi.xpaw.me/).

If that’s not enough, Steam also uses another API internally. This API
(also known as the “storefront” API) is not documented at all, but avid
Steam developers have gathered a sizable amount of information about the
storefront API in the [Internal Steam web API
Documentation](https://github.com/Revadike/InternalSteamWebAPI). The
internal Steam API mirrors some methods from the Web API and also
implements many complementary methods.

In terms of authentication, Steam’s APIs are also very much
inconsistent. Many API methods can be used without providing any kind of
authentication. Then again, some Web API methods require developers to
request an [API key](https://steamcommunity.com/dev/apikey). For more
sensitive information, both Web and Storefront API can even require the
session to be authenticated, i.e. logged in. `steamr` provides ways for
both API key handling and session authentication.

The following table gives an overview of the package functions, the API
they use, whether they need an API key, and whether they need
authentication.

| Topic          | Function                        | API                | Needs.key          | Needs.auth         |
|:---------------|:--------------------------------|:-------------------|:-------------------|:-------------------|
| Apps           | `get_app_list`                  | Web                | :x:                | :x:                |
| Apps           | `get_games_by_ccu`              | Web                | :x:                | :x:                |
| Apps           | `get_most_played_games`         | Web                | :x:                | :x:                |
| Apps           | `get_top_releases`              | Web                | :x:                | :x:                |
| Apps           | `get_apps_in_genre`             | Storefront         | :x:                | :x:                |
| Apps           | `get_apps_in_category`          | Storefront         | :x:                | :x:                |
| Apps           | `get_apps_in_genre`             | Storefront         | :x:                | :x:                |
| Store          | `query`                         | Web                | :x:                | :x:                |
| Store          | `get_items`                     | Web                | :x:                | :x:                |
| Store          | `get_hardware_items`            | Web                | :x:                | :x:                |
| Store          | `search_apps`                   | Storefront         | :x:                | :x:                |
| Store          | `suggest`                       | Storefront         | :x:                | :x:                |
| Store          | `store_search`                  | Storefront         | :x:                | :x:                |
| Store          | `get_weeky_top_sellers`         | Web                | :x:                | :x:                |
| Market         | `query_market_assets`           | Storefront         | :x:                | :x:                |
| Market         | `get_asset_prices`              | Web                | :heavy_check_mark: | :x:                |
| Market         | `get_asset_info`                | Web                | :heavy_check_mark: | :x:                |
| Market         | `get_price_history`             | Storefront         | :x:                | :x:                |
| Details        | `appdetails`                    | Storefront         | :x:                | :x:                |
| Files          | `query_files`                   | Web                | :heavy_check_mark: | :x:                |
| Files          | `get_published_file`            | Web                | :heavy_check_mark: | :x:                |
| Friends        | `get_friend_list`               | Web                | :heavy_check_mark: | :x:                |
| Friends        | `get_friends_playtimes`         | Web                | :heavy_check_mark: | :heavy_check_mark: |
| Friends        | `get_friends_recommendations`   | Web                | :heavy_check_mark: | :heavy_check_mark: |
| Friends        | `get_friends`                   | Storefront         | :x:                | :heavy_check_mark: |
| Friends        | `get_friend_data`               | Storefront         | :x:                | :heavy_check_mark: |
| User           | `get_player_summary`            | Web                | :heavy_check_mark: | :x:                |
| User           | `get_user_group_list`           | Web                | :heavy_check_mark: | :x:                |
| User           | `get_player_bans`               | Web                | :heavy_check_mark: | :x:                |
| User           | `get_owned_games`               | Web                | :heavy_check_mark: | :x:                |
| User           | `get_recently_played_games`     | Web                | :heavy_check_mark: | :x:                |
| User           | `get_game_playtime`             | Web                | :heavy_check_mark: | :heavy_check_mark: |
| User           | `get_last_playtimes`            | Web                | :heavy_check_mark: | :x:                |
| Auth user      | `get_userdata`                  | Storefront         | :x:                | :heavy_check_mark: |
| Auth user      | `get_app_user_details`          | Storefront         | :x:                | :heavy_check_mark: |
| Auth user      | `get_library_stats`             | Storefront         | :x:                | :heavy_check_mark: |
| Auth user      | `get_owned_apps`                | Storefront         | :x:                | :heavy_check_mark: |
| Auth user      | `get_badge`                     | Storefront         | :x:                | :heavy_check_mark: |
| Auth user      | `get_badge_info`                | Storefront         | :x:                | :heavy_check_mark: |
| Auth user      | `recommend_apps`                | Storefront         | :x:                | :heavy_check_mark: |
| Profile        | `get_profile_items`             | Web                | :heavy_check_mark: | :heavy_check_mark: |
| Profile        | `get_profile_reactions`         | Web                | :x:                | :x:                |
| Profile        | `get_profile_avatar`            | Web                | :x:                | :x:                |
| Profile        | `get_profile_frame`             | Web                | :x:                | :x:                |
| Profile        | `get_profile_background`        | Web                | :x:                | :x:                |
| Profile        | `get_profile_mini_background`   | Web                | :x:                | :x:                |
| Profile        | `get_profile_badges`            | Web                | :heavy_check_mark: | :x:                |
| Profile        | `get_equipped_profile_items`    | Web                | :x:                | :x:                |
| Profile        | `get_profile_themes`            | Web                | :heavy_check_mark: | :heavy_check_mark: |
| Profile        | `get_profile_customization`     | Web                | :x:                | :x:                |
| Level          | `get_steam_level`               | Web                | :heavy_check_mark: | :x:                |
| Level          | `get_level_percentile`          | Web                | :heavy_check_mark: | :x:                |
| Reviews        | `get_app_reviews`               | Storefront         | :x:                | :x:                |
| Reviews        | `get_app_review`                | Web                | :heavy_check_mark: | :x:                |
| Reviews        | `get_review_histogram`          | Storefront         | :x:                | :x:                |
| News           | `get_news`                      | Web                | :x:                | :x:                |
| Stats          | `get_game_schema`               | Web                | :heavy_check_mark: | :x:                |
| Stats          | `get_game_stats`                | Web                | :x:                | :x:                |
| Stats          | `get_user_stats_for_game`       | Web                | :heavy_check_mark: | :x:                |
| Tags           | `get_tags`                      | Web                | :x:                | :x:                |
| Tags           | `get_most_popular_tags`         | Web                | :x:                | :x:                |
| Tags           | `get_frequent_tags`             | Storefront         | :x:                | :heavy_check_mark: |
| Tags           | `get_recommended_tags`          | Storefront         | :x:                | :heavy_check_mark: |
| Categories     | `get_categories`                | Storefront         | :x:                | :x:                |
| Categories     | `get_genres`                    | Storefront         | :x:                | :x:                |
| Categories     | `get_store_categories`          | Web                | :x:                | :x:                |
| Achievements   | `get_game_achievements`         | Web                | :x:                | :x:                |
| Achievements   | `get_top_achievements`          | Web                | :heavy_check_mark: | :x:                |
| Achievements   | `get_player_achievements`       | Web                | :heavy_check_mark: | :x:                |
| Loyalty        | `query_loyalty_rewards`         | Web                | :x:                | :x:                |
| Loyalty        | `get_loyalty_apps`              | Web                | :x:                | :x:                |
| Events         | `query_events`                  | Storefront         | :x:                | :x:                |
| Events         | `get_best_events`               | Storefront         | :x:                | :heavy_check_mark: |
| Marketing      | `get_active_marketing_messages` | Web                | :x:                | :x:                |
| Year in review | `user_in_review`                | Web                | :x:                | :x:                |
| Year in review | `friends_in_review`             | Web                | :heavy_check_mark: | :heavy_check_mark: |
| Year in review | `achievements_in_review`        | Web                | :x:                | :x:                |
| Year in review | `screenshots_in_review`         | Web                | :x:                | :x:                |
| Discovery      | `get_discovery_queue`           | :x:                | :heavy_check_mark: |                    |
| Discovery      | `get_discovery_settings`        | :x:                | :heavy_check_mark: |                    |
| Discovery      | `get_discovery_skipped`         | :x:                | :heavy_check_mark: |                    |
| Weights        | `weight_app`                    | :heavy_check_mark: | :x:                |                    |
| Location       | `query_locations`               | Storefront         | :x:                | :x:                |
| Location       | `get_country_list`              | Web                | :x:                | :x:                |
| Session        | `get_logon_info`                | Web                | :heavy_check_mark: | :heavy_check_mark: |
| Session        | `get_client_info`               | Web                | :heavy_check_mark: | :heavy_check_mark: |
| Misc           | `html_to_bbcode`                | Web                | :x:                | :x:                |
| Misc           | `search_support`                | Web                | :x:                | :x:                |
| Meta           | `steam_stats`                   | Storefront         | :x:                | :x:                |
| Meta           | `get_supported_api_list`        | Web                | :x:                | :x:                |
| Meta           | `get_servertime`                | Web                | :x:                | :x:                |
| Meta           | `get_servers`                   | Web                | :heavy_check_mark: | :x:                |

## Example

`steamr` implements most important data collection functions from both
the official Web API and the internal storefront API of Steam. Many
functions operate around a so-called application ID or `appid`. One way
to find the appID for a game is to search for it programmatically:

``` r
library(steamr)
apps <- search_apps("team fortress")
appid <- apps$appid
apps
#> # A tibble: 4 × 3
#>   term          appid   name                 
#>   <chr>         <chr>   <chr>                
#> 1 team fortress 440     Team Fortress 2      
#> 2 team fortress 20      Team Fortress Classic
#> 3 team fortress 1551180 TF Visualizer        
#> 4 team fortress 860080  lilGunBois
```

Many functions need an app ID to work, for example the following
function which retrieves user review data from an application:

``` r
get_app_reviews(440)
#> # A tibble: 100 × 22
#>    recommendationid language review      timestamp_created   timestamp_updated  
#>    <chr>            <chr>    <chr>       <dttm>              <dttm>             
#>  1 166847493        english  "It was a … 2024-06-07 07:46:14 2024-06-07 07:46:14
#>  2 167445706        english  "Witnessin… 2024-06-15 22:54:19 2024-06-15 22:54:19
#>  3 167057723        english  "So long a… 2024-06-10 03:41:34 2024-06-10 03:41:34
#>  4 166858627        russian  "#FixTf2"   2024-06-07 13:08:08 2024-06-07 13:08:08
#>  5 167665571        english  "Leaving t… 2024-06-19 03:13:21 2024-06-19 03:13:21
#>  6 168896025        russian  "Игра под … 2024-07-03 18:34:04 2024-07-03 18:34:04
#>  7 166920297        russian  "#FixTF2 \… 2024-06-08 10:22:54 2024-06-08 10:22:54
#>  8 167060501        english  "Grew up w… 2024-06-10 04:44:50 2024-06-10 04:44:50
#>  9 167532816        english  "Valve has… 2024-06-17 02:47:55 2024-06-17 02:47:55
#> 10 166847042        english  "This game… 2024-06-07 07:32:21 2024-06-07 07:32:21
#> # ℹ 90 more rows
#> # ℹ 17 more variables: voted_up <lgl>, votes_up <int>, votes_funny <int>,
#> #   weighted_vote_score <dbl>, comment_count <int>, steam_purchase <lgl>,
#> #   received_for_free <lgl>, written_during_early_access <lgl>,
#> #   hidden_in_steam_china <lgl>, steam_china_location <chr>,
#> #   author.steamid <chr>, author.num_games_owned <int>,
#> #   author.num_reviews <int>, author.playtime_forever <int>, …
```

Steam can also use the official Web API given a valid API key. One
function offered by the Web API allows us to query details on a Steam
application:

``` r
get_items(appid)
#> # A tibble: 4 × 27
#>   item_type      id success visible name     store_url_path  appid  type is_free
#>       <int>   <int>   <int> <lgl>   <chr>    <chr>           <int> <int> <lgl>  
#> 1         0     440       1 TRUE    Team Fo… app/440/Team_… 4.4 e2     0 TRUE   
#> 2         0      20       1 TRUE    Team Fo… app/20/Team_F… 2   e1     0 FALSE  
#> 3         0 1551180       1 TRUE    TF Visu… app/1551180/T… 1.55e6     6 TRUE   
#> 4         0  860080       1 TRUE    lilGunB… app/860080/li… 8.60e5     0 FALSE  
#> # ℹ 18 more variables: content_descriptorids <list>,
#> #   categories.supported_player_categoryids <list>,
#> #   categories.feature_categoryids <list>,
#> #   categories.controller_categoryids <list>,
#> #   best_purchase_option.packageid <int>,
#> #   best_purchase_option.purchase_option_name <chr>,
#> #   best_purchase_option.final_price_in_cents <chr>, …
```
