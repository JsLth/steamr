
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

| Topic | Function | API | Needs key | Needs auth |
|:---|:---|:---|:---|:---|
| Profile | `get_profile_items` | Web API | :heavy_check_mark: | :heavy_check_mark: |
| Profile | `get_profile_frame` | Web API | :x: | :x: |
| Countries | `get_country_list` | Web API | :x: | :x: |
| Discovery | `get_discovery_queue` | Web API | :x: | :heavy_check_mark: |
| Apps | `get_most_played_games` | Web API | :x: | :x: |
| Profile | `get_profile_customization` | Web API | :x: | :x: |
| News | `get_news` | Web API | :x: | :x: |
| Tags | `get_frequent_tags` | Storefront | :x: | :heavy_check_mark: |
| Loyalty | `query_loyalty_rewards` | Web API | :x: | :x: |
| Userlevel | `get_level_percentile` | Web API | :x: | :x: |
| Search | `search_apps` | Storefront | :x: | :x: |
| Profile | `get_profile_background` | Web API | :x: | :x: |
| Profile | `get_profile_badges` | Web API | :x: | :x: |
| User | `get_game_playtime` | Web API | :heavy_check_mark: | :heavy_check_mark: |
| Auth | `get_access_token` | Storefront | :x: | :x: |
| Categories | `get_store_categories` | Web API | :x: | :x: |
| User | `get_user_group_list` | Web API | :heavy_check_mark: | :x: |
| Query | `query_by_recommended_tags` | Web API | :x: | :heavy_check_mark: |
| Bbcode | `html_to_bbcode` | Web API | :x: | :x: |
| User | `get_player_bans` | Web API | :heavy_check_mark: | :x: |
| Auth | `get_steam_qr` | Web API | :x: | :x: |
| Marketing | `get_marketing_message` | Web API | :x: | :x: |
| Yearinreview | `user_in_review` | Web API | :x: | :x: |
| Weights | `weight_app` | Web API | :x: | :x: |
| Achievements | `get_top_achievements` | Web API | :heavy_check_mark: | :x: |
| Details | `appdetails` | Storefront | :x: | :x: |
| Events | `get_event_details` | Storefront | :x: | :x: |
| Auth | `get_password_rsa_public_key` | Web API | :x: | :x: |
| Profile | `get_profile_mini_background` | Web API | :x: | :x: |
| Economy | `get_asset_info` | Web API | :heavy_check_mark: | :x: |
| Authstats | `get_badge` | Storefront | :x: | :heavy_check_mark: |
| Reviews | `get_app_review` | Web API | :x: | :x: |
| Profile | `get_profile_avatar` | Web API | :x: | :x: |
| Reviews | `get_app_reviews` | Storefront | :x: | :x: |
| Browse | `get_items` | Web API | :x: | :x: |
| Profile | `get_profile_themes` | Web API | :heavy_check_mark: | :heavy_check_mark: |
| Yearinreview | `screenshots_in_review` | Web API | :x: | :x: |
| Categories | `get_categories` | Web API | :x: | :x: |
| Gamegroups | `get_game_group` | Storefront | :x: | :x: |
| User | `get_last_playtimes` | Web API | :heavy_check_mark: | :heavy_check_mark: |
| Discovery | `get_discovery_settings` | Web API | :x: | :heavy_check_mark: |
| Authinfo | `get_logon_info` | Web API | :x: | :heavy_check_mark: |
| Stats | `get_game_schema` | Web API | :heavy_check_mark: | :x: |
| Economy | `get_asset_prices` | Web API | :heavy_check_mark: | :x: |
| Tags | `get_most_popular_tags` | Web API | :x: | :x: |
| Topsellers | `get_weekly_top_sellers` | Web API | :x: | :x: |
| Authinfo | `get_client_info` | Web API | :x: | :heavy_check_mark: |
| Search | `store_search` | Storefront | :x: | :x: |
| Reviews | `get_review_histogram` | Storefront | :x: | :x: |
| Yearinreview | `friends_in_review` | Web API | :heavy_check_mark: | :heavy_check_mark: |
| Authstats | `get_userdata` | Storefront | :x: | :heavy_check_mark: |
| Auth | `update_with_mobile_confirmation` | Web API | :x: | :x: |
| Profile | `get_profile_reactions` | Web API | :x: | :x: |
| Authstats | `get_badge_info` | Storefront | :x: | :heavy_check_mark: |
| Yearinreview | `achievements_in_review` | Web API | :x: | :x: |
| Api | `steam_stats` | Storefront | :x: | :x: |
| Wishlist | `get_wishlist` | Storefront | :x: | :x: |
| Bundles | `resolve_packages` | Storefront | :x: | :x: |
| User | `get_owned_games` | Web API | :heavy_check_mark: | :x: |
| User | `get_recently_played_games` | Web API | :heavy_check_mark: | :x: |
| Countries | `query_locations` | Storefront | :x: | :x: |
| Authstats | `get_app_user_details` | Storefront | :x: | :heavy_check_mark: |
| Apps | `get_apps_in_category` | Storefront | :x: | :x: |
| Events | `get_best_events` | Storefront | :x: | :heavy_check_mark: |
| Support | `search_support` | Web API | :x: | :x: |
| Categories | `get_genres` | Web API | :x: | :x: |
| Auth | `poll_auth_session_status` | Web API | :x: | :x: |
| Files | `get_published_file` | Web API | :heavy_check_mark: | :x: |
| Economy | `get_price_history` | Storefront | :x: | :heavy_check_mark: |
| Loyalty | `get_loyalty_apps` | Web API | :x: | :x: |
| Friends | `get_friend_list` | Web API | :heavy_check_mark: | :x: |
| Tags | `get_tags` | Web API | :x: | :x: |
| Api | `get_supported_api_list` | Web API | :x: | :x: |
| Recommender | `recommender` | Storefront | :x: | :heavy_check_mark: |
| Achievements | `get_player_achievements` | Web API | :heavy_check_mark: | :x: |
| Files | `query_files` | Web API | :heavy_check_mark: | :x: |
| Stats | `get_user_stats_for_game` | Web API | :x: | :x: |
| Query | `query` | Web API | :x: | :x: |
| Bundles | `package_details` | Storefront | :x: | :x: |
| Browse | `get_hardware_items` | Web API | :x: | :x: |
| User | `get_player_summary` | Web API | :heavy_check_mark: | :x: |
| Apps | `get_app_list` | Web API | :x: | :x: |
| Apps | `get_games_by_ccu` | Web API | :x: | :x: |
| Apps | `get_apps_in_genre` | Storefront | :x: | :x: |
| Bundles | `resolve_bundles` | Storefront | :x: | :x: |
| Events | `query_events` | Storefront | :x: | :x: |
| Recommender | `get_recommender_input` | Storefront | :x: | :x: |
| Economy | `query_market_assets` | Storefront | :x: | :x: |
| Economy | `get_item_histogram` | Storefront | :x: | :x: |
| Steamid | `resolve_vanity_url` | Web API | :x: | :x: |
| Authstats | `get_owned_apps` | Storefront | :x: | :heavy_check_mark: |
| Apps | `get_top_releases` | Web API | :x: | :x: |
| Friends | `get_friends_recommendations` | Web API | :heavy_check_mark: | :heavy_check_mark: |
| Profile | `get_equipped_profile_items` | Web API | :x: | :x: |
| Discovery | `get_discovery_skipped` | Web API | :x: | :x: |
| Marketing | `get_active_marketing_messages` | Web API | :x: | :x: |
| Api | `get_servertime` | Web API | :x: | :x: |
| Query | `search_suggestions` | Web API | :x: | :x: |
| Achievements | `get_game_achievements` | Web API | :x: | :x: |
| Auth | `update_with_steam_guard_code` | Web API | :x: | :x: |
| Authstats | `recommend_apps` | Storefront | :x: | :heavy_check_mark: |
| Weights | `cluster_apps` | Web API | :x: | :x: |
| Auth | `begin_auth_session` | Web API | :x: | :x: |
| Stats | `get_game_stats` | Web API | :heavy_check_mark: | :x: |
| Authstats | `get_library_stats` | Storefront | :x: | :heavy_check_mark: |
| Tags | `get_recommended_tags` | Storefront | :x: | :heavy_check_mark: |
| Friends | `get_friend_data` | Storefront | :x: | :heavy_check_mark: |
| Search | `suggest` | Storefront | :x: | :x: |
| Friends | `get_friends` | Storefront | :x: | :heavy_check_mark: |
| Api | `get_servers` | Web API | :heavy_check_mark: | :x: |
| Userlevel | `get_steam_level` | Web API | :x: | :x: |
| Friends | `get_friends_playtimes` | Web API | :heavy_check_mark: | :heavy_check_mark: |

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
#> # A tibble: 20 × 21
#>    recommendationid language review      timestamp_created   timestamp_updated  
#>    <chr>            <chr>    <chr>       <dttm>              <dttm>             
#>  1 178996847        english  "I'm an ol… 2024-11-14 13:37:31 2024-11-14 13:37:31
#>  2 179339672        english  "4 thousan… 2024-11-19 03:24:53 2024-11-19 03:24:53
#>  3 181004269        thai     "เป็นเกมฟรีที่… 2024-11-30 10:39:04 2024-11-30 10:39:04
#>  4 179371609        schinese "跟守望先…  2024-11-19 16:34:27 2024-11-19 16:34:27
#>  5 180315208        english  "It is a v… 2024-11-28 09:32:11 2024-11-28 09:32:11
#>  6 182262294        english  "[h1][i]Re… 2024-12-08 16:46:49 2024-12-08 16:46:49
#>  7 181973452        english  "The great… 2024-12-05 19:18:24 2024-12-05 19:18:24
#>  8 181897450        polish   "Najlepsza… 2024-12-04 19:23:42 2024-12-04 19:23:42
#>  9 181669000        russian  "Достойная… 2024-12-03 14:49:41 2024-12-03 14:49:41
#> 10 180697600        russian  "Игра прос… 2024-11-29 08:15:47 2024-11-29 08:15:47
#> 11 180678859        russian  "Эта игра … 2024-11-29 06:07:48 2024-11-29 06:07:48
#> 12 180427056        russian  "Эта игра … 2024-11-28 14:35:28 2024-11-28 14:35:28
#> 13 181045888        english  "It's a ni… 2024-11-30 14:09:09 2024-11-30 14:09:09
#> 14 181010775        russian  "Накаченны… 2024-11-30 11:13:51 2024-11-30 11:13:51
#> 15 179991171        polish   "Grając od… 2024-11-27 20:08:00 2024-11-27 20:08:00
#> 16 178918642        spanish  "me costo,… 2024-11-13 03:01:27 2024-11-13 03:01:27
#> 17 182297119        english  "watch sol… 2024-12-09 00:05:17 2024-12-09 00:05:17
#> 18 182255791        russian  "The game … 2024-12-08 15:33:03 2024-12-08 15:33:03
#> 19 182236676        english  "i acciden… 2024-12-08 11:14:13 2024-12-08 11:14:13
#> 20 182124740        english  "One of th… 2024-12-07 10:05:38 2024-12-07 10:05:38
#> # ℹ 16 more variables: voted_up <lgl>, votes_up <int>, votes_funny <int>,
#> #   weighted_vote_score <dbl>, comment_count <int>, steam_purchase <lgl>,
#> #   received_for_free <lgl>, written_during_early_access <lgl>,
#> #   primarily_steam_deck <lgl>, author.steamid <chr>,
#> #   author.num_games_owned <int>, author.num_reviews <int>,
#> #   author.playtime_forever <int>, author.playtime_last_two_weeks <int>,
#> #   author.playtime_at_review <int>, author.last_played <dttm>
```

Steam can also use the official Web API given a valid API key. One
function offered by the Web API allows us to query details on a Steam
application:

``` r
get_items(appid)
#> # A tibble: 4 × 24
#>   item_type      id success visible name     store_url_path  appid  type is_free
#>       <int>   <int>   <int> <lgl>   <chr>    <chr>           <int> <int> <lgl>  
#> 1         0     440       1 TRUE    Team Fo… app/440/Team_… 4.4 e2     0 TRUE   
#> 2         0      20       1 TRUE    Team Fo… app/20/Team_F… 2   e1     0 FALSE  
#> 3         0 1551180       1 TRUE    TF Visu… app/1551180/T… 1.55e6     6 TRUE   
#> 4         0  860080       1 TRUE    lilGunB… app/860080/li… 8.60e5     0 FALSE  
#> # ℹ 15 more variables: content_descriptorids <list>,
#> #   categories.supported_player_categoryids <list>,
#> #   categories.feature_categoryids <list>,
#> #   categories.controller_categoryids <list>,
#> #   best_purchase_option.packageid <int>,
#> #   best_purchase_option.purchase_option_name <chr>,
#> #   best_purchase_option.final_price_in_cents <chr>, …
```
