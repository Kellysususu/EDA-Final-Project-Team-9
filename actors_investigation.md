Actors Investigation
================
Henry Savich

In this notebook we are going to look at data from the IMDB movie
dataset sourced from kaggle to determine if there is a relationship
between actors/actresses and movie ratings

``` r
suppressPackageStartupMessages(library(tidyverse)) #For convenient data frame manipulation
suppressPackageStartupMessages(library(ggplot2)) #For visualizing the data
```

IMDb title\_principals lists every occurrence of an actor/actress in a
movie

``` r
tp = read_csv("data/IMDb\ title_principals.csv") %>%
  #the dataframe also includes writers, directors, cinmetographers etc., so
  #we'll filter to only get actors/actresses
  filter(category == "actor" | category == "actress") %>%
  select(imdb_title_id, imdb_name_id, category, characters)
```

    ## Rows: 835513 Columns: 6

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): imdb_title_id, imdb_name_id, category, job, characters
    ## dbl (1): ordering

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
length(unique(tp$imdb_name_id))
```

    ## [1] 132123

Our dataset contains 132,123 actors / actresses. However, since we only
want to look at English movies, we will need to add that information
from another csv in order to filter.

``` r
mv = read_csv("data/IMDb\ movies.csv") %>%
  select(imdb_title_id, language)
```

    ## Rows: 85855 Columns: 22

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (15): imdb_title_id, title, original_title, date_published, genre, count...
    ## dbl  (7): year, duration, avg_vote, votes, metascore, reviews_from_users, re...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tp2 = left_join(tp, mv, by = "imdb_title_id") %>%
  filter(str_detect(language, "English"))

length(unique(tp2$imdb_name_id))
```

    ## [1] 74663

So out of the 132,123 actors/actresses in the database, 74,663 have
acted in English language movies (56.5% of actors/actresses).

Let’s now look at how many movies these actors/actresses were in.

``` r
freq_df = count(tp2, imdb_name_id)

max(freq_df$n)
```

    ## [1] 132

``` r
mean(freq_df$n)
```

    ## [1] 2.584399

``` r
median(freq_df$n)
```

    ## [1] 1

``` r
ggplot(freq_df, aes(x = n)) +
  geom_histogram(bins = 100)
```

![](actors_investigation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

From this we learn:

-The actor/actress with the most English-language titles on IMDB has 132
(John Wayne) -The mean number of English-language titles is 2.58 -The
median number of titles is 1, which means most of our actors/actresses
only appear in one title

Because we want to see distributions of ratings of movies for an
individual actor, it doesn’t make sense to try and gather information
from an actor with only 1 movie, and for a small enough number of
movies, the distribution of ratings won’t really reflect the
actor/actress, so we want to establish some cutoff number of movies.
Let’s look at how many actors/actresses we have that meet a given
threshold:

``` r
threshold = 2:100
num_actors = rep(0, 99)
for(i in threshold){
  num_actors[i-1] = sum(freq_df$n >= i)
}

ggplot(data.frame(threshold = threshold, num_actors = num_actors), aes(x = threshold, y = num_actors)) +
  geom_line() +
  geom_hline(yintercept = 1000, color = "red")
```

![](actors_investigation_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
sum(freq_df$n >= 20)
```

    ## [1] 1424

If we decide we want to have at least 1,000 actors/actresses to look at,
the threshold can be at most 23 films. 20 seems like a good round number
to use as the threshold. This will leave us with 1424 people.

``` r
tp3 = left_join(tp2, freq_df, by = "imdb_name_id") %>%
  filter(n >= 20)
```

Now we add in the actual names from IMDb names.csv and our
actors/actresses dataframe is complete!

``` r
id_to_names = read_csv("data/IMDb\ names.csv") %>%
  select(imdb_name_id, name)
```

    ## Rows: 297705 Columns: 17

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (12): imdb_name_id, name, birth_name, bio, birth_details, date_of_birth,...
    ## dbl  (5): height, spouses, divorces, spouses_with_children, children

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
people = left_join(tp3, id_to_names, by = "imdb_name_id")
```

# Actors Correlations with Ratings

The meat of this investigation will involve looking at how different
actors effect the distribution of ratings of a movie.

The IMDb rating scale is 1-10, and we have a choice of how we want to
look at rating. We’ll start by using the avg\_vote column in IMDb
movies.csv

ˆ

``` r
people = read_csv("data/IMDb\ movies.csv") %>%
  select(imdb_title_id, avg_vote, votes) %>%
  right_join(people, by = "imdb_title_id")
```

    ## Rows: 85855 Columns: 22

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (15): imdb_title_id, title, original_title, date_published, genre, count...
    ## dbl  (7): year, duration, avg_vote, votes, metascore, reviews_from_users, re...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

We could run into trouble here if some movies have a very small number
of votes, as that would mean avg\_vote would be very variable from a
real representation of audience response. Let’s look at how num\_votes
is distributed:

``` r
english_mv = read_csv("data/IMDb\ movies.csv") %>%
  filter(str_detect(language, "English"))
```

    ## Rows: 85855 Columns: 22

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (15): imdb_title_id, title, original_title, date_published, genre, count...
    ## dbl  (7): year, duration, avg_vote, votes, metascore, reviews_from_users, re...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
num_votes = english_mv$votes

sprintf("There are %d English-language movies", length(num_votes))
```

    ## [1] "There are 47446 English-language movies"

``` r
sprintf("The max number of votes is %d.", max(num_votes))
```

    ## [1] "The max number of votes is 2278845."

``` r
sprintf("The min number of votes is %d.", min(num_votes))
```

    ## [1] "The min number of votes is 99."

``` r
sprintf("The mean number of votes is %f.", mean(num_votes))
```

    ## [1] "The mean number of votes is 15740.330945."

``` r
sprintf("The median number of votes is %d.", median(num_votes))
```

    ## [1] "The median number of votes is 687."

``` r
l100 = sum(num_votes < 100)
sprintf("There are %d movies with less than 100 votes, which is %f of movies",
        l100, l100/length(num_votes))
```

    ## [1] "There are 3 movies with less than 100 votes, which is 0.000063 of movies"

``` r
l1000 = sum(num_votes < 1000)
sprintf("There are %d movies with less than 1000 votes, which is %f of movies",
        l1000, l1000/length(num_votes))
```

    ## [1] "There are 27167 movies with less than 1000 votes, which is 0.572588 of movies"

``` r
hist(num_votes)
```

![](actors_investigation_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
The distribution is so right skewed, that with the default parameters,
the histogram doesn’t tell us much.

It looks like only movies with 99 or more ratings are included in the
dataset, so we can go ahead without filtering movies with too few
ratings.

``` r
actor_ratings = people %>%
  group_by(imdb_name_id) %>%
  summarize(avg_rating = mean(avg_vote), vote_var = var(avg_vote)) %>%
  left_join(id_to_names, by = "imdb_name_id") %>%
  left_join(freq_df, by = "imdb_name_id")

hist(actor_ratings$avg_rating)
```

![](actors_investigation_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

We get to see some interesting stats now by looking at the
actor\_ratings table!

The actor with the worst average rating is Joe Estevez (Charlie Sheen’s
uncle), who is credited in 23 movies with an average rating of 3.92.

The actor with the highest average rating is Leonardo DiCaprio,who is
credited in 27 movies with an average rating of 7.36

``` r
sprintf("The average average rating is %f", mean(actor_ratings$avg_rating))
```

    ## [1] "The average average rating is 6.095120"

``` r
sprintf("The median average rating is %f", median(actor_ratings$avg_rating))
```

    ## [1] "The median average rating is 6.226365"

The average and median are pretty close, but the distribution is
definitely slightly left-skewed (by people like Joe Estevez).

To see how strong the correlation between actor and avg\_rating is,
let’s do a simulation of if each actor just starred in as many random
movies.

``` r
avg_ratings = actor_ratings$avg_rating
n = actor_ratings$n
sim_avg_ratings = rep(0, length(n))
movie_ratings = english_mv$avg_vote
for(i in 1:length(n)){
  sim_avg_ratings[i] = mean(sample(movie_ratings, n[i]))
}

hist(sim_avg_ratings)
```

![](actors_investigation_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Let’s compare this distribution to the actual distribution of
avg\_ratings:

``` r
compare_sim = data.frame(actual = actor_ratings$avg_rating, sim = sim_avg_ratings) %>%
  pivot_longer(c(actual, sim), names_to = "type")

ggplot(compare_sim, aes(x=value, fill=type)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#12C734", "#050399"))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](actors_investigation_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

We can see the distributions look very different! We can conclude there
is a strong correlation between actors and movie\_ratings.
