# sportsviz

## Description
Nothing to it, there is a single function that takes your favorite MLB team and year and makes a funky chart. 

The "stream-plot" shows the top hitters on the team that year by wRC - weighted runs created. It's a comprehensive counting stat. 

## Installation

```{r}
devtools::install_github("ewimpey/sports_viz")
```

## Usage
Just plug in your favorite team and year. There is no customization possible. People will like what I tell them to like. 

```{r}
fav_team = "ATL"
fun_year = 2021

sportsviz::generate_viz(fav_team, fun_year)
```

## Color Schemes
Check out the `get_cp` function for a list of color palettes by team. There are only a few choices now, and if you pick a different team then you're getting Braves colors. 
Feel free to add a color palette for your team, just follow the structure that's already available. Here are the teams that are available now:
ATL
WSN
BAL
