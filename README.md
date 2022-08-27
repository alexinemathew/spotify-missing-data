## Missing Data Analysis 
The objective of this project is to understand the implications of missing data on data analyses. 

**Techniques used:** mean/mode imputation, random imputation, hotdecking, regression imputation (with and without noise), multiple imputation, kNN imputation


## Dataset
The original dataset is called “Spotify Top 200 Charts (2020-2021)” and can be found on Kaggle [here](https://www.kaggle.com/datasets/sashankpillai/spotify-top-200-charts-20202021).

The dataset includes all the songs that have been on the Top 200 Weekly (Global) charts of Spotify in 2020 & 2021. I have subset the original dataset to only include the following variables:

    • Number of Times Charted: The number of times that the song has been on in the Spotify Top 200 Weekly Global Charts in 2020 & 2021.
    • Artist Followers: The number of followers the main artist has on Spotify.
    • Popularity: The popularity of the track. The value will be between 0 and 100, with 100 being the most popular.    
    • Tempo: The overall estimated tempo of a track in beats per minute (BPM).
    • Duration: The duration of the song in milliseconds.   
    • Feature: This is a column that I created based on whether on not there are multiple artists (aka guest features) on the track. This is a binary value (coding is 1 if there are guest features, 0 if none).
    
This dataset is completely observed with 1,545 observations, so I had to manually generate missingness (MAR).

## Research Question
Do song popularity, tempo, number of artist followers, guest features, and song duration have an influence on the number of times that a song is charted? 

In other words, we are looking to estimate how many times a song will be charted (Number.of.Times.Charted) based on the following five factors: popularity, tempo, number of artists followers, guest features, and duration.

**Implemented Model:** MLR



