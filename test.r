install.packages('tidyverse')
install.packages("gpairs")
install.packages("grid")
install.packages("lattice")
install.packages("arsenal")

library(tidyverse)
library(gpairs)
library(grid)
library(lattice)
library(arsenal)

imdb_title.ratings <- read_tsv("~/Downloads/title.ratings.tsv", na = "\\N", quote = '')
str(imdb_title.ratings)

hist(imdb_title.ratings$averageRating)
