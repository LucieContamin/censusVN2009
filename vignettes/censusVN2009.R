## ----include = FALSE-----------------------------------------------------
knitr::knit_hooks$set(margin = function(before, options, envir) {
  if (before) par(mgp = c(1.5, .5, 0), bty = "n", plt = c(.105, .97, .13, .97))
  else NULL
})

knitr::opts_chunk$set(margin = TRUE, prompt = TRUE, comment = "",
                      collapse = TRUE, cache = FALSE,
                      dev.args = list(pointsize = 11), fig.height = 3.5,
                      fig.width = 4.24725, fig.retina = 2, fig.align = "center")

## ----eval = FALSE--------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("choisy/censusVN2009", build_vignettes = TRUE)

## ------------------------------------------------------------------------
library(censusVN2009)

## ------------------------------------------------------------------------
data(communes)
data(districts)
data(provinces)

## ------------------------------------------------------------------------
library(sp)
plot(communes)
plot(districts)
plot(provinces)

## ------------------------------------------------------------------------
head(communes@data)
head(districts@data)
head(provinces@data)

## ------------------------------------------------------------------------
length(communes_r)
length(unique(communes_r$district_id))
length(districts_r)
setdiff(communes_r$district_id, districts_r$district_id)
length(unique(districts_r$province_id))
length(provinces_r)
setdiff(provinces_r$province_id, districts_r$province_id)

## ------------------------------------------------------------------------
sum(provinces$population)
sum(provinces$area) / 100  # km2

## ------------------------------------------------------------------------
hist(communes$area / 100, n = 100, col = "grey", main = NA,
     xlab = "area", ylab = "frequency")
hist(districts$area / 100, col = "grey", main = NA,
     xlab = "area", ylab = "frequency")
hist(provinces$area / 100, col = "grey", main = NA,
     xlab = "area", ylab = "frequency")

## ------------------------------------------------------------------------
hist(communes$population, n = 100, col = "grey", main = NA,
     xlab = "population size", ylab = "frequency")
hist(districts$population, col = "grey", main = NA,
     xlab = "population size", ylab = "frequency")
hist(provinces$population, n = 15, col = "grey", main = NA,
     xlab = "population size", ylab = "frequency")

## ------------------------------------------------------------------------
with(communes@data, hist(log10(100 * population / area), n = 100, col = "grey",
                         main = NA, xlab = "population density", ylab = "frequency"))
with(districts@data, hist(log10(100 * population / area), n = 100, col = "grey",
                         main = NA, xlab = "population density", ylab = "frequency"))
with(provinces@data, hist(log10(100 * population / area), n = 15, col = "grey",
                         main = NA, xlab = "population density", ylab = "frequency"))

## ------------------------------------------------------------------------
plot(population ~ area, communes@data)
plot(population ~ area, communes@data, log = "xy")

## ------------------------------------------------------------------------
n <- 9
pal <- RColorBrewer::brewer.pal(n, "Blues")

## ------------------------------------------------------------------------
library(classInt)
tmp <- classIntervals(districts$population, n = n, style = "quantile")
plot(tmp, pal = pal, main = NA)

## ------------------------------------------------------------------------
plot(districts, col = findColours(tmp, pal))

## ------------------------------------------------------------------------
tmp <- classIntervals(districts$population / districts$area, n = n, style = "quantile")
plot(tmp, pal = pal, main = NA)
plot(districts, col = findColours(tmp, pal))

