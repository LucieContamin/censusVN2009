library(rgdal)     # for "readOGR"
library(maptools)  # for "thinnedSpatialPoly"
library(dplyr)
library(magrittr)  # for %<>%

# NOTES:
# the data come with polygons only for the 3 levels (communes, districts and
# province) on one hand and with polygons with census data for commune level
# only on the other hand.
# The idea is (1) to combine polygons and census data for the commune level and
# (2) to spread census data from commune level to district and province levels.
# Also note that the Spratley islands (Trường Sa) are included in the province
# of Khánh Hòa. Likewise, the Paracel island (Hoàng Sa) are included in the
# province of Đà Nẵng. The polygons of these islands are present only for the
# province level.

# (1) reading polygons of communes, districts and provinces --------------------
provinces_r <- readOGR("data-raw/Polygons", "provinces")
districts_r <- readOGR("data-raw/Polygons", "districts")
communes_r <- readOGR("data-raw/Polygons", "communes")


# rearranging the attributes of the communes -----------------------------------
communes_r@data$ADDRESS <- NULL
names(communes_r@data) <- c("object_id", "commune_id", "district_id",
                            "commune_vn", "commune", "district_vn", "district",
                            "province_vn", "province", "province_id",
                            "shape_length", "shape_area")
communes_r@data <- communes_r@data[, c("object_id", "province_id",
                                       "district_id", "commune_id", "province",
                                       "district", "commune", "province_vn",
                                       "district_vn", "commune_vn",
                                       "shape_length", "shape_area")]


# rearranging the attributes of the districts ----------------------------------
districts_r@data$UniqueIden <- NULL
names(districts_r@data) <- c("object_id", "district_id", "district_vn",
                             "district", "province", "level", "type",
                             "province_id", "shape_length", "shape_area")
hash <- with(lapply(unique(communes_r@data[, c("province", "province_vn")]),
                    as.character),
             setNames(province_vn, province))
districts_r@data$province_vn <- hash[districts_r@data$province]

districts_r@data <- districts_r@data[, c("object_id", "province_id",
                                         "district_id", "province", "district",
                                         "province_vn", "district_vn",
                                         "shape_length", "shape_area")]


# rearranging the attributes of the provinces ----------------------------------
names(provinces_r@data) <- c("object_id", "province_vn", "province",
                             "province_id", "shape_length", "shape_area")

provinces_r@data <- provinces_r@data[, c("object_id", "province_id", "province",
                                         "province_vn", "shape_length",
                                         "shape_area")]


# fixing spelling for provinces ------------------------------------------------

provinces_r@data$province %<>%
  stringi::stri_escape_unicode() %>% dictionary::vn_province[.]

provinces_r@data$province_vn %<>% stringi::stri_escape_unicode()

# fixing spelling for districts ------------------------------------------------

districts_r@data$province %<>% gsub(" city", "", .) %>% tolower() %>%
  stringi::stri_escape_unicode() %>% dictionary::vn_province[.]

districts_r@data %<>%
  mutate_at(vars(matches("_vn")), stringi::stri_escape_unicode)


# fxing spelling for communes --------------------------------------------------

communes_r@data$province %<>%
  stringi::stri_escape_unicode() %>% dictionary::vn_province[.]

communes_r@data %<>%
  mutate_at(vars(matches("_vn")), stringi::stri_escape_unicode)

# fixing a commune ID ----------------------------------------------------------
communes_r@data[communes_r@data$district_id == 60141, "district_id"] <- 60114


# fixing districts -------------------------------------------------------------
districts_to_remove <- names(which(table(communes_r$district_id) < 2))
communes_r <- subset(communes_r, !(district_id %in% districts_to_remove))
districts_r <- subset(districts_r, !(district_id %in% districts_to_remove))

tomerge <- subset(districts_r, object_id %in% c(357, 704))
merged <- maptools::unionSpatialPolygons(tomerge, c(357, 357))
attr(merged@polygons[[1]]@Polygons[[1]]@coords, "dimnames") <- NULL
sel <- which(sapply(districts_r@polygons, function(x) x@ID) == 357)
districts_r@polygons[[sel]] <- merged@polygons[[1]]
districts_r <- subset(districts_r, !(object_id %in% paste(c(385, 617, 704))))


# (2) reading the census data (and polygons) of communes -----------------------

census <- readOGR("data-raw/Vietnam_pop_commune", "Vietnam_pop_commune")
census@data$PRO_NAME_E <- sub("Thua Thien Hue", "Thua Thien - Hue",
                              census@data$PRO_NAME_E)
census@data$ADDRESS <- NULL
census@data$Shape_Le_1 <- NULL
names(census@data) <- c("commune_id", "district_id", "commune_vn", "commune",
                        "district_vn", "district", "province_vn", "province",
                        "province_id", "population", "area", "shape_length",
                        "shape_area")
census@data <- census@data[, c("commune_id", "district_id", "province_id",
                               "commune", "district", "province", "commune_vn",
                               "district_vn", "province_vn", "population",
                               "area", "shape_length", "shape_area")]
rownames(census@data) <- NULL
for (i in seq_along(census@polygons))
  census@polygons[[i]]@ID <- as.character(
    as.numeric(census@polygons[[i]]@ID) + 1)


# (3) putting polygons and communes census data together -----------------------
sel <- setdiff(census$commune_id, communes_r$commune_id)
# there are 18 communes of the census absent from polygons
census <- subset(census, !(commune_id %in% sel))
tmp <- rownames(communes_r@data)
communes_r <- merge(communes_r, subset(census@data,
                                    select = c(commune_id, area, population)))
rownames(communes_r@data) <- tmp


# generating the population sizes for the districts ----------------------------
tmp <- rownames(districts_r@data)
districts_r <- communes_r@data %>%
  group_by(district_id) %>%
  summarise(area = sum(area), population = sum(population)) %>%
  merge(districts_r, .)
rownames(districts_r@data) <- tmp


# generating the population sizes for the provinces ----------------------------
tmp <- rownames(provinces_r@data)
provinces_r <- districts_r@data %>%
  group_by(province_id) %>%
  summarise(area = sum(area), population = sum(population)) %>%
  merge(provinces_r, .)
rownames(provinces_r@data) <- tmp


# rearraging the communes, districts and provinces attributes ------------------
communes_r@data <- communes_r@data[, c("province_id", "district_id",
                                       "commune_id", "province", "district",
                                       "commune", "province_vn", "district_vn",
                                       "commune_vn", "shape_length",
                                       "shape_area", "area", "population")]

districts_r@data <- districts_r@data[, c("province_id", "district_id",
                                         "province", "district", "province_vn",
                                         "district_vn", "shape_length",
                                         "shape_area", "area", "population")]

provinces_r@data <- provinces_r@data[, c("province_id", "province",
                                         "province_vn", "shape_length",
                                         "shape_area", "area", "population")]


# changing the feature IDs -----------------------------------------------------
communes_r <- spChFIDs(communes_r, as.character(seq_len(length(communes_r))))
districts_r <- spChFIDs(districts_r, as.character(seq_len(length(districts_r))))
provinces_r <- spChFIDs(provinces_r, as.character(seq_len(length(provinces_r))))


# thining polygons -------------------------------------------------------------
provinces <- thinnedSpatialPoly(provinces_r, .01)
districts <- thinnedSpatialPoly(districts_r, .0036)
communes <- thinnedSpatialPoly(communes_r, .00145)


# converting factors to characters in the attributes ---------------------------
communes@data %<>% mutate_if(is.factor, as.character)
districts@data %<>% mutate_if(is.factor, as.character)
provinces@data %<>% mutate_if(is.factor, as.character)
communes_r@data %<>% mutate_if(is.factor, as.character)
districts_r@data %<>% mutate_if(is.factor, as.character)
provinces_r@data %<>% mutate_if(is.factor, as.character)


# saving -----------------------------------------------------------------------
devtools::use_data(provinces, districts, communes, provinces_r, districts_r,
                   communes_r, overwrite = TRUE)
