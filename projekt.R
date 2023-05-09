# Załadowanie bibliotek
library(dplyr)
library(osmdata)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlr3tuning)
library(mlr3spatiotempcv)
library(raster)
library(sf)
library(rsample)
library(rgdal)
library(terra)
sf_use_s2(FALSE)
# Wczytanie danych  RGB plików w formacie TIFF
blue = raster("C:/Bartek/magisterka/Projekt_R/dane/kanały2/LC08_L1TP_186024_20230501_20230501_02_RT_B2.TIF")
green = raster("C:/Bartek/magisterka/Projekt_R/dane/kanały2/LC08_L1TP_186024_20230501_20230501_02_RT_B3.TIF")
red = raster("C:/Bartek/magisterka/Projekt_R/dane/kanały2/LC08_L1TP_186024_20230501_20230501_02_RT_B4.TIF")
rgb = stack(red, green, blue)
names(rgb) = c("red","green","blue")
#s <- 0.5
#plotRGB(rgb, scale=65535, stretch="lin", r = 1, g = 2, b = 3, saturate=s)


#przygotowanie dantych referencyjnych
wody = st_read("C:/Bartek/magisterka/Projekt_R/dane/referencyjne/przygotowane_dane/woda.shp")
lasy = st_read("C:/Bartek/magisterka/Projekt_R/dane/referencyjne/przygotowane_dane/las.shp")
rolne = st_read("C:/Bartek/magisterka/Projekt_R/dane/referencyjne/przygotowane_dane/rolne.shp")
budynki = st_read("C:/Bartek/magisterka/Projekt_R/dane/referencyjne/przygotowane_dane/budynki.shp")

reference_data <- rbind(wody, lasy, rolne, budynki)
#st_is_valid(reference_data)
reference_data2 = rmapshaper::ms_simplify(reference_data)
reference_data4 = st_sample(reference_data2,size=500)
reference_data4 = st_sf(geom = reference_data4)
Data_extract = extract(rgb,reference_data4)
#if_extract
Data_extract
reference_data5 = cbind(reference_data4,Data_extract)
reference_data5 = st_join(reference_data5,reference_data2)
reference_data5$osm_id = NULL
reference_data5
class(reference_data5$klasa)
reference_data5$klasa <- factor(reference_data5$klasa)
# Dodanie kolumn x i y
reference_data5$x = st_coordinates(reference_data5)[, 1]
reference_data5$y = st_coordinates(reference_data5)[, 2]
reference_data6 = as.data.frame(reference_data5)
reference_data6
reference_data6$geom = NULL
row.names(reference_data6) = 1:nrow(reference_data6) 

# Utworzenie zadania uczenia maszynowego
task = mlr3spatiotempcv::TaskClassifST$new(
  id = "landsat_class",
  backend = reference_data6, 
  target = "klasa", 
  coordinate_names = c("x", "y"),
  coords_as_features = FALSE,
  crs = "EPSG:4326")

task

learner = mlr3::lrn("classif.ranger", predict_type = "prob")
learner

resampling = mlr3::rsmp("repeated_spcv_coords", folds = 3, repeats = 100)
resampling

rr_spcv_glm = mlr3::resample(task = task,
                             learner = learner,
                             resampling = resampling)

rr_spcv_glm


mlr_measures

miara = mlr3::mlr_measures$get("classif.bacc")

score_spcv_glm = rr_spcv_glm$score(measures = miara)
score_spcv_glm

mean(score_spcv_glm$classif.bacc)

learner$train(task)
rgb2 = rast(rgb)
pred_cv = terra::predict(rgb2, model = learner$model, fun = predict, na.rm = TRUE)
plot(pred_cv)
pred_cv

