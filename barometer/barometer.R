library(data.table)

path <- "c:/Users/ykama/OneDrive - Helsedirektoratet/dataset/barometer"
fcsv <- "InndataBarometer_Bydel2026.csv"

dt <- data.table::fread(file.path(path, fcsv))
