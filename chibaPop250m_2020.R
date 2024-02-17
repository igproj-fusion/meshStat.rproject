###############################################################
#
# 統計地理情報システム＞
#   統計データダウンロード＞
#     国勢調査＞地域メッシュ統計＞
#       250mメッシュ
#
#
# 国勢調査/
#   2020年/5次メッシュ（250mメッシュ）/人口及び世帯/千葉県 
#   https://www.e-stat.go.jp/gis/statmap-search?page=1&type=1&toukeiCode=00200521&toukeiYear=2020&aggregateUnit=Q&serveyId=Q002005112020&statsId=T001102&prefCode=12
#
# 空間情報センター
#   250mメッシュ_12_千葉県
#   https://www.geospatial.jp/ckan/dataset/npli-pref-250m
#
##############################################################



if (!require("pacman")) {
  install.packages("pacman")}
pacman::p_load(
  sf,
  rmapshaper,
  tidyverse,
  ggplot2,
  ggthemes)


DirPath <- "~/meshStat/chibaPopHousehold_250m_2020/"  
File <- paste0(DirPath, dir(DirPath))

DF <- NULL
for(i in 1:length(File)) {
  DF <- read.csv(File[i], fileEncoding　=　"cp932") |>
    slice(-1) |>
    select(KEY_CODE, pop = T001102001) |>
    mutate(pop = as.numeric(pop)) |>
    mutate(KEY_CODE = as.character(KEY_CODE)) |>
    rbind(DF)
}

URL_mesh <- "https://raw.githubusercontent.com/igproj-fusion/R-gis/main/47_250m_geojson/12chiba250m.geojson"
CHIBA250m <- read_sf(URL_mesh)

map <- left_join(CHIBA250m, DF, by = c("code" = "KEY_CODE"))

g <- ggplot(map)
g <- g + geom_sf(aes(fill = pop), color = NA)
g <- g + scale_fill_gradient(low = "white", high ="darkred",
                             na.value="gray50")
g


#################################################################
# 日本地図を読み込んで千葉県を抽出
#
JP_geo <- "https://github.com/igproj-fusion/R-gis/raw/main/ESRI_japan_84.geojson"
read_sf(JP_geo) |>
  filter(KEN == "千葉県") |>
  ms_dissolve() -> Chiba_ol

g <- ggplot()
g <- g + geom_sf(data = map, aes(fill = pop), color = NA)
g <- g + scale_fill_gradient(low = "#f8f8ff", high = "darkred",
                             na.value="darkgreen") 
g <- g + geom_sf(data = Chiba_ol, 
                 color = "gray20", linewidth = .1, fill = NA)
g <- g + theme_void()
g <- g + labs(title = "千葉県の人口",
              subtitle = "2020年")
g <- g + theme(plot.margin = unit(c(2, 2, 2, 2), "lines"),
               plot.title = element_text(size = rel(1.6)),
               plot.subtitle = element_text(size = rel(1.4)),
               legend.text = element_text(size = rel(1.0)),
               legend.position = c(0.9, 0.15),
               legend.title = element_blank())
print(g)



