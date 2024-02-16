library(sf)
library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(ggthemes)

# 日本地図を読み込んで千葉県を抽出
#
JP_geo <- "https://github.com/igproj-fusion/R-gis/raw/main/ESRI_japan_84.geojson"
read_sf(JP_geo) |>
  filter(KEN == "千葉県") |>
  ms_dissolve() -> Chiba_ol

# 千葉県の500mメッシュ人口推定データ読み込み＆処理
#
Chiba_geo <- "https://raw.githubusercontent.com/igproj-fusion/R-gis/main/500m_mesh_2018_12.geojson"
read_sf(Chiba_geo) |>
  mutate(rate = (PTN_2050 - PTN_2015) / PTN_2015 * 100) |>
  mutate(rank = ifelse(rate >= 30, "a", 
                       (ifelse(rate < 30 & rate > 0, "b",
                               (ifelse(rate <= 0 & rate > -30, "c",
                                       ifelse(rate <= -30 & rate > -50, "d",
                                              ifelse(rate > -100, "e", "f")))))))) -> df
# 地図プロット
#
g <- ggplot()
g <- g + geom_sf(data = Chiba_ol, 
                 color = NA, linewidth=.1,fill = "darkgreen")
g <- g + geom_sf(data = df, aes(fill = PTN_2015), color = NA)
g <- g + geom_sf(data = Chiba_ol, 
                 color = "gray20", linewidth=.1,fill = NA)
g <- g + scale_fill_gradient(low = "#f8f8ff", high="darkred") 
g <- g + theme_map()
g <- g + labs(title = "千葉県の人口",
              subtitle = "2015年",
              caption = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-mesh500h30.html")
g <- g + theme(plot.margin = unit(c(2, 2, 2, 2), "lines"),
               plot.title = element_text(size = rel(2)),
               plot.subtitle = element_text(size = rel(1.5)),
               plot.caption = element_text(size = rel(1.2)),
               legend.text = element_text(size = rel(1.0)),
               legend.position = c(0.7, 0.05),
               legend.title = element_blank())
print(g)




g <- ggplot()
g <- g + geom_sf(data = Chiba_ol, 
                 color = NA, linewidth=.1,fill = "darkgreen")
g <- g + geom_sf(data = df, aes(fill = PTN_2050), color = NA)
g <- g + geom_sf(data = Chiba_ol, 
                 color = "gray20", linewidth=.1,fill = NA)
g <- g + scale_fill_gradient(low = "#f8f8ff", high="darkred") 
g <- g + theme_map()
g <- g + labs(title = "千葉県の人口",
              subtitle = "2050年",
              caption = "https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-mesh500h30.html")
g <- g + theme(plot.margin = unit(c(2, 2, 2, 2), "lines"),
               plot.title = element_text(size = rel(2)),
               plot.subtitle = element_text(size = rel(1.5)),
               plot.caption = element_text(size = rel(1.2)),
               legend.text = element_text(size = rel(1.0)),
               legend.position = c(0.7, 0.05),
               legend.title = element_blank())
print(g)







hist(df$PTN_2015)
hist(df$PTN_2050)


library(reshape2)

d <- data.frame(code = df$SHICODE,
                y2015 = df$PTN_2015,
                y2050 = df$PTN_2050)
dm <- melt(d, id.vars = "code")

ggplot(data=dm) + 
  geom_histogram(aes(x=value, color=variable), fill=NA,
                 position="identity") +xlim(c(0,1000))



