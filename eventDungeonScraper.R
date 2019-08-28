library(rvest)
library(data.table)
library(RSQLite)
library(DBI)

if (Sys.info()[["nodename"]] == "jpan-personal") {
  setwd("/home/jpan/paddungeon")
  dbPath <- "/home/jpan"
} else if (Sys.info()[["nodename"]] == "MU-JPAN") {
  setwd("C:/Users/jpan/Documents/repos/paddungeon")
  dbPath <- "C:/Users/jpan/Documents/repos"
} else if (Sys.info()[["nodename"]] == "JUTONG-X1C") {
  setwd("C:/Users/jutong/Documents/repos/paddungeon")
  dbPath <- "C:/Users/jutong/Documents/repos"
}
dbPath <- file.path(dbPath, "paddata/padmonster.sqlite3")

source("dungeonScraperFunctions.R")

options(HTTPUserAgent = "Mozilla/5.0 (iPhone; CPU iPhone OS 12_2 like Mac OS X)")

URL_ROOT <- "http://pad.skyozora.com"


extractEventDungeonLinks <- function(URL_ROOT) {

  homepage <- read_html(URL_ROOT)

  nodes_table <- homepage %>% html_nodes('table')

  loc_node_eventList <- which(grepl(x = html_text(nodes_table), pattern = "目前進行中的活動"))

  links_event <- nodes_table[loc_node_eventList] %>% html_nodes("a") %>% html_attr("href")

  links_eventDungeon <- grep(x = links_event, pattern = "^/s/", value = T)

  return(links_eventDungeon)

}


filterEventDungeons <- function(links_eventDungeon) {

  patterns_excluded <- c(
    "初級者", "中級者",
    "の丼龍", "の猫龍", "の機甲龍", "の犬龍", "の古代龍", "の契約龍",
    "の転界龍", "の護神龍", "の寶珠龍", "の戰武龍", "の伴神龍",
    "スキルレベルアップ",
    "超絶経験値",
    "オール曜日ダンジョン",
    "イベント記念ダンジョン"
  )

  links_eventDungeon <- grep(
    x = links_eventDungeon,
    pattern = paste0(patterns_excluded, collapse = "|"),
    invert = T, value = T
  )

  return(links_eventDungeon)

}


links_eventDungeon <- extractEventDungeonLinks(URL_ROOT) %>% filterEventDungeons()

dt_eventDungeon <- rbindlist(lapply(links_eventDungeon, extractSubDungeons, URL_ROOT = URL_ROOT))


