library(xml2)
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
  Sys.setlocale(category = "LC_ALL", locale = "chs")
} else if (Sys.info()[["nodename"]] == "JUTONG-X1C") {
  setwd("C:/Users/jutong/Documents/repos/paddungeon")
  dbPath <- "C:/Users/jutong/Documents/repos"
  Sys.setlocale(category = "LC_ALL", locale = "chs")
}
dbPath <- file.path(dbPath, "paddata/padmonster.sqlite3")

source("dungeonScraperFunctions.R", encoding = "utf-8")

# options(HTTPUserAgent = "Mozilla/5.0 (iPhone; CPU iPhone OS 12_2 like Mac OS X)")

URL_ROOT <- "http://pad.skyozora.com"


extractEventDungeonLinks <- function(URL_ROOT) {

  homepage <- readHtmlMobileIgnoreSSL(URL_ROOT)

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


prepareDirStructure <- function(dt_eventDungeon) {

  ## Get existing directories for event dungeons
  names_dungeon <- list.dirs(
    path = file.path("templates", "dungeonHtml", "event"),
    full.names = F,
    recursive = F
  )

  ## Delete directories for obsolete event dungeons
  names_obsoleteDungeon <- setdiff(names_dungeon, dt_eventDungeon$dungeonName)
  unlink(file.path("templates", "dungeonHtml", "event", names_obsoleteDungeon), recursive = T)

  ## Add directories for new event dungeons
  names_newDungeon <- setdiff(dt_eventDungeon$dungeonName, names_dungeon)
  for (name_dungeon in names_newDungeon) {
    dir.create(file.path("templates", "dungeonHtml", "event", name_dungeon), recursive = T)
  }

  return(
    list(names_obsoleteDungeon = names_obsoleteDungeon, names_newDungeon = names_newDungeon)
  )

}


saveDungeonInfoAsHtml <- function(dt_eventDungeon, names_newDungeon, dbPath, URL_ROOT) {

  conn <- dbConnect(SQLite(), dbPath)
  dt_Type <- setDT(dbReadTable(conn, "Type"))
  dbDisconnect(conn)
  dt_Type[, TypeLinkOriginal := gsub(x = TypeIconDownload, pattern = "http://pad.skyozora.com/", replacement = "")]

  for (name_dungeon in names_newDungeon) {

    for (name_subDungeon in dt_eventDungeon[dungeonName==name_dungeon, subDungeonName]) {

      link_subDungeon <- dt_eventDungeon[dungeonName==name_dungeon & subDungeonName == name_subDungeon, subDungeonLink]

      writeLines(
        text = cleanDungeonInfo(
          dungeonInfo = extractDungeonInfo(paste0(URL_ROOT, "/", link_subDungeon)),
          dt_Type = dt_Type
        ),
        con = file.path("templates", "dungeonHtml", "event", name_dungeon, paste0(name_subDungeon, ".html")),
        useBytes = (Sys.info()[["sysname"]] == "Windows")
      )

    }

  }

}


links_eventDungeon <- extractEventDungeonLinks(URL_ROOT) %>% filterEventDungeons()

dt_eventDungeon <- rbindlist(lapply(links_eventDungeon, extractSubDungeons, URL_ROOT = URL_ROOT))

output_prepareDirStructure <- prepareDirStructure(dt_eventDungeon)
names_obsoleteDungeon <- output_prepareDirStructure$names_obsoleteDungeon
names_newDungeon <- output_prepareDirStructure$names_newDungeon

saveDungeonInfoAsHtml(dt_eventDungeon, names_newDungeon, dbPath, URL_ROOT)

fwrite(dt_eventDungeon, "eventDungeon.csv")

print(Sys.time())
print("Deleted obsolete event dungeons:")
print(names_obsoleteDungeon)
print("Added new event dungeons:")
print(names_newDungeon)
print("")
