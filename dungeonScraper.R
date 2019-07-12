library(rvest)
library(data.table)
library(RSQLite)
library(DBI)

if (Sys.info()[["nodename"]] == "jpan-personal") {
  setwd("/home/jpan/paddungeon")
} else if (Sys.info()[["nodename"]] == "MU-JPAN") {
  setwd("C:/Users/jpan/Documents/repo/paddungeon")
} else if (Sys.info()[["nodename"]] == "JUTONG-X1C") {
  setwd("C:/Users/jutong/Documents/repos/paddungeon")
}

dt_dungeon <- fread("dungeon.csv", encoding = "UTF-8")

temp <- tempfile()
download.file("https://raw.githubusercontent.com/jutongpan/paddata/master/padmonster.sqlite3", temp, mode = "wb")
conn <- dbConnect(SQLite(), temp)
dt_Type <- setDT(dbReadTable(conn, "Type"))
dbDisconnect(conn)
dt_Type[, TypeLinkOriginal := gsub(x = TypeIconDownload, pattern = "http://pad.skyozora.com/", replacement = "")]

options(HTTPUserAgent = "Mozilla/5.0 (iPhone; CPU iPhone OS 12_2 like Mac OS X)")

extractDungeonInfo <- function(link) {

  webpage <- read_html(link)

  nodes_table <- webpage %>% html_nodes('table')

  loc_node_dungeonInfo <- which(grepl(x = html_text(nodes_table), pattern = "^層數"))

  dungeonInfo <- toString(nodes_table[[loc_node_dungeonInfo]])

}

cleanDungeonInfo <- function(dungeonInfo) {

  # Replace image source for monster icons
  dungeonInfo <- gsub(
    x = dungeonInfo,
    pattern = "<a href=\"pets/([0-9]{1,4})\".*?.png(.*?)</a>",
    replacement = paste0(
      "<a href=\"/padmonster/?singleMonsterId=\\1\">",
      "<img src=\"https://raw.githubusercontent.com/jutongpan/paddata/master/img/MonsterIcon/\\1.png\\2</a>"
    )
  )

  dungeonInfo <- gsub(
    x = dungeonInfo,
    pattern = ".png%20\"",
    replacement = ".png\""
  )

  # Replace image source for monster icons in enemy skills
  dungeonInfo <- gsub(
    x = dungeonInfo,
    pattern = "<a href=\"/pets/([0-9]{1,4})\"><img src=\"images/pets.*?.png\\?*1*\"(.*?)</a>",
    replacement = paste0(
      "<a href=\"/padmonster/?singleMonsterId=\\1\">",
      "<img src=\"https://raw.githubusercontent.com/jutongpan/paddata/master/img/MonsterIcon/\\1.png\"\\2</a>"
    )
  )

  # Replace type icon
  for (i in dt_Type$TypeId) {
    dungeonInfo <- gsub(
      x = dungeonInfo,
      pattern = paste0("<img src=\"", dt_Type[i, TypeLinkOriginal]),
      replacement = paste0("<img src=\"https://raw.githubusercontent.com/jutongpan/paddata/master/img/Type/", i, ".png")
    )
  }

  # Replace image source for drops/orbs
  dungeonInfo <- gsub(
    x = dungeonInfo,
    pattern = "<img src=\"images/drops/(.*?).png",
    replacement = "<img src=\"https://raw.githubusercontent.com/jutongpan/paddata/master/img/Orb/\\1.png"
  )

  # Replace change.gif
  dungeonInfo <- gsub(
    x = dungeonInfo,
    pattern = "<img src=\"images/change.gif\">",
    replacement = "變為"
  )

  # Replace money
  dungeonInfo <- gsub(
    x = dungeonInfo,
    pattern = "<img src=\"images/money.png\".*?> ",
    replacement = "錢箱"
  )

}

saveDungeonInfoAsHtml <- function(x, overwrite = T) {

  filename <- paste0("templates/dungeonHtml/", x["dungeonName"], ".html")
  if (overwrite || !file.exists(filename)) {
    writeLines(
      cleanDungeonInfo(extractDungeonInfo(x["dungeonLink"])),
      filename,
      useBytes = (Sys.info()[["sysname"]] == "Windows")
    )
  }

}

apply(X = dt_dungeon, MARGIN = 1, FUN = saveDungeonInfoAsHtml, overwrite = F)
