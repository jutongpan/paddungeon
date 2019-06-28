library(rvest)
library(data.table)
library(RSQLite)
library(DBI)

if (Sys.info()[["nodename"]] == "jpan-personal") {
  setwd("/home/jpan/paddungeon")
}

temp <- tempfile()
download.file("https://raw.githubusercontent.com/jutongpan/paddata/master/padmonster.sqlite3", temp)
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
    pattern = "<a href=\"pets/([0-9]{1,4})\" (class=\"tooltip\" title=.*?)<img.*?(.png.*?)</a>",
    replacement = "<a \\2<img src=\"https://raw.githubusercontent.com/jutongpan/paddata/master/img/MonsterIcon/\\1\\3</a>"
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
    pattern = "<img src=\"images/money.png\" width=\"25\" alt=\"錢箱\"> ",
    replacement = "錢箱"
  )

}

saveDungeonInfoAsHtml <- function(x) {

  writeLines(
    cleanDungeonInfo(extractDungeonInfo(x["dungeonLink"])),
    paste0("templates/dungeonHtml/", x["dungeonName"], ".html")
  )

}

dt_dungeon <- data.table(
  dungeonName = c(
    "六天の星霜龍",
    "異形の存在"
  ),
  dungeonLink = c(
    "http://pad.skyozora.com/stage/%E3%83%98%E3%82%AD%E3%82%B5%E3%82%BC%E3%82%AA%E3%83%B3%E9%99%8D%E8%87%A8%EF%BC%81/%E5%85%AD%E5%A4%A9%E3%81%AE%E6%98%9F%E9%9C%9C%E9%BE%8D%20%E5%A3%8A%E6%BB%85%E7%B4%9A",
    "http://pad.skyozora.com/stage/%E6%A5%B5%E9%99%90%E3%81%AE%E9%97%98%E6%8A%80%E5%A0%B4/%E7%95%B0%E5%BD%A2%E3%81%AE%E5%AD%98%E5%9C%A8"
  )
)

apply(X = dt_dungeon, MARGIN = 1, FUN = saveDungeonInfoAsHtml)
