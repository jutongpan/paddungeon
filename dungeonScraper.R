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


saveDungeonInfoAsHtml <- function(x, dbPath, overwrite = T) {

  filename <- paste0("templates/dungeonHtml/", x["dungeonName"], ".html")
  if (overwrite || !file.exists(filename)) {
    writeLines(
      text = cleanDungeonInfo(
        dungeonInfo = extractDungeonInfo(x["dungeonLink"]),
        dbPath = dbPath
      ),
      con = filename,
      useBytes = (Sys.info()[["sysname"]] == "Windows")
    )
  }

}


dt_dungeon <- fread("dungeon.csv", encoding = "UTF-8")

apply(X = dt_dungeon, MARGIN = 1, FUN = saveDungeonInfoAsHtml, dbPath = dbPath, overwrite = F)
