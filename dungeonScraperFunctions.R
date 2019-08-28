extractDungeonInfo <- function(link) {

  webpage <- read_html(link)

  nodes_table <- webpage %>% html_nodes('table')

  loc_node_dungeonInfo <- which(grepl(x = html_text(nodes_table), pattern = "^層數"))

  dungeonInfo <- toString(nodes_table[[loc_node_dungeonInfo]])

}


cleanDungeonInfo <- function(dungeonInfo, dbPath) {

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
  conn <- dbConnect(SQLite(), dbPath)
  dt_Type <- setDT(dbReadTable(conn, "Type"))
  dbDisconnect(conn)
  dt_Type[, TypeLinkOriginal := gsub(x = TypeIconDownload, pattern = "http://pad.skyozora.com/", replacement = "")]

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


extractSubDungeons <- function(link_dungeon, URL_ROOT) {

  dungeonPage <- read_html(
    paste0(URL_ROOT, link_dungeon) %>% 
      gsub(pattern = " ", replacement = "%20")
  )

  a_stage <- dungeonPage %>% html_nodes(".stage a")

  links_stage <- a_stage %>% html_attr("href")

  names_stage <- a_stage %>% html_text()

  links_subDungeon <- links_stage[grep(x = links_stage, pattern = "stage/")]

  names_subDungeon <- names_stage[grep(x = links_stage, pattern = "stage/")]

  return(
    data.table(
      dungeonLink = link_dungeon,
      dungeonName = gsub(x = link_dungeon, pattern = "^/s/", replacement = ""),
      subDungeonLink = links_subDungeon,
      subDungeonName = names_subDungeon
    )
  )

}
