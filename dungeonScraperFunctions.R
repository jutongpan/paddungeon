readHtmlMobileIgnoreSSL <- function(url) {

  read_html(
    httr::GET(
      url = url,
      httr::add_headers("user_agent" = "Mozilla/5.0 (iPhone; CPU iPhone OS 12_2 like Mac OS X)"),
      config = httr::config(ssl_verifypeer = F)
    )
  )

}


extractDungeonInfo <- function(link) {

  webpage <- readHtmlMobileIgnoreSSL(link)

  nodes_table <- webpage %>% html_nodes('table')

  loc_node_dungeonInfo <- which(grepl(x = html_text(nodes_table), pattern = "^層數"))

  dungeonInfo <- toString(nodes_table[[loc_node_dungeonInfo]])

}


cleanDungeonInfo <- function(dungeonInfo, dt_Type) {

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
    pattern = "<img src=\"images/change.gif\".*?>",
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

  dungeonPage <- readHtmlMobileIgnoreSSL(
    paste0(URL_ROOT, link_dungeon) %>% 
      gsub(pattern = " ", replacement = "%20")
  )

  a_stage <- dungeonPage %>% html_nodes(".stage a")

  links_stage <- a_stage %>% html_attr("href")

  names_stage <- a_stage %>% html_text()

  links_subDungeon <- links_stage[grep(x = links_stage, pattern = "stage/")]

  names_subDungeon <- names_stage[grep(x = links_stage, pattern = "stage/")]

  links_img <- dungeonPage %>% html_nodes(".content li div a img") %>% html_attr("data-original")

  links_imgBoss <- links_img[!is.na(links_img)]

  ids_boss <- gsub(
    x = links_imgBoss,
    pattern = "https://i1296.photobucket.com/albums/ag18/skyozora/pets_icon[0-9]?/([0-9]{1,4})_.*?.png",
    replacement = "\\1"
  ) %>% as.integer()

  return(
    data.table(
      dungeonLink = link_dungeon,
      dungeonName = gsub(x = link_dungeon, pattern = "^/s/", replacement = ""),
      subDungeonLink = links_subDungeon,
      subDungeonName = names_subDungeon,
      subDungeonBossId = ids_boss
    )
  )

}
