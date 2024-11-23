## packages
library(atrrr)
library(anytime)
library(dplyr)
library(stringr)
library(glue)
library(purrr)
library(xml2)

rss_settings  <- tibble (
  url = c("https://www.letemps.ch/articles.rss", "https://www.heidi.news/articles.rss"),
  handle = c("letemps.bsky.social", "heidi-news.bsky.social"),
  token = c( "letemps_bsky.rds", "heidi_news_bsky.rds")
  )

for (i in 1:nrow(rss_settings)) {

  ## Part 1: read RSS feed vers "Tous les articles"
  feed <- read_xml(rss_settings$url[i])
  # minimal custom RSS reader
  rss_posts <- tibble::tibble(
    title = xml_find_all(feed, "//item/title") |>
      xml_text(),

    creator = xml_find_all(feed, "//item/dc:creator") |>
      xml_text(),

    link = xml_find_all(feed, "//item/link") |>
      xml_text(),

    ext_link = xml_find_all(feed, "//item/guid") |>
      xml_text(),

    timestamp = xml_find_all(feed, "//item/pubDate") |>
      xml_text() |>
      utctime(tz = "UTC"),

    description = xml_find_all(feed, "//item/description") |>
      xml_text() |>
      # strip html from description
      vapply(function(d) {
        read_html(d) |>
          xml_text() |>
          trimws()
      }, FUN.VALUE = character(1))
  )

  ## Part 2: create posts from feed
  posts <- rss_posts |>
    mutate(desc_preview_len = 294 - nchar(title) - nchar(link),
           desc_preview_len_tmp = ifelse(desc_preview_len < 3, 3, desc_preview_len),
           desc_preview = map2_chr(description, desc_preview_len_tmp, function(x, y) str_trunc(x, y)),
           desc_preview = ifelse(desc_preview_len < 3, "", desc_preview),
           post_text = glue("{title}\n\n\"{desc_preview}\"\n\n{link}"))


  ## Part 3: get already posted updates and de-duplicate
  Sys.setenv(BSKY_TOKEN = rss_settings$token[i])
  auth(user = rss_settings$handle[i],
       password = if(i == 1) Sys.getenv("ATR_PW") else Sys.getenv("ATR_PW_HD"),
       overwrite = TRUE)
  old_posts <- get_skeets_authored_by(rss_settings$handle[i], limit = 5000L)
  posts_new <- posts |>
    filter(!post_text %in% old_posts$text) %>%
    filter(nchar(post_text) <= 300)


  ## Part 4: Post skeets!
  for (i in seq_len(nrow(posts_new))) {
    # if people upload broken preview images, this fails
    resp <- try(post_skeet(text = posts_new$post_text[i],
                           created_at = posts_new$timestamp[i]))
    if (methods::is(resp, "try-error")) post_skeet(text = posts_new$post_text[i],
                                                   created_at = posts_new$timestamp[i],
                                                   preview_card = FALSE)
  }

}


# ## Part 1: read RSS feed vers "Tous les articles"
# feed <- read_xml("https://www.letemps.ch/articles.rss")
# # minimal custom RSS reader
# rss_posts <- tibble::tibble(
#   title = xml_find_all(feed, "//item/title") |>
#     xml_text(),
#
#   creator = xml_find_all(feed, "//item/dc:creator") |>
#     xml_text(),
#
#   link = xml_find_all(feed, "//item/link") |>
#     xml_text(),
#
#   ext_link = xml_find_all(feed, "//item/guid") |>
#     xml_text(),
#
#   timestamp = xml_find_all(feed, "//item/pubDate") |>
#     xml_text() |>
#     utctime(tz = "UTC"),
#
#   description = xml_find_all(feed, "//item/description") |>
#     xml_text() |>
#     # strip html from description
#     vapply(function(d) {
#       read_html(d) |>
#         xml_text() |>
#         trimws()
#     }, FUN.VALUE = character(1))
# )



