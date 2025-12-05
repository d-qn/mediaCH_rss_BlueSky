## packages
library(atrrr)
library(anytime)
library(dplyr)
library(stringr)
library(glue)
library(purrr)
library(xml2)
library(text2vec)


rss_feeds  <- c (
  "https://www.letemps.ch/suisse.rss",
  "https://www.letemps.ch/tags/votations.rss",
  "https://www.letemps.ch/economie.rss",

  "https://www.heidi.news/articles.rss",

  "https://www.rts.ch/info/suisse/?format=rss/news",
  "https://www.rts.ch/info/regions/?format=rss/news",

  "https://www.publiceye.ch/fr/rssPublications.xml",

  "https://partner-feeds.publishing.tamedia.ch/rss/24heures/suisse",
  "https://partner-feeds.publishing.tamedia.ch/rss/24heures/vaud-regions",

  "https://partner-feeds.publishing.tamedia.ch/rss/tdg/geneve",

  "https://partner-feeds.publishing.tamedia.ch/rss/bilan/economie",


 # "https://partner-feeds.20min.ch/rss/20minutes/suisse-romande",
  "https://partner-feeds.20min.ch/rss/20minutes/suisse",


  "https://www.blick.ch/fr/suisse/rss.xml",

  "https://www.watson.ch/fr/api/2.0/rss/index.xml?tag=Suisse",

  "https://lecourrier.ch/rubrique/suisse/rss"#,
  # "https://lecourrier.ch/theme/geneve/rss",
  # "https://lecourrier.ch/theme/neuchatel/rss",
  # "https://lecourrier.ch/theme/valais/rss",
  # "https://lecourrier.ch/theme/vaud/rss",
  # "https://lecourrier.ch/theme/jura/rss",

 # "https://www.protestinfo.ch/flux/protestinfo/rss.xml"

)

### Part 1: Get the feeds ###

feedsl <- 1:length(rss_feeds) |>
  map(function(i) {
    cat("Processing feed", rss_feeds[i], "\n")

    ## Part 1: read RSS feed vers "Tous les articles"
    feed <- read_xml(rss_feeds[i])

    descr <- xml_find_all(feed, "//item/description") |>
      xml_text()

    if(str_detect(rss_feeds[i], "(partner\\-feeds|protestinfo|publiceye|blick)")) {
      descr_txt <- descr |> str_replace_all("<[^>]+>", "")
    } else {
      # strip html from description
      descr_txt <- descr |>
        vapply(function(d) {
          read_html(d) |>
            xml_text() |>
            trimws()
        }, FUN.VALUE = character(1))
    }
    # minimal custom RSS reader
    rss_posts <- tibble::tibble(
      title = xml_find_all(feed, "//item/title") |>
        xml_text(),

      # creator = xml_find_all(feed, "//item/dc:creator") |>
      #   xml_text(),

      link = xml_find_all(feed, "//item/link") |>
        xml_text(),

      # ext_link = xml_find_all(feed, "//item/guid") |>
      #   xml_text(),

      timestamp = xml_find_all(feed, "//item/pubDate") |>
        xml_text() |>
        utctime(tz = "UTC"),

      description = descr_txt

    )
    rss_posts %>%
      mutate(
        feed = rss_feeds[i]
      )
  })

feeds_df <- bind_rows(feedsl)

### Part 2: remove duplicated or very similar feeds ##
# Combiner titre et description pour chaque ligne
feeds_df <- feeds_df %>% mutate(tit_des = paste(title, description))

# Tokenisation des textes combinés
it <- itoken(feeds_df$tit_des, progressbar = FALSE)
vectorizer <- hash_vectorizer()
dtm <- create_dtm(it, vectorizer)

# Calculer la matrice de similarité de cosinus
similarity_matrix <- sim2(dtm, method = "cosine", norm = "l2")
# Remettre la diagonale à 0 pour éviter les auto-similarités parfaites
similarity_matrix <- as.matrix(similarity_matrix)
diag(similarity_matrix) <- 0

threshold <- 0.9
similar_pairs <- which(similarity_matrix > threshold, arr.ind = TRUE)

# Sélectionner les indices uniques en gardant seulement une version des lignes similaires
unique_indices <- setdiff(1:nrow(feeds_df), unique(similar_pairs[, 2]))
feeds_dfu <- feeds_df[unique_indices, ]

# Résultat sans doublons sémantiques
#feeds_dfu

### Part 3: create posts from unique feed ##
posts <- feeds_dfu |>
  arrange(timestamp) |>
  mutate(desc_preview_len = 294 - nchar(title) - nchar(link),
         desc_preview_len_tmp = ifelse(desc_preview_len < 3, 3, desc_preview_len),
         desc_preview = map2_chr(description, desc_preview_len_tmp, function(x, y) str_trunc(x, y)),
         desc_preview = ifelse(desc_preview_len < 3, "", desc_preview),
         post_text = glue("{title}\n\n\"{desc_preview}\"\n\n{link}"))

cat("\n", nrow(posts), "uniques new posts (", nrow(feeds_df) - nrow(posts), " similar or duplicated posts removed)\n")

## Part 4: get already posted updates and de-duplicate
Sys.setenv(BSKY_TOKEN = "r-bs-mediaCH.rds")
auth(user = "mediasch.bsky.social",
     password = Sys.getenv("MEDIACH_PW"),
     overwrite = TRUE)
old_posts <- get_skeets_authored_by("mediasch.bsky.social", limit = 5000L)

cat("\n", sum(posts$post_text %in% old_posts$text),
    " posts already skeeted (out of ", nrow(posts), " posts)\n")

posts_new <- posts |>
  filter(!post_text %in% old_posts$text) %>%
  filter(nchar(post_text) <= 300)


## Part 5: Post skeets!
for (i in seq_len(nrow(posts_new))) {
  # if people upload broken preview images, this fails
  resp <- try(post_skeet(text = posts_new$post_text[i],
                         created_at = posts_new$timestamp[i]))
  if (methods::is(resp, "try-error")) post_skeet(text = posts_new$post_text[i],
                                                 created_at = posts_new$timestamp[i],
                                                 preview_card = FALSE)
}
