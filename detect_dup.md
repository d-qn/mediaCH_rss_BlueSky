

# Charger les bibliothèques nécessaires
library(dplyr)
library(text2vec)

# Exemple de tibble
df <- tibble(
  title = c("Titre 1", "Titre 2", "Titre très similaire à 1", "Titre 4"),
  description = c("Description A", "Description B", "Description A", "Description D")
)

# Combiner titre et description pour chaque ligne
df <- df %>% mutate(full_text = paste(title, description))

# Tokenisation des textes combinés
it <- itoken(df$full_text, progressbar = FALSE)
vectorizer <- hash_vectorizer()
dtm <- create_dtm(it, vectorizer)

# Calculer la matrice de similarité de cosinus
similarity_matrix <- sim2(dtm, method = "cosine", norm = "l2")

# Repérer les paires très similaires (seuil de similarité de 0.9 par exemple)
threshold <- 0.9
similar_rows <- which(similarity_matrix > threshold & row(similarity_matrix) != col(similarity_matrix), arr.ind = TRUE)

# Garder seulement une version des lignes similaires
unique_indices <- setdiff(1:nrow(df), unique(similar_rows[, 2]))
df_unique <- df[unique_indices, ]

# Résultat sans doublons sémantiq


Explications

	•	Combinaison de title et description : En joignant title et description, nous facilitons la détection de similarités globales.
	•	Matrice de similarité : La fonction sim2 crée une matrice de similarité cosinus où chaque paire de lignes est comparée.
	•	Filtrage avec un seuil : En ajustant le seuil (ici 0.9), vous pouvez contrôler le niveau de similarité requis pour considérer des lignes comme des doublons.
	•	Filtrage des lignes similaires : En gardant uniquement les indices non répétés, on supprime les doublons sémantiques, conservant une seule ligne par groupe de descriptions similaires.

En ajustant le seuil, vous pouvez tester différents niveaux de tolérance selon les besoins de votre analyse.ues
df_unique
