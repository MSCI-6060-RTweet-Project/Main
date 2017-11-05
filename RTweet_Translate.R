library("translateR")

df_source_lang <- read.csv("deerestatM.csv", stringsAsFactors = FALSE)
df_source_lang$translatedContent <- NA

df_minus_en <- subset(df_source_lang, df_source_lang$lang != "en")
df_minus_und <- subset(df_minus_en, df_minus_en$lang != "und")
Lang_Plot <- qplot(lang, data = df_minus_und, geom = "bar")

df_fr <- subset(df_source_lang, df_source_lang$lang == "fr")
df_fr <- translate(dataset = df_fr,
                  content.field = 'stripped_text',
                  google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                  source.lang = "fr", target.lang = "en")

df_es <- subset(df_source_lang, df_source_lang$lang == "es")
df_es <- translate(dataset = df_es,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "es", target.lang = "en")

df_rbind <- rbind(df_es, df_fr)

df_pt <- subset(df_source_lang, df_source_lang$lang == "pt")
df_pt <- translate(dataset = df_pt,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "pt", target.lang = "en")

df_rbind <- rbind(df_pt, df_rbind)

df_nl <- subset(df_source_lang, df_source_lang$lang == "nl")
df_nl <- translate(dataset = df_nl,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "nl", target.lang = "en")

df_rbind <- rbind(df_nl, df_rbind)

df_ja <- subset(df_source_lang, df_source_lang$lang == "ja")
df_ja <- translate(dataset = df_ja,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "ja", target.lang = "en")

df_rbind <- rbind(df_ja, df_rbind)

df_de <- subset(df_source_lang, df_source_lang$lang == "de")
df_de <- translate(dataset = df_de,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "de", target.lang = "en")

df_rbind <- rbind(df_de, df_rbind)

df_rbind$stripped_text <- df_rbind$translatedContent

df_exclude_lang <- subset(df_source_lang, df_source_lang$lang != "de")
df_exclude_lang <- subset(df_exclude_lang, df_exclude_lang$lang != "nl")
df_exclude_lang <- subset(df_exclude_lang, df_exclude_lang$lang != "es")
df_exclude_lang <- subset(df_exclude_lang, df_exclude_lang$lang != "fr")
df_exclude_lang <- subset(df_exclude_lang, df_exclude_lang$lang != "ja")
df_exclude_lang <- subset(df_exclude_lang, df_exclude_lang$lang != "pt")

df_translated <- rbind(df_exclude_lang, df_rbind)

df_translated$translatedContent <- NULL

deerestatsNew <- df_translated
