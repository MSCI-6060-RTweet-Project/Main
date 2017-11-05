library("translateR")
library("ggplot2")

df <- read.csv("deerestatM.csv", stringsAsFactors = FALSE)
df$translatedContent <- NA

df_minus_en <- subset(df, df$lang != "en")
df_minus_und <- subset(df_minus_en, df_minus_en$lang != "und")
Lang_Plot <- qplot(lang, data = df_minus_und, geom = "bar")

df_fr <- subset(df, df$lang == "fr")
df_fr <- translate(dataset = df_fr,
                  content.field = 'stripped_text',
                  google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                  source.lang = "fr", target.lang = "en")

df_rbind <- rbind(df_fr, df, by = "X")

df_es <- subset(df, df$lang == "es")
df_es <- translate(dataset = df_es,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "es", target.lang = "en")

df_rbind <- rbind(df_es, df, by = "X")

df_pt <- subset(df, df$lang == "pt")
df_pt <- translate(dataset = df_pt,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "pt", target.lang = "en")

df_rbind <- rbind(df_pt, df, by = "X")

df_nl <- subset(df, df$lang == "nl")
df_nl <- translate(dataset = df_nl,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "nl", target.lang = "en")

df_rbind <- rbind(df_nl, df, by = "X")

df_ja <- subset(df, df$lang == "ja")
df_ja <- translate(dataset = df_ja,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "ja", target.lang = "en")

df_rbind <- rbind(df_ja, df, by = "X")

df_de <- subset(df, df$lang == "de")
df_de <- translate(dataset = df_de,
                   content.field = 'stripped_text',
                   google.api.key = "AIzaSyBTXz8tyyiuTUJVAjyy4A6nrw8-EQhCqiE",
                   source.lang = "de", target.lang = "en")

df_rbind <- rbind(df_de, df, by = "X")

df_excludeNA <- subset(df_rbind, !is.na(df_rbind$translatedContent))
df_excludeNA$stripped_text <- df_excludeNA$translatedContent

df_translated <- rbind(df_excludeNA, df_rbind, by = "X")

df_translated$translatedContent <- NULL

#deerestatsNew <- df_translated

write.csv(df_rbind, "deerestatsNew.csv")
