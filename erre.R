library(pdftools)
download.file("https://edisciplinas.usp.br/pluginfile.php/2596054/mod_resource/content/1/PROMETHEUS%20BOUND%20BY%20AESCHYLLUS.pdf",
              "/tmp/on_the_road.pdf", mode = "wb")
txt <- pdf_text('/tmp/on_the_road.pdf')

lines <- ''
for (i in 2:length(txt)) {
  tmp <- gsub('\r\n', ' ', txt[i]) # \r\n for Windows, \n for Linux
  lines <- paste(lines, tmp, sep=' ')
}

vec <- strsplit(lines, '\\.')
df <- data.frame(vec)

df <- as.data.frame(df[-c(nrow(df), nrow(df)-1), ]) # Remove Last 2 lines
colnames(df)[1] = 'line' # Rename Columns

df$line <- gsub("«", "", gsub("»", "", df$line))
df$line <- gsub('^\\s+PART\\s+[A-Z]+\\s+', '', df$line)
df$line <- as.character(trimws(df$line, 'both'))
df$line <- gsub('^[1-5]\\s{2,}', '',  df$line)
df$line <- gsub('- -, ', '',df$line)
library(sentimentr)
sentence <- c()
for (line in df$line) {
  tmp <- get_sentences(line)
  for(i in 1:length(tmp[[1]])) {
    sentence_tmp <- tmp[[1]][i]
    sentence <- c(sentence, sentence_tmp)
  }
}

df_sentr <- data.frame(sentence)
df_sentr$sentence <- as.character(df_sentr$sentence)

sentiment <- sentiment(df_sentr$sentence)

df_sentr$sentiment <- as.numeric(sentiment$sentiment)
df_sentr$pntag <- ifelse(sentiment$sentiment == 0, 'Neutral',
                         ifelse(sentiment$sentiment > 0, 'Positive',
                                ifelse(sentiment$sentiment < 0, 'Negative', 'NA')))

# base R plot
plot(df_sentr$sentiment, type='l', pch=3)

# plotly- more fun
ax <- list(
  title = "Sentence",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE
)

library(plotly)
library(magrittr)
plot_ly(data = df_sentr, x = ~sentence, y = ~sentiment, color = ~pntag,
        type = 'scatter', mode = 'markers') %>% layout(xaxis = ax)
plot_ly(data = df_sentr, y = ~sentiment, color = ~pntag,
        type = 'scatter', mode = 'markers') %>% layout(xaxis = ax)