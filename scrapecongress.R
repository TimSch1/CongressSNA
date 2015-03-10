library(rvest)
wiki = html('')
ala = wiki%>%
html_nodes('td a:nth-child(1)') %>%
html_text()
ala = ala[1:1530] #trim misc stuff at end and beginnin
ala = ala[212:1530]
ala = ala[-c(45,892)] #trim citations
ala = ala[-grep("run for", ala)]
ala = ala[-grep("San B", ala)]
ala = ala[-grep("Lieut", ala)]
ala = ala[-grep("Governor", ala)]
ala = ala[-grep("Alma ", ala)]
ala = ala[-grep("Donald Norcross", ala)]
vect = rep(1:3, length(ala)/3) #seq to make columns of data frame
Incumbent = ala[vect==1]
Terms = ala[vect==2]
Winner = ala[vect==3]
df = data.frame(Incumbent, Terms, Winner)
df$Winner = ifelse(df$Incumbent==Winner, 1, 0)
df$Terms = cut(as.numeric(as.character(df$Terms)), seq(from = 2014, to = 1954, by = -2))
levels(df$Terms) = seq(from = 30, to = 1, by=-1)
