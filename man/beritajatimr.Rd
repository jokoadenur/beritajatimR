\name{beritajatimr}
\alias{beritajatimr}
\title{Scrape Economic News from Berita Jatim Website}
\usage{
beritajatimr(x)
}
\arguments{
  \item{x}{An integer specifying the number of pages to scrape.}
}
\description{
This function scrapes economic news articles from the Berita Jatim website.
It extracts article titles, publication dates, content, and links, processes
the data to clean dates, and categorizes keywords.
}
\details{
The function extracts the following information:
\itemize{
  \item{\code{judul}: The title of the news article.}
  \item{\code{tglberita}: The raw publication date of the article.}
  \item{\code{isiberita}: The full content of the news article.}
  \item{\code{link_judul}: The URL of the news article.}
  \item{\code{tanggal}: The cleaned publication date in \code{YYYY-MM-DD} format.}
  \item{\code{sentimen}: Sentiment classification based on keywords in the title (1 = positive, 0 = neutral, -1 = negative).}
  \item{\code{lokasi}: The location mentioned in the content (e.g., "Kediri").}
  \item{\code{katakunci}: Extracted keywords from the article content.}
}
}
\value{
A data frame containing the extracted and processed data from the specified number of pages.
}
\seealso{
\code{\link[rvest]{read_html}} for web scraping and \code{\link[dplyr]{mutate}} for data processing.
}
\examples{
\dontrun{
# Scrape 5 pages of economic news articles from Berita Jatim
beritajatim_data <- beritajatimr(x = 5)
}
}
