#' beritajatimr: Scrape Economic News from Berita Jatim Website
#'
#' This function scrapes economic news articles from Berita Jatim website.
#' It extracts article titles, publication dates, content, and links, and then processes
#' the data to clean dates and categorize keywords.
#' @name beritajatimr
#' @param x An integer specifying the number of pages to scrape.
#'
#' @import readxl
#' @import dplyr
#' @import stringr
#' @import openxlsx
#' @import tidyverse
#' @import rvest
#' @import lubridate
#' @import tidyr
#' @import tibble::tribble
#' @import utils
#'
#' @return A data frame containing the following columns:
#'   \item{judul}{The title of the news article.}
#'   \item{tglberita}{The publication date of the article.}
#'   \item{isiberita}{The content of the news article.}
#'   \item{link_judul}{The link to the full news article.}
#'   \item{tanggal}{The cleaned publication date in YYYY-MM-DD format.}
#'   \item{sentimen}{Sentiment classification based on keywords in the title.}
#'   \item{lokasi}{The location mentioned in the content (e.g., "Kediri").}
#'   \item{katakunci}{The extracted keywords from the article content.}
#'   \item{dptisi}{The function for extract the article content.}
#' @export beritajatimr
#' @examples
#' \dontrun{
#' beritajatim <- beritajatimr(x = 5)
#' }

utils::globalVariables(c("katakunci", "kategori", "kategori2", "tanggal", "lokasi",
                         "dptisi", "%>%"))

beritajatimr <- function(x) {
  dapatisi <- function(y){
    teks <- read_html(y) %>% html_nodes('.entry-content p') %>% html_text() %>%
      paste(collapse =",")
    return(teks)
  }

  beritajatimcom <- data.frame()

  for (hasil in seq(from = 1, to = x, by = 1)){
    url <- paste0("https://beritajatim.com/kanal/ekbis/page/",hasil,"/")
    laman <- read_html(url)

    judul <- laman %>% html_nodes('.list-post-on-sm .post-title a') %>% html_text()
    tglberita <- laman %>% html_nodes('.list-post-on-sm .post-date') %>% html_text()
    link_judul <- laman %>% html_nodes('.list-post-on-sm .post-title a') %>% html_attr("href") %>%
      paste(sep = "")

    isiberita <- sapply(link_judul, FUN = dapatisi, USE.NAMES = FALSE)

    beritajatimcom <- rbind(beritajatimcom, data.frame(judul, tglberita, isiberita, link_judul, stringsAsFactors = FALSE))
    print(paste("Page ke-", hasil))
  }

  beritajatimcom$lokasi <- beritajatimcom$isiberita
  beritajatimcom$lokasi <- gsub("\\s*\\(.*", "", beritajatimcom$lokasi)
  beritajatimcom$lokasi <- ifelse(grepl("Kota", beritajatimcom$isiberita), paste0("Kota ", beritajatimcom$lokasi), beritajatimcom$lokasi)
  beritajatimcom$lokasi[beritajatimcom$lokasi == "Surabaya"] <- "Kota Surabaya"

  beritajatimcom <- beritajatimcom %>%
    filter(grepl("Pacitan|Ponorogo|Trenggalek|Tulungagung|Blitar|Kediri|Malang|Lumajang|Jember|Banyuwangi|Bondowoso|Situbondo|Probolinggo|Pasuruan|Sidoarjo|Mojokerto|Jombang|Nganjuk|Madiun|Magetan|Ngawi|Bojonegoro|Tuban|Lamongan|Gresik|Bangkalan|Sampang|Pamekasan|Sumenep|Kota Kediri|Kota Blitar|Kota Malang|Kota Probolinggo|Kota Pasuruan|Kota Mojokerto|Kota Madiun|Kota Surabaya|Kota Batu", lokasi))
  beritajatimcom$tglberita <- gsub(" [|] ", " ", beritajatimcom$tglberita)

  beritajatimcom <- beritajatimcom %>%
    mutate(tanggal = gsub("Januari", "January", tglberita)) %>%
    mutate(tanggal = gsub("Februari", "February", tanggal)) %>%
    mutate(tanggal = gsub("Maret", "March", tanggal)) %>%
    mutate(tanggal = gsub("Mei", "May", tanggal)) %>%
    mutate(tanggal = gsub("Juni", "June", tanggal)) %>%
    mutate(tanggal = gsub("Juli", "July", tanggal)) %>%
    mutate(tanggal = gsub("Agustus", "August", tanggal)) %>%
    mutate(tanggal = gsub("Oktober", "October", tanggal)) %>%
    mutate(tanggal = gsub("Desember", "December", tanggal))

  beritajatimcom <- beritajatimcom %>%
    mutate(tanggal = format(dmy_hm(tanggal), "%Y-%m-%d"))

  beritajatimcom <- beritajatimcom %>%
    mutate(sentimen = ifelse(grepl("Meroket|Melambung|Melangit|Mencekik|Naik|Bertahan Tinggi|Menggigit|Pedas|Melejit|Merangkak|Membubung|Belum Turun|Meningkat|Digelontor|Untung|Dorong|Mendorong|Cepat|Pesat|Laris|Optimis|Murah|Mudah|Permudah", judul), 1,
                             ifelse(grepl("Turun|Terjun|Menurun|Turbulensi|Nyungsep|Merosot|Menurun|Berat|Melorot|Lesuh|Rugi|Buntung|Merugi|Lambat|Lamban|Sulit|Susah|Meleset", judul), -1, 0))
    )

  #hapus duplikat
  beritajatimcom2 <- beritajatimcom[!duplicated(beritajatimcom$judul),]
  judulku <- beritajatimcom2$judul
  katelapus <- tibble::tribble(
    ~katakunci, ~kategori,
    "pabrik", "industri",
    "kredit", "bank, jasa keuangan",
    "penumpang kereta", "transportasi darat",
    "penumpang pesawat", "transportasi udara",
    "kapal", "transportasi laut",
    "nissan", "industri kendaraan",
    "investasi", "investasi dan konstruksi",
    "bendungan", "konstruksi",
    "pembangunan tol", "konstruksi",
    "panen", "pertanian",
    "hutan", "kehutanan",
    "tambak", "perikanan",
    "perikanan", "perikanan",
    "pelabuhan", "transportasi laut",
    "infrastruktur", "investasi dan konstruksi",
    "resmikan kantor", "konstruksi",
    "penjualan hewan", "perdagangan",
    "perdagangan", "perdagangan",
    "kawasan industri", "industri, investasi, konstruksi",
    "banyak pesanan", "perdagangan",
    "potensi pasar", "perdagangan",
    "ekspor", "ekspor",
    "peluang ekspor", "ekspor",
    "impor", "impor",
    "bangun rumah", "konstruksi",
    "bangun kantor", "konstruksi",
    "bangun gedung", "konstruksi",
    "bangun tol", "konstruksi",
    "tkdn", "ekspor, impor",
    "mampu jual", "perdagangan",
    "untuk jual", "perdagangan",
    "panen raya", "pertanian",
    "curah hujan", "pertanian, cek",
    "pasarkan produk", "perdagangan",
    "pasar sapi", "perdagangan",
    "pasar hewan", "perdagangan",
    "pasar saham", "investasi",
    "penjualan", "perdagangan",
    "omset", "perdagangan",
    "tambah operasional", "transportasi",
    "tambah gerbong", "transportasi",
    "armada", "transportasi",
    "kawasan khusus industri", "industri, investasi, konstruksi",
    "kawasan industri khusus", "industri, investasi, konstruksi",
    "bazar", "perdagangan",
    "festival kuliner", "penyediaan makan minum",
    "proyek", "konstruksi",
    "pupuk", "pertanian",
    "benih", "pertanian",
    "emas antam", "perdagangan",
    "usaha", "industri",
    "pasar ponsel", "perdagangan",
    "harga mobil", "perdagangan",
    "harga sepeda motor", "perdagangan",
    "harga motor", "perdagangan",
    "harga kendaraan", "perdagangan",
    "harga", "perdagangan",
    "berinvestasi", "investasi",
    "pekerjaan drainase", "konstruksi",
    "pekerjaan jembatan", "konstruksi",
    "perumahan", "konstruksi",
    "pekerjaan bangunan", "konstruksi",
    "daur ulang", "pengelolaan sampah, daur ulang",
    "pengelolaan sampah", "pengelolaan sampah, daur ulang",
    "manfaatkan limbah", "pengelolaan sampah, daur ulang",
    "manfaatkan ampas", "pengelolaan sampah, daur ulang",
    "besi bekas", "pengelolaan sampah, daur ulang",
    "plastik bekas", "pengelolaan sampah, daur ulang",
    "pasang jaringan", "konstruksi",
    "pengerjaan", "konstruksi",
    "selesai dibangun", "konstruksi",
    "jembatan", "konstruksi",
    "pasokan", "perdagangan",
    "penanaman mangrove", "kehutanan",
    "penanaman kayu", "kehutanan",
    "penanaman modal", "investasi",
    "layanan kesehatan", "jasa kesehatan",
    "pasaran", "perdagangan",
    "sembako murah", "perdagangan",
    "pln", "pengadaan listrik",
    "pdam", "pengadaan air",
    "hortikultura", "pertanian",
    "pembayaran elektronik", "keuangan",
    "wisata", "akomodasi",
    "hotel", "akomodasi, cek lagi",
    "rumah makan", "penyediaan makan minum",
    "restoran", "penyediaan makan minum",
    "peternak", "peternakan",
    "petani", "pertanian",
    "nelayan", "perikanan",
    "penerbangan", "transportasi udara",
    "petrokimia", "industri",
    "pasokan bahan baku", "industri",
    "gerai", "perdagangan",
    "bisnis emas", "perdagangan",
    "nasabah", "keuangan",
    "diekspor", "ekspor",
    "diimpor", "impor",
    "disnak", "peternakan",
    "dishub", "transportasi",
    "perizinan", "investasi",
    "investor", "investasi",
    "perbaiki", "konstruksi",
    "kerajinan", "industri kreatif",
    "pedagang", "perdagangan",
    "produksi", "industri",
    "dibangun", "konstruksi",
    "potensi perikanan", "perikanan",
    "perajin", "industri kreatif",
    "hama", "pertanian",
    "pertamina", "pengadaan minyak dan gas, cek lagi",
    "daihatsu", "perdagangan",
    "danamon", "jasa keuangan bank",
    "pembangunan smelter", "konstruksi",
    "rumah kreatif", "industri kreatif",
    "bangun", "konstruksi",
    "kafe", "penyediaan makan minum",
    "harga cabai", "perdagangan",
    "harga telur", "perdagangan",
    "harga susu", "perdagangan",
    "harga bawang", "perdagangan",
    "harga minyak", "perdagangan",
    "harga beras", "perdagangan",
    "harga gula", "perdagangan",
    "harga tempe", "perdagangan",
    "harga tahu", "perdagangan",
    "harga tomat", "perdagangan",
    "harga kedelai", "perdagangan",
    "musim giling", "pertanian",
    "pemesanan", "perdagangan",
    "pasar tradisional", "perdagangan",
    "pasar moderen", "perdagangan",
    "pasar modern", "perdagangan",
    "penggantian", "konstruksi",
    "bisnis", "industri",
    "kai daop", "transportasi darat",
    "ptpn", "industri",
    "daya listrik industri", "pengadaan listrik",
    "tanaman", "pertanian",
    "jualan", "perdagangan",
    "ternak", "peternakan",
    "jalan", "konstruksi",
    "segmen", "perdagangan",
    "sentra ikan", "perikanan",
    "sentra industri", "industri",
    "kursi kereta", "industri",
    "inka", "transportasi",
    "kai", "transportasi",
    "krl", "transportasi",
    "stasiun", "transportasi darat",
    "terminal", "transportasi darat",
    "bandara", "transportasi udara",
    "promosikan", "perdagangan, ekspor",
    "promosi", "perdagangan, ekspor",
    "pelayaran", "transportasi laut",
    "okupansi hotel", "akomodasi",
    "desa wisata", "akomodasi",
    "tiket", "transportasi, cek darat, laut, udaranya",
    "lintasi", "transportasi",
    "melintas", "transportasi",
    "jumlah kendaraan", "transportasi",
    "permintaan", "perdagangan",
    "yamaha", "perdagangan motor",
    "honda", "perdagangan mobil atau motor",
    "tangkapan", "perikanan",
    "swalayan", "perdagangan",
    "nikmati listrik", "pengadaan listrik",
    "furniture", "perdagangan",
    "pembuatan gerbong", "industri alat angkutan",
    "perancang busana", "industri kreatif",
    "perbaikan", "konstruksi",
    "optimalisasi jaringan", "konstruksi",
    "kuliner", "penyediaan makan minum",
    "angkringan", "penyediaan makan minum",
    "makanan jadi", "penyediaan makan minum",
    "lesehan", "penyediaan makan minum",
    "minuman", "penyediaan makan minum",
    "kelontong", "penyediaan makan minum",
    "spklu", "pengadaan listrik",
    "plts", "pengadaan listrik",
    "gas", "pengadaan gas",
    "gas kota", "pengadaan gas"
  )
  beritajatimcom2 %>%
    mutate(katakunci = isiberita) %>%
    separate_rows(katakunci, sep = ' ') %>%
    mutate(katakunci = tolower(katakunci)) %>%
    left_join(katelapus) %>%
    filter(!is.na(kategori)) %>%
    select(-katakunci) -> judulku2

  judulku2 %>%
    group_by(judul) %>%
    summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
              kategori = toString(kategori),
              .groups = "drop") %>%
    select(judul, kategori, kategori2) -> oke

  coba <- inner_join(beritajatimcom, oke, by = "judul")

  # berita fixed
  beritajatim <- coba[,-c(2, 8)]
  names(beritajatim) <- c("judul", "isiberita", "link", "lokasi", "tanggal", "sentimen", "estimasi lapus")
  View(beritajatim)
  write.xlsx(beritajatim, paste0("beritajatim_", Sys.Date(),".xlsx"))

  return(beritajatim)
}
