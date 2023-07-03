temp1 <- tempfile()
temp2 <- tempfile()

download.file(
  url = "http://dados.transportes.gov.br/dataset/42e2320b-ea67-4fdc-896f-71363e043fc6/resource/f76bcc13-4bce-4e15-af42-38bdbce47da5/download/renaest_dabertos_20230512.zip",
  destfile = temp1,
  method = "wget"
)

unzip(zipfile = temp1, exdir = temp2)

vitimas <- readr::read_csv2(paste0(
  temp2,
  "/Vitimas_DadosAbertos_20230512.csv"
))

save(vitimas, file = "data/vitimas.rda")
