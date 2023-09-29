# Cria arquivos temporarios para baixar o .zip e descompactar

temp1 <- tempfile()
temp2 <- tempfile()

download.file(
  url = "http://dados.transportes.gov.br/dataset/42e2320b-ea67-4fdc-896f-71363e043fc6/resource/dd6aadae-7655-4fb8-b537-5a4e3e42d28e/download/renaest_dabertos_20230912.zip",
  destfile = temp1,
  method = "wget"
)

unzip(zipfile = temp1, exdir = temp2)

# Carrega apenas o csv dos acidentes e salva em um .rda para o uso no projeto

acidentes <- readr::read_csv2(paste0(
  temp2,
  "/Acidentes_DadosAbertos_20230912.csv"
))

save(acidentes, file = "data/acidentes.rda", compress = "bzip2")
