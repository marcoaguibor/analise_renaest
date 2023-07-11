# É bom dedicar uma parte do projeto só para exportar os resultados de forma
# automática. Todas as funções aqui se baseam no `ggsave`

export_plots <- function(tipo, plot) {
  if (length(plot) == 28) {
    nomes <- paste0(tipo, "_", c("BR", lista_uf))
  } else {
    nomes <- paste0(tipo, "_", lista_uf)
  }

  path <- paste0("plot/", nomes, ".png")

  map2(
    path,
    plot,
    ~ggsave(.x, .y, device = "png", dpi = 300, width = 6, height = 3.5)
  )

}
