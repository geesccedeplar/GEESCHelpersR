# GEESCHelpersR
Helpers no R, inicialmente pra puxar arquivos usando api do dropbox e POST

Esse pacote deve usado da seguinte forma:

# Lendo arquivos do dropbox ou localmente, atualizando arquivo local se diferente.
```{r try-download, eval=FALSE}
#' \dontrun{
library(GEESCHelpersR)
local_path <- "/local/path.csv"
drop_path <- "/dropbox/path.csv"
dbx <- get_dropbox_access_token("/path/to/.env")

file <- dbx %>%
  try_download(dropbox_path, local_path) %>%
  # function to read the file
  read.csv()
#' }
```

# Lendo do Dropbox ou localmente para arquivos ".zip" com shapefiles.
```{r try-sf, eval=FALSE}
#' \dontrun{
library(GEESCHelpersR)
local_path <- "/local/path.csv"
drop_path <- "/dropbox/path.csv"
dbx <- get_dropbox_access_token("/path/to/.env")

file <- dbx %>%
  try_sf_download(dropbox_path, local_path) %>%
  # function to read the file
  read.csv()
#' }
```

# Baixando do Dropbox somente para o disco.
```{r download-file, eval=FALSE}
#' \dontrun{
library(GEESCHelpersR)
local_path <- "/local/path.csv"
drop_path <- "/dropbox/path.csv"
dbx <- get_dropbox_access_token("/path/to/.env")

dbx %>%
  download_dropbox_file(drop_path, local_path)
#' }
```

# Lendo do Dropbox somente para a memória.
```{r download-memory, eval=FALSE}
#' \dontrun{
library(GEESCHelpersR)

drop_path <- "/dropbox/path.csv"
dbx <- get_dropbox_access_token("/path/to/.env")

dbx %>%
  download_dropbox_memory(drop_path) %>%
  # function to read the file
  read.csv()
#' }
```

# Guia pra fazer pacotes no R
[Making your first R package](https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html)