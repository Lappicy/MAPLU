# Função para puxar informação das estações telemétricas da ANA de qualquer Unidade Federativa do Brasil ####
#' Puxar informações sobre as estações da ANA dentro de uma UF específica
#'
#' @param estacao_codigo se a estação está ativa (0) ou em manutenção (1).
#' O *default* é puxar os dois tipos
#' @param origem_codigo de quem é o dado. O *default* é puxar todos.
#' Os valores podem ser:
#' 0-Todas, 1-ANA/INPE, 2-ANA/SIVAM, 3-RES_CONJ_03, 4-CotaOnline, 5-Projetos Especiais.
#' @param UF_Brasil qual a UF (Unidade Federativa se quer analisar). O *default* é o "DF" (Distrito Federal).
#' Caso se selecione uma UF que não está no geobr, a função será paradad ("stop") e será retornado um aviso na tela.
#' @param buffer_UF tamanho do buffer em volta da UF escolhilda anteriormente. O *default* é de 10km.
#' Para não utilizar um *buffer* basta utilizar buffer_UF = 0.
#' @param projecao qual a projeção a ser utilizada para rodar o programa. O *default* é a UTM 23S (aconselhada para o DF).
#' As projeções devem ser informadas pelo número EPSG. Detalhes podem ser encontrados como no site <https://epsg.io/>.
#'
#' @return sf dataframe
#' @export
#'
#' @examples
#' # Para puxar todas as estações do DF + entorno (10km)
#' teste_ANA_info <- ANA_info()
#'
#' # Para puxar todas as estações contidas dentro do limite do DF
#' ANA_info_sem_buffer <- ANA_info(buffer_UF = 0)

ANA_info <- function(estacao_codigo = "",
                     origem_codigo = "",
                     UF_Brasil = "DF",
                     buffer_UF = 10000,
                     projecao = 31981){

  # Demora +-8s

  # Primeiro acessamos a lista contendo TODAS as estacoes (ver o que queremos)
  # o site <http://telemetriaws1.ana.gov.br/ServiceANA.asmx>
  url_base <- paste0("http://telemetriaws1.ana.gov.br/ServiceANA.asmx/",
                     "ListaEstacoesTelemetricas?",
                     "statusEstacoes=", estacao_codigo,
                     "&origem=", origem_codigo)


  # Transformar os dados desse link em um dataframe
  url_parse <- XML::xmlParse(url_base, encoding = "UTF-8")
  nodes_doc <- XML::getNodeSet(url_parse, "//Table")
  cadastro_estacoes <- XML::xmlToDataFrame(nodes = nodes_doc)


  # Ler o shp da UF selecionada pelo geobr ####
  # o geobr utiliza o SIRGAS 2000 e CRS(4674)
  
  # primeiro deixamos o "UF_Brasil" em maiuscula, para evitar erros
  UF_Brasil <- toupper(UF_Brasil)
  
  # Se a UF não for válida, a função para e é printado um aviso na tela
  if(!(UF_Brasil %in% c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO",
                        "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR",
                        "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"))){
    stop("nome da UF não é válida!\nUsar apenas a sigla (2 letras).")
  }
  
  # Com o UF sendo válido, puxa-se apenas os dados dessa UF
  UF_sf <- geobr::read_state(code_state = UF_Brasil)

  # Projeção para a definida no começo do código (padrão é a UTM 23S)
  UF_sf_proj <- sf::st_transform(UF_sf, crs = projecao)

  # Buffer em volta da UF selecionada
  buffer_UF_sf_proj <- sf::st_buffer(UF_sf_proj, dist = buffer_UF)


  # Selecionar so as estacoes dentro do Buffer ####
  # Transformar dados da ANA em formato sf com as colunas de Long/Lat
  cadastro_estacoes_sf <- sf::st_as_sf(cadastro_estacoes,
                                       coords = c("Longitude", "Latitude"),
                                       crs = 4326)

  # Projeção dos dados no formato definido (o padrão é UTM 23S)
  cadastro_estacoes_sf_proj <- sf::st_transform(cadastro_estacoes_sf,
                                                crs = projecao)

  # Quais estações estão dentro do Buffer
  # A função st_intersection() retorna apenas a geometria
  estacoes_UF <- sf::st_intersection(cadastro_estacoes_sf_proj$geometry,
                                     buffer_UF_sf_proj$geom)

  # Transformar objeto em sf
  estacoes_UF <- sf::st_as_sf(estacoes_UF)

  # Pegar a linha dos dados que estão dentro da UF definida
  UF_dados <- which(cadastro_estacoes_sf_proj$geometry %in% estacoes_UF$x)

  # Filtrar a tabela para conter apenas esses dados
  estacoes_UF_dados <- cadastro_estacoes_sf_proj[UF_dados,]

  # Trocar o nome das linhas
  rownames(estacoes_UF_dados) <- seq_len(nrow(estacoes_UF_dados))

  return(estacoes_UF_dados)



  # Filtrar pelo nome
  # estacoes_brasilia <- cadastro_estacoes %>%
  #   filter(`Municipio-UF` == "BRASÍLIA-DF")
}



# Função para puxar dados de determinada estação da ANA ####
#' Puxar dados para estações da ANA
#'
#' @param data_inicio primeira data que se quer os dados
#' @param data_fim última data que se quer os dados. O *default* é a data atual.
#' @param codigo_estacao pode ser um valor único de uma estação ou um vetor
#' contendo diversas estações.
#'
#' @return dataframe
#' @export
#'
#' @examples
#' teste_ANA_dados <- ANA_dados()
ANA_dados <- function(data_inicio = "01/01/2021",
                      data_fim = format(Sys.Date(), "%d/%m/%Y"),
                      codigo_estacao = c(60478482, 60479280)){

  # Demora +-20s POR estação
  # LEIA-ME ####
  # Se eu quiser puxar os dados, agora eh outro link!
  # <http://telemetriaws1.ana.gov.br/ServiceANA.asmx?op=DadosHidrometeorologicos>

  # Função interna que puxa os dados de UMA estação específica da ANA ####
  fun_puxar_dados_ANA <-
    function(cod_estacao_proxy){

      # Definir link da ANA
      url_base <- paste0("http://telemetriaws1.ana.gov.br/ServiceANA.asmx/",
                         "DadosHidrometeorologicos?",
                         "codEstacao=", cod_estacao_proxy,
                         "&dataInicio=", data_inicio,
                         "&dataFim=", data_fim)

      # Transformar em dataframe
      url_parse <- XML::xmlParse(url_base, encoding = "UTF-8")
      node_doc <- XML::getNodeSet(url_parse, "//DadosHidrometereologicos")
      Dados_proxy <- XML::xmlToDataFrame(nodes = node_doc)


      # Separar a coluna "DataHora" em duas
      Dados_proxy$Data <- substr(Dados_proxy$DataHora, 1, 10)
      Dados_proxy$Hora <- substr(Dados_proxy$DataHora, 12, 19)


      # Re-organizar a ordem das colunas do dataframe Dados_proxy
      Dados_proxy <- Dados_proxy[,c(1, 6, 7, 3, 4, 5)]


      # Retornar tabela final
      return(Dados_proxy)
    }


  # Se so tiver uma estação, puxar ela ####
  if(length(codigo_estacao) == 1) return(fun_puxar_dados_ANA(codigo_estacao))


  # Se tiver várias estações, puxar e juntar todas ####
  if(length(codigo_estacao) > 1){


    # Botar barra de progressão
    pb <- utils::txtProgressBar(0, length(codigo_estacao), style = 3)
    utils::setTxtProgressBar(pb, (length(codigo_estacao)-1))


    # Aplicar a função criada para cada estação
    Dados_proxy <- lapply(codigo_estacao,
                          FUN = function(x){
                            # Rodar a função
                            proxy <- fun_puxar_dados_ANA(cod_estacao_proxy = x)
                            # Atualizar a barra
                            utils::setTxtProgressBar(pb, length(codigo_estacao))
                            # Retornar a saida da função
                            return(proxy)
                          })


    # Juntar cada tabela em uma só
    Tabela_proxy_total <- do.call(rbind, Dados_proxy)
  }

  # Retornar tabela final ####
  return(Tabela_proxy_total)

}



# Juntar dados em valores Diários ####


