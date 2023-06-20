#Carregando bibliotecas

#Manipulação de dados
library(dplyr)
library(stringr)

#Data Scraping
library(rvest)

#Desenvolvimento do WebApp
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinyalert)

#Requisições HTTP para o GPT
library(httr)

#Interacao com API do DALLE para imagens
library(openai)

#Substitua com sua chave da api abaixo
#Caso ainda não possua, gere uma em https://platform.openai.com/account/api-keys
chave_api <- readLines("~/Downloads/chave.txt")
Sys.setenv(OPENAI_API_KEY = chave_api)

#Criar uma função auxiliar para passar o prompt e extrair a resposta
chama_gpt <- function(prompt) {
    resposta <- POST(
        url = "https://api.openai.com/v1/chat/completions", 
        add_headers(Authorization = paste("Bearer", chave_api)),
        content_type_json(),
        encode = "json",
        body = list(
            model = "gpt-3.5-turbo",
            messages = list(list(
                role = "user", 
                content = prompt
            ))
        )
    )
    str_trim(content(resposta)$choices[[1]]$message$content)
}

#Importar listas de cidade para serem os cenários do filme
cidades <- read.csv(
    "https://raw.githubusercontent.com/mapaslivres/municipios-br/main/tabelas/municipios.csv"
) %>%
    arrange(name) %>%
    pull(name)

#Importar lista de filmes que podem servir de inspiração
lista_filmes <- 
    read_html(
    "https://pt.wikipedia.org/wiki/Lista_de_filmes_de_maior_bilheteria"
    ) %>%
    html_element(
        xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[1]"
        ) %>%
    html_table()

#Devido ao corte do ChatGPT em 2021,
#vamos filtrar os filmes lançados depois disso
lista_filmes <- 
    lista_filmes %>%
        filter(Ano < 2021) %>%
        arrange(Filme) %>% #Depois de filtrar, ordenamos alfabeticamente
        pull(Filme) #E selecionamos apenas os nomes

    
#Montando o WebApp

#Front-end
ui <- fluidPage(
    #CSS para o painel ocupar toda a área vertical
    tags$style(
        HTML(
            "hr {border-top: 1px solid #000000;}
                .well {height: 1200px;}"
        )
    ),
    #Tema escuro
    theme = shinythemes::shinytheme(theme = "darkly"),
    sidebarLayout(
        sidebarPanel =  sidebarPanel(
            img(
                src = "logo-imersao-ai-header.png",
                width = "75%",
                align = "middle"
                ),
            h1("Gerador de Sinopse"),
            hr(),
            p("Use essa ferramenta para gerar uma sinopse de um filme único!"),
            p("Escolha uma cidade brasileira e um filme original para servir de inspiração"),
            pickerInput(
                inputId = "cidade",
                label = "Selecione a cidade onde se passará o filme",
                selected = "Rio de Janeiro",
                choices = cidades,
                options = pickerOptions(
                    liveSearch = TRUE
                )
            ),
            pickerInput(
                inputId = "filme",
                label = "Selecione um filme para servir de base",
                selected = NULL,
                choices = lista_filmes,
                options = pickerOptions(
                    liveSearch = TRUE
                )
            ),
            actionButton(
                inputId = "botao",
                label = "",
                icon = icon(
                    name = "arrow-right",
                    lib = "font-awesome"
                ),
            ),
            br(),
            hr()
        ),
        mainPanel = mainPanel(
            img(
                src = "alura_logo.svg",
                width = "10%",
                align = "right"
            ),
            uiOutput("poster") |>
                withSpinner(),
            textOutput(
                "resposta"
            ) |>
                withSpinner()
        )
    )
)

#Back-end
server <- function(input, output) {
    #Prompt engineering
    #Usamos um prompt base e inserimos as variáveis passadas pelo usuário
    
    #Criar uma expressão reativa para plugar os inputs
    rval_prompt <- reactive({
        cidade <- input$cidade
        filme <- input$filme
        paste(
            "Escreva uma sinopse de um filme, baseado em",
            filme,
            "e que se passe na cidade de",
            cidade,
            ".",
            "A sinopse deve ser curta, com apenas um parágrafo.",
            "Enfatize características da cidade escolhida.",
            "Detalhes são importantes."
        )
    })
    
    #Ao apertar o botão, concatena as opções selecionadas ao prompt 
    rval_resposta <- eventReactive(
        input$botao,
        {
                chama_gpt(
                    rval_prompt()
                )
        }
    )
    
    #Salva a resposta e imprime na tela
    output$resposta <-
        renderText({
            rval_resposta()
        })
    
    #Converter a sinopse na descrição de uma imagem pro poster
    prompt_poster <- reactive({
        chama_gpt(
            paste(
                "Trasforme a sinopse a seguir na descrição visual de um poster",
                rval_resposta()
            )
        )
    })
    
    #Gerar poster e exibi-lo
    output$poster <- 
        renderUI({
            src = 
                create_image(
                    prompt = prompt_poster(),
                    n = 1,
                    size = "512x512",
                    openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                    response_format = "url"
                )$data %>%
                unlist()
            tags$img(src = src)
        })

}
#Roda o App
shinyApp(ui = ui, server = server)