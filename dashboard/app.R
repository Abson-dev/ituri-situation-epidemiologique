# Tableau de bord de surveillance de l'Ituri.
# Mêmes fonctions de nettoyage et d'indicateurs que les rapports : mêmes chiffres partout.
# Les fichiers de dashboard/R/ sont chargés automatiquement par Shiny.

library(shiny)
library(bslib)

for (f in list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

verifier_auth()
APP <- charger_app()
LOGO <- logo_msf_src()

theme <- bs_theme(
  version = 5, primary = "#c40000", secondary = "#4d4f53", bg = "#ffffff", fg = "#17191c",
  base_font = font_collection("Segoe UI", "system-ui", "-apple-system", "sans-serif"),
  "font-size-base" = "0.92rem", "border-radius" = "0.5rem"
)

ui <- proteger_ui(page_navbar(
  id = "page",
  title = marque(LOGO),
  window_title = "Ituri · Surveillance épidémiologique",
  theme = theme,
  fillable = FALSE,
  header = tags$head(tags$link(rel = "stylesheet", href = "style.css")),
  sidebar = sidebar(
    width = 250, gap = "0.5rem",
    # Sur écran étroit, le sélecteur de semaine reste accessible au-dessus du contenu.
    open = list(desktop = "open", mobile = "always-above"),
    if (APP$demo) div(class = "bandeau-demo", "DONNÉES SYNTHÉTIQUES : aucune valeur n'est réelle"),
    selectInput("semaine", "Semaine de référence", choices = choix_semaines(APP), selected = APP$S_max),
    uiOutput("periode"),
    hr(),
    downloadButton("rapport", "Rapport Word", class = "btn-primary w-100"),
    div(class = "note", "Projet à relire avant diffusion."),
    hr(),
    div(class = "note",
        strong("Source : "), APP$ids$fichier, br(),
        strong("Empreinte : "), substr(APP$ids$sha256, 1, 12), "…", br(),
        strong("Données jusqu'à : "), paste0("S", APP$S_max), br(),
        strong("Années passées : "), if (length(APP$historique)) paste(names(APP$historique), collapse = ", ") else "non chargées")
  ),
  nav_panel("Vue d'ensemble", icon = bsicons::bs_icon("speedometer2"), mod_apercu_ui("apercu")),
  nav_panel("Maladies", icon = bsicons::bs_icon("activity"), mod_maladies_ui("maladies")),
  nav_panel("Années", icon = bsicons::bs_icon("calendar-range"), mod_annees_ui("annees", APP)),
  nav_panel("Zones de santé", icon = bsicons::bs_icon("geo-alt"), mod_zones_ui("zones")),
  nav_panel("Paludisme", icon = bsicons::bs_icon("droplet"), mod_paludisme_ui("paludisme")),
  nav_panel("Ebola", icon = bsicons::bs_icon("shield-exclamation"), mod_ebola_ui("ebola")),
  nav_panel("Qualité des données", icon = bsicons::bs_icon("clipboard-check"), mod_qualite_ui("qualite")),
  nav_panel("Analyse d'impact", icon = bsicons::bs_icon("graph-down"), mod_analyse_ui("analyse")),
  nav_panel("Données", icon = bsicons::bs_icon("table"), mod_donnees_ui("donnees"))
), logo = LOGO)

server <- function(input, output, session) {
  proteger_serveur()
  S <- reactive(as.integer(input$semaine))

  # Comme le rapport : seules les données disponibles jusqu'à S sont visibles, règles de rupture comprises.
  ids_S <- reactive({
    x <- APP$ids
    x$donnees <- dplyr::filter(APP$ids$donnees, semaine <= S())
    x$ruptures <- detecter_ruptures_palu(x$donnees, APP$cfg)
    x
  })

  output$periode <- renderUI({
    deb <- date_semaine(S(), APP$cfg)
    div(class = "note", sprintf("Du %s au %s", format(deb, "%d/%m/%Y"), format(deb + 6, "%d/%m/%Y")))
  })

  output$rapport <- downloadHandler(
    filename = function() sprintf("Rapport_Ituri_S%02d.docx", S()),
    content = function(fichier) {
      withProgress(message = "Génération du rapport…", value = 0.3, {
        sortie <- rmarkdown::render(
          here::here("rapports/rapport_hebdomadaire.Rmd"), output_file = basename(tempfile(fileext = ".docx")),
          output_dir = tempdir(), params = list(semaine = S()), envir = new.env(parent = globalenv()),
          quiet = TRUE, knit_root_dir = here::here("rapports")
        )
        file.copy(sortie, fichier, overwrite = TRUE)
      })
    }
  )

  mod_apercu_server("apercu", APP, S, ids_S)
  mod_maladies_server("maladies", APP, S, ids_S)
  mod_annees_server("annees", APP, S)
  mod_zones_server("zones", APP, S, ids_S)
  mod_paludisme_server("paludisme", APP, S, ids_S)
  mod_ebola_server("ebola", APP, S, ids_S)
  mod_qualite_server("qualite", APP, S, ids_S)
  mod_analyse_server("analyse", APP)
  mod_donnees_server("donnees", APP, S, ids_S)
}

shinyApp(ui, server)
