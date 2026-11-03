mod_donnees_ui <- function(id) {
  ns <- NS(id)
  page(
    entete_page("Données", "Base nettoyée, à filtrer et à télécharger", "table", uiOutput(ns("semaine"))),
    uiOutput(ns("filtres")),
    bslib::card(card_header("Données nettoyées"), card_body(fillable = FALSE, fill = FALSE, class = "corps-libre", DT::DTOutput(ns("table"), height = "auto", fill = FALSE)))
  )
}

mod_donnees_server <- function(id, app, S, ids_S) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$semaine <- rendu_puce_semaine(S, app$cfg)
    output$filtres <- renderUI({
      layout_columns(
        col_widths = c(4, 4, 4), class = "filtres",
        selectizeInput(ns("maladies"), "Maladies", choix_maladies(app), multiple = TRUE, options = list(placeholder = "Toutes")),
        selectizeInput(ns("zones"), "Zones de santé", choix_zones(app), multiple = TRUE, options = list(placeholder = "Toutes")),
        div(class = "boutons",
            downloadButton(ns("csv"), "CSV", class = "btn-outline-primary"),
            downloadButton(ns("excel"), "Excel", class = "btn-outline-primary"))
      )
    })
    filtre <- reactive({
      x <- ids_S()$donnees
      if (length(input$maladies)) x <- x[x$maladie %in% input$maladies, ]
      if (length(input$zones)) x <- x[x$zs %in% input$zones, ]
      x |> dplyr::arrange(dplyr::desc(semaine), zs, maladie) |>
        dplyr::transmute(Semaine = semaine, `Début` = format(date, "%d/%m/%Y"), Zone = nom_zone(zs), Maladie = libelle(maladie, app$cfg),
                         Cas = cas, `Décès` = deces, Population = pop, `Pour 100 000` = round(cas / pop * 1e5, 1))
    })
    # Pagination côté serveur : la base compte plus de 10 000 lignes.
    output$table <- DT::renderDT(
      DT::datatable(
        filtre(), rownames = FALSE, class = "compact hover stripe", selection = "none",
        options = list(pageLength = 25, lengthMenu = c(25, 50, 100), autoWidth = FALSE, order = list(),
                       language = list(thousands = " ", search = "Rechercher :", lengthMenu = "_MENU_ lignes", info = "_START_–_END_ sur _TOTAL_",
                                       infoEmpty = "Aucune ligne", infoFiltered = "(sur _MAX_)", zeroRecords = "Aucune donnée",
                                       paginate = list(previous = "Précédent", `next` = "Suivant")))
      ) |>
        DT::formatRound(c("Cas", "Décès", "Population"), digits = 0, mark = "\u202f") |>
        DT::formatRound("Pour 100 000", digits = 1, mark = "\u202f", dec.mark = ",") |>
        DT::formatStyle("Zone", fontWeight = "650") |>
        DT::formatStyle("Cas", fontWeight = "650", color = "#1c5cab") |>
        DT::formatStyle("D\u00e9c\u00e8s", color = DT::styleInterval(0, c("inherit", COUL_DECES)), fontWeight = DT::styleInterval(0, c("400", "700"))),
      server = TRUE
    )
    nom <- function(ext) sprintf("Ituri_donnees_S%02d.%s", S(), ext)
    output$csv <- downloadHandler(filename = function() nom("csv"), content = function(f) readr::write_excel_csv2(filtre(), f))
    output$excel <- downloadHandler(filename = function() nom("xlsx"), content = function(f) writexl::write_xlsx(filtre(), f))
  })
}
