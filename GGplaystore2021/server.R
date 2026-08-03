# server.R — restored from https://hopesmasher1118.shinyapps.io/GGplaystore2021/

server <- function(input, output, session) {
  # Filters shared by range / checkbox inputs (category optional)
  apply_common_filters <- function(df) {
    req(
      input$rating, input$price, input$`install.count`,
      input$performance, input$`rating.count`, input$app_size
    )

    ad_vals <- as.logical(input$ad.supported)
    iap_vals <- as.logical(input$`in.app.purchases`)
    free_vals <- as.logical(input$free_or_not)
    editor_vals <- as.logical(input$`editors.choice`)

    if (length(ad_vals) == 0 || length(iap_vals) == 0 ||
        length(free_vals) == 0 || length(editor_vals) == 0) {
      return(df[0, , drop = FALSE])
    }

    df %>%
      filter(
        Rating >= input$rating[1], Rating <= input$rating[2],
        Price >= input$price[1], Price <= input$price[2],
        Install.Count >= input$`install.count`[1],
        Install.Count <= input$`install.count`[2],
        Performance >= input$performance[1],
        Performance <= input$performance[2],
        Rating.Count >= input$`rating.count`[1],
        Rating.Count <= input$`rating.count`[2],
        Size.MB >= input$app_size[1], Size.MB <= input$app_size[2],
        Ad.Supported %in% ad_vals,
        In.App.Purchases %in% iap_vals,
        FreeOrNot %in% free_vals,
        Editors.Choice %in% editor_vals
      )
  }

  # Category-specific reactive (value boxes, performance tables, etc.)
  filtered <- reactive({
    req(input$category)
    apply_common_filters(playstore) %>%
      filter(as.character(Category) == input$category)
  })

  # Cross-category reactive (overview plots / revenue by category)
  filtered_all_categories <- reactive({
    apply_common_filters(playstore)
  })


  # ---- Value boxes ----
  output$ibox1 <- renderValueBox({
    valueBox(
      value = nrow(filtered()),
      subtitle = "QUANTITY",
      icon = icon("list"),
      color = "navy"
    )
  })

  output$ibox2 <- renderValueBox({
    avg <- mean(filtered()$Rating, na.rm = TRUE)
    valueBox(
      value = ifelse(is.nan(avg), 0, round(avg, 1)),
      subtitle = "AVERAGE RATING",
      icon = icon("credit-card"),
      color = "orange"
    )
  })

  output$ibox3 <- renderValueBox({
    valueBox(
      value = format(sum(filtered()$Install.Count, na.rm = TRUE), scientific = FALSE),
      subtitle = "INSTALLATIONS",
      icon = icon("hdd"),
      color = "green"
    )
  })

  # ---- General View ----
  output$DefaultPlot <- renderPlot({
    df <- filtered_all_categories()
    validate(need(nrow(df) > 0, "No data"))
    ggplot(df, aes(x = log10(Install.Count + 1), y = Price, color = Rating)) +
      geom_point(alpha = 0.35, size = 1) +
      scale_color_gradient(low = "#c6dbef", high = "#08306b") +
      labs(x = "installations (log10)", y = "price", color = "Rating") +
      theme_minimal()
  })

  output$DefaultTable <- renderDT({
    datatable(
      filtered_all_categories() %>%
        select(
          App.Name, Category, Price, Install.Count, Rating, Rating.Count,
          Size.MB, FreeOrNot, Editors.Choice, Released.Date, Last.Updated,
          Content.Rating, Ad.Supported, In.App.Purchases, Install.Level, Size
        ),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$CategoryPlot <- renderPlot({
    df <- filtered_all_categories()
    validate(need(nrow(df) > 0, "No data"))
    ggplot(df, aes(x = Category)) +
      geom_bar(fill = "#2c3e50") +
      labs(title = "App Categories", x = "category", y = "count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
  })

  output$FreeOrNotPlot <- renderPlot({
    df <- filtered_all_categories()
    validate(need(nrow(df) > 0, "No data"))
    counts <- df %>%
      count(FreeOrNot) %>%
      mutate(pct = n / sum(n), label = paste0(FreeOrNot))
    ggplot(counts, aes(x = "", y = n, fill = factor(FreeOrNot))) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Free / Paid", fill = "FreeOrNot", x = NULL, y = NULL) +
      theme_void() +
      scale_fill_manual(values = c("FALSE" = "#e67e85", "TRUE" = "#1abc9c"))
  })

  # ---- Basic Information ----
  output$app_category <- renderPrint({
    input$category
  })

  output$RatingPlot <- renderPlot({
    df <- filtered()
    validate(need(nrow(df) > 1, "No data"))
    ggplot(df, aes(x = Rating)) +
      geom_density(fill = "#3498db", alpha = 0.4, color = "#2c3e50") +
      labs(x = "rating", y = "density") +
      theme_minimal()
  })

  output$InstallationPlot <- renderPlot({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data"))
    ggplot(df, aes(x = log10(Install.Count + 1))) +
      geom_histogram(bins = 40, fill = "#2c3e50", color = "white") +
      labs(x = "installations (log10)", y = "count") +
      theme_minimal()
  })

  output$PricePlot <- renderPlot({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data"))
    ggplot(df, aes(x = seq_len(nrow(df)), y = Price)) +
      geom_point(alpha = 0.4, size = 1, color = "#2c3e50") +
      labs(x = "price", y = "Price") +
      theme_minimal()
  })

  output$PricePaidPlot <- renderPlot({
    df <- filtered() %>% filter(FreeOrNot == FALSE, Price > 0)
    validate(need(nrow(df) > 1, "No paid apps in current filter"))
    # remove extreme price outliers (top 1%)
    cap <- quantile(df$Price, 0.99, na.rm = TRUE)
    df <- df %>% filter(Price <= cap)
    ggplot(df, aes(x = Price)) +
      geom_density(fill = "#9b59b6", alpha = 0.4, color = "#2c3e50") +
      labs(x = "price paid (without outliers)", y = "density") +
      theme_minimal()
  })

  output$RatingCountPlot <- renderPlot({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data"))
    ggplot(df, aes(x = log10(Rating.Count + 1))) +
      geom_histogram(bins = 40, fill = "#2c3e50", color = "white") +
      labs(x = "rating counts (log10)", y = "count") +
      theme_minimal()
  })

  output$SizePlot <- renderPlot({
    df <- filtered() %>% filter(Size.MB > 0)
    validate(need(nrow(df) > 1, "No data"))
    ggplot(df, aes(x = Size.MB)) +
      geom_density(fill = "#16a085", alpha = 0.4, color = "#2c3e50") +
      labs(x = "app size (mb)", y = "density") +
      theme_minimal()
  })

  output$PerformancePlot <- renderPlot({
    df <- filtered() %>% filter(Performance > 0)
    validate(need(nrow(df) > 1, "No data"))
    ggplot(df, aes(x = Performance)) +
      geom_density(fill = "#e67e22", alpha = 0.4, color = "#2c3e50") +
      scale_x_log10(labels = scales::comma) +
      labs(x = "performance (>0)", y = "density") +
      theme_minimal()
  })

  # ---- Value Analysis ----
  output$RevenuePlot <- renderPlot({
    df <- filtered_all_categories() %>%
      group_by(Category) %>%
      summarise(Exp.Revenue = sum(Exp.Revenue, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Exp.Revenue)) %>%
      mutate(Category = factor(Category, levels = Category))
    validate(need(nrow(df) > 0, "No data"))
    ggplot(df, aes(x = Category, y = Exp.Revenue)) +
      geom_col(fill = "#2c3e50") +
      labs(x = "category", y = "expected revenue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
  })

  output$InAppPlot <- renderPlot({
    df <- filtered_all_categories() %>%
      group_by(Ad.Supported, In.App.Purchases) %>%
      summarise(Exp.Revenue = sum(Exp.Revenue, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No data"))
    ggplot(df, aes(x = factor(Ad.Supported), y = Exp.Revenue, fill = factor(In.App.Purchases))) +
      geom_col(position = "stack") +
      labs(
        x = "Ad Supported",
        y = "expected revenue",
        fill = "In.App.Purchases"
      ) +
      scale_fill_manual(values = c("FALSE" = "#e67e85", "TRUE" = "#1abc9c")) +
      theme_minimal()
  })

  # ---- Performance ----
  output$EditorsChoiceTable <- renderDT({
    datatable(
      filtered() %>%
        filter(Editors.Choice == TRUE) %>%
        select(
          App.Name, Category, Rating, Install.Count, Performance, FreeOrNot
        ),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  top10 <- reactive({
    filtered() %>%
      arrange(desc(Performance)) %>%
      slice_head(n = 10) %>%
      select(App.Name, Performance)
  })

  output$Top10AppTable <- renderDT({
    datatable(
      top10(),
      options = list(pageLength = 10, dom = "t", scrollX = TRUE),
      rownames = TRUE
    )
  })

  output$Top10AppPlot <- renderPlot({
    df <- top10()
    validate(need(nrow(df) > 0, "No data"))
    df <- df %>%
      mutate(App.Name = factor(App.Name, levels = App.Name))
    ggplot(df, aes(x = App.Name, y = Performance)) +
      geom_col(fill = "#2c3e50") +
      labs(x = "apps", y = "performance") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))
  })

  output$BestRatingPlot <- renderPlot({
    df <- filtered() %>% filter(Rating > 4)
    validate(need(nrow(df) > 0, "No apps with rating over 4.0"))
    ggplot(df, aes(x = Rating, fill = factor(FreeOrNot))) +
      geom_histogram(bins = 8, position = "stack", color = "white") +
      labs(x = "app rating over: 4.0", y = "count", fill = "factor(FreeOrNot)") +
      scale_fill_manual(values = c("FALSE" = "#e67e85", "TRUE" = "#1abc9c")) +
      theme_minimal()
  })

  output$BestInstallationPlot <- renderPlot({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data"))
    cutoff <- quantile(df$Install.Count, 0.75, na.rm = TRUE)
    df <- df %>% filter(Install.Count >= cutoff)
    validate(need(nrow(df) > 0, "No apps over 75% installs"))
    ggplot(df, aes(x = cut(Install.Count, breaks = 8), fill = factor(FreeOrNot))) +
      geom_bar(position = "stack") +
      labs(x = "app installation over 75%", y = "count", fill = "factor(FreeOrNot)") +
      scale_fill_manual(values = c("FALSE" = "#e67e85", "TRUE" = "#1abc9c")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
  })

  output$TrashAppTable <- renderDT({
    datatable(
      filtered() %>%
        filter(Performance < 3000) %>%
        select(
          App.Name, Category, Rating, Install.Count, Performance,
          FreeOrNot, Editors.Choice
        ),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # ---- Statistic Facts ----
  output$summary <- renderPrint({
    df <- filtered() %>%
      select(Price, Rating, Rating.Count, Size.MB, Install.Count)
    print(summary(df))
  })

  output$CorHeatmap <- renderPlot({
    df <- filtered() %>%
      select(Price, Rating, Install.Count, Rating.Count, Size.MB, Performance) %>%
      filter(complete.cases(.))
    validate(need(nrow(df) > 2, "Not enough data for correlation"))
    cm <- cor(df, use = "pairwise.complete.obs")
    cm_long <- as.data.frame(as.table(cm))
    names(cm_long) <- c("Var1", "Var2", "value")
    ggplot(cm_long, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#08306b", high = "#deebf7", limits = c(-1, 1)) +
      labs(x = NULL, y = NULL, fill = "value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}
