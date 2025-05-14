# 🏀 ⚾ Sports Salaries ⚾ 🏀
<p> by: Sasha Botsul, Tanner Buol, Logan Farley </p>

---
## 🎉 Shiny App 🎉
link

---

### This page shows snippets of our code during the process of cleaning our sport salary data, creating charts and the shiny app.

---
## 🫧 Data Cleaning 🫧
1. Importing MLB data, renaming columns, and removing columns
```
```

2. Fixing MLB team names and applying to dataframe
```
```

3. Importing NBA data, renaming columns, and removing NAs
```
```

4. Fixing NBA team names and applying to dataframe
```
```

5. Adding sport column
```
```

6. Combining datasets
```
```

## Creating our charts 📊
1. Salary Trend Plot Line
- Chart
```
output$salaryTrendPlot <- renderPlot({
    df <- adjusted_data()
    if (nrow(df) == 0) return(NULL)
    df %>%
      group_by(Year) %>%
      summarise(mean_salary = mean(adj_salary, na.rm = TRUE)) %>%
      ggplot(aes(x = Year, y = mean_salary)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      labs(title = paste("Average Salary Over Time -", input$selected_league),
           x = "Year", y = "Average Salary") +
      scale_y_continuous(labels = label_comma()) +
      theme_minimal()
  })
```

2. Salary Heat Map
- Chart
```
output$salaryHeatmap <- renderPlot({
    df <- adjusted_data()
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = teamID, y = factor(Year), fill = adj_salary)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Salary", labels = label_comma()) +
      labs(title = paste("Average Salary Heatmap -", input$selected_league),
           x = "Team", y = "Year") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })  
```

3.
- Chart
```
output$salary_by_year_plot <- renderPlot({
    req(input$salary_metric, input$selected_league)
    
    df <- adjusted_data() 
    if (nrow(df) == 0) return(NULL)
    
    salary_col <- if (input$salary_metric == "Average Salary") {
      "mean_salary"
    } else {
      "median_salary"
    }
    
    df <- df %>%
      group_by(Year, teamID) %>%
      summarise(salary = mean(.data[[salary_col]], na.rm = TRUE), .groups = "drop")
    
    ggplot(df, aes(x = Year, y = salary, fill = teamID)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      geom_point() +
      facet_wrap(~teamID, scales = 'free', ncol = 4)+
      labs(title = paste(input$salary_metric, "by Team for", input$selected_league),
           x = "Year", y = input$salary_metric) +
      scale_y_continuous(labels = label_comma())+
      theme_minimal()+
      theme(legend.position = "none")
    
  })
```

4.
- Chart
```
```
