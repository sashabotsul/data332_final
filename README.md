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
1.
- Chart
```  #Salary Heat Map
  output$salaryHeatmap <- renderPlot({
    df <- adjusted_data()
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = teamID, y = factor(year), fill = adj_salary)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Salary") +
      labs(title = paste("Average Salary Heatmap -", input$selected_league),
           x = "Team", y = "Year") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
```

2.
- Chart
```  #Salary Trend Plot
  output$salaryTrendPlot <- renderPlot({
    df <- adjusted_data()
    if (nrow(df) == 0) return(NULL)
    df %>%
      group_by(year) %>%
      summarise(mean_salary = mean(adj_salary, na.rm = TRUE)) %>%
      ggplot(aes(x = year, y = mean_salary)) +
      geom_line(color = "steelblue", size = 1.2) +
      labs(title = paste("Average Salary Over Time -", input$selected_league),
           x = "Year", y = "Average Salary") +
      theme_minimal()
  })
```

3.
- Chart
```
```

4.
- Chart
```
```
