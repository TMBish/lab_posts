high_tmbish = function() {
  
  out = 
    hc_theme(
      chart = list(
        backgroundColor = "#FDFFFC",
        style = list(
          fontFamily = "Grand Hotel"
        )
      ),
      colors = list("#FF9F1C", "#2EC4B6", "#FE5F55", "#FE5F55"),
      title = list(
        style = list(fontWeight = "bold", color = "#414141"),
        align = "left"
      ),
      xAxis = list(
        gridLineColor = "#011627",
        lineColor = "#011627",
        gridLineWidth = 0.1,
        lineWidth = 1,
        title = list(style = list(fontWeight = "bold"))
      ),
      yAxis = list(
        gridLineColor = "#011627",
        lineColor = "#011627",
        gridLineWidth = 0.1,
        lineWidth = 1,
        title = list(style = list(fontWeight = "bold"))
      ),
      subtitle = list(
        style = list(fontStyle = "italic", color ="#414141"), 
        align = "left"
      ),
      tooltip = list(
        borderWidth = 0,
        shape = "square",
        valueDecimals = 2
      ),
      plotOptions = list(
        line = list(marker = list(symbol = "circle", lineWidth = 2, radius = 5)),
        spline = list(marker = list(symbol = "circle", lineWidth = 2, radius = 5))
      ),
      legend = list(
        align = "right",
        # floating = TRUE,
        layout = "vertical",
        backgroundColor = "#FFF",
        shadow = TRUE,
        title = "Legend",
        verticalAlign = "middle"
      ),
      exporting = list(
        enabled = TRUE
      ),
      credits = list(
        enabled = TRUE,
        href = "https://github.com/TMBish/lab_posts/blob/master/highcharts_theme.R",
        text = "My schmick highcharts theme"
      )
      
    )
  
}
