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
        lineWidth = 1, lineColor = "#011627",
        gridLineWidth = 0.2, gridLineColor = "#011627", gridLineDashStyle = "LongDash",
        title = list(style = list(fontWeight = "bold"))
      ),
      yAxis = list(
        lineWidth = 1, lineColor = "#011627",
        gridLineWidth = 0.2, gridLineColor = "#011627", gridLineDashStyle = "LongDash",
        minorGridLineWidth = 0.1, minorGridLineColor = "#011627",
        title = list(style = list(fontWeight = "bold")),
        tickColor = "#011627",
        tickLength = 10,
        minorTickLength = 5, minorTickWidth = 1
        
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

