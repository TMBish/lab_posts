high_tmbish = function() {
  
  base_font = "Josefin Slab"
  header_font = "Arvo"
  
  header_style = list(fontFamily = header_font, fontWeight = "bold", color = "#011627")
  
  out = 
    hc_theme(
      chart = list(
        backgroundColor = "#FDFFFC",
        style = list(
          fontFamily = base_font
        ),
        shadow = TRUE
      ),
      colors = list("#FF9F1C", "#2EC4B6", "#FE5F55", "#C1E1F1", "#5FEF9B"),
      title = list(
        style = header_style,
        align = "left"
      ),
      xAxis = list(
        lineWidth = 1, lineColor = "#011627",
        gridLineWidth = 0.1, gridLineColor = "#414141", gridLineDashStyle = "LongDash",
        tickWidth = 0,
        title = list(style = header_style)
      ),
      yAxis = list(
        lineWidth = 1, lineColor = "#011627",
        gridLineWidth = 0.1, gridLineColor = "#414141", gridLineDashStyle = "LongDash",
        minorGridLineWidth = 0, minorGridLineColor = "#414141",
        # tickColor = "#011627",tickLength = 10,
        # minorTickLength = 5, minorTickWidth = 1,
        tickWidth = 0,
        title = list(style = header_style)
      ),
      subtitle = list(
        style = list(fontStyle = "italic", color ="#414141"), 
        align = "left"
      ),
      tooltip = list(
        shape = "square",
        valueDecimals = 2,
        backgroundColor = "#FFF",
        valueDecimals = 2,
        headerFormat = ""
        # footerFormat = "<span style = 'background-color: {point.color}; height = 4px; width = 100%;'> </span>"
        # formatter = JS("function() {
        #                series = this.series.name;
        #                x_var = this.series.x;
        #                x_val = this.x;
        #                y_var = this.y;
        #                y_val = this.y
        #                colour = this.y;
        #                
        #                string = '<b>' + series + '</b> <br> ' + this.series.xAxis.categories ;
        #                return(string) 
        #                }")
      ),
      plotOptions = list(
        line = list(marker = list(symbol = "circle", lineWidth = 2, radius = 5)),
        spline = list(marker = list(symbol = "circle", lineWidth = 2, radius = 5)),
        column = list(dataLabels = list(backgroundColor = "#FFF"))
      ),
      legend = list(
        align = "right",
        layout = "vertical",
        backgroundColor = "#FFF",
        shadow = TRUE,
        title = "Legend",
        verticalAlign = "middle"
      ),
      credits = list(
        href = "https://github.com/TMBish/lab_posts/blob/master/highcharts_theme.R",
        text = "TMBish's schmick highcharts theme"
      )
    )
}
