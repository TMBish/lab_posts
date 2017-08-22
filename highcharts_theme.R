high_tmbish = function() {
  
  out = 
    hc_theme(
      chart = list(
        backgroundColor = list("white"),
        style = list(
          fontFamily = "Droid Sans"
        )
      ),
      colors = list("#414141", "#FF6A5C", "#CCDFC8", "#056571"),
      title = list(
        style = list(fontWeight = "bold", color = "#414141"),
        align = "left"
      ),
      xAxis = list(
        gridLineColor = "#2196F3",
        lineColor = "#2196F3",
        gridLineWidth = 0.5,
        lineWidth = 0,
        title = list(style = list(fontWeight = "bold"))
      ),
      yAxis = list(
        gridLineColor = "#2196F3",
        lineColor = "#2196F3",
        gridLineWidth = 0.5,
        lineWidth = 0,
        title = list(style = list(weight = "bold"))
      ),
      subtitle = list(
        style = list(fontStyle = "italic", color ="#414141"), 
        align = "left"
      ),
      tooltip = list(
        borderWidth = 0,
        shape = "square",
        valueDecimals = 2
      )
    )
  
}
  