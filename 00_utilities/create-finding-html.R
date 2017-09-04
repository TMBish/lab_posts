create_finding = function(finding_text) {
  
  html = 
glue::glue("<br><br><div class='finding'>
  
<div class='sidebar'>
  <img src='https://github.com/TMBish/lab_posts/blob/master/00_utilities/lightbulb.png?raw=true'>
  <span> Finding!</span>
</div>

<div class='body'> 
  <span> {finding_text} </span> 
</div>
  
</div><br><br>")
  
  return(html)
  
}
