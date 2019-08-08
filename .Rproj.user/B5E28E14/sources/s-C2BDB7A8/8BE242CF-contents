library(shiny)

shinyUI(fluidPage(
  singleton(tags$head(HTML(
    '
  <script type="text/javascript">
    $(document).ready(function() {
      // creates a handler for our special message type
      Shiny.addCustomMessageHandler("api_url", function(message) {
        // set up the the submit URL of the form
        $("#form1").attr("action", "/" + message.url);
        $("#submitbtn").click(function() { $("#form1").submit(); });
      });
    })
  </script>
'
  ))),
  tabsetPanel(
    tabPanel('POST request example',
             # create a raw HTML form
             HTML('
<form enctype="multipart/form-data" method="post" action="" id="form1">
    <span>Name:</span>
    <input type="text" name="name" /> <br />
    <span>Passcode: </span> <br />
    <input type="password" name="passcode" /><br />
    <span>Avatar:</span>
    <input name="file" type="file" /> <br />
    <input type="button" value="Upload" id="submitbtn" />
</form>
')
    )
  )
))
