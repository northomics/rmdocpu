library(shiny)

shinyServer(function(input, output, session) {
  api_url <- session$registerDataObj(
    name   = 'api', # an arbitrary but unique name for the data object
    data   = list(), # you can bind some data here, which is the data argument for the
    # filter function below.
    filter = function(data, req) {
      print(ls(req))  # you can inspect what variables are encapsulated in this req
      # environment
      if (req$REQUEST_METHOD == "GET") {
        # handle GET requests
        query <- parseQueryString(req$QUERY_STRING)
        # say:
        # name <- query$name
        # etc...
      }

      if (req$REQUEST_METHOD == "POST") {
        # handle POST requests here

        reqInput <- req$rook.input

        # read a chuck of size 2^16 bytes, should suffice for our test
        buf <- reqInput$read(2^16)

        # simply dump the HTTP request (input) stream back to client
        shiny:::httpResponse(
          200, 'text/plain', buf
        )
      }
    }
  )

  # because the API entry is UNIQUE, we need to send it to the client
  # we can create a custom pipeline to convey this message
  session$sendCustomMessage("api_url", list(url=api_url))

})
