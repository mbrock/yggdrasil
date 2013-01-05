$ ->

  toggleReply = (element) -> () ->
    form = element.find 'form :first'
    form.toggle()
    if form.is(':visible')
      $('textarea', form).focus()
    false

  makeReplyFunction = (id, element) -> () ->
    content = $('textarea', element).val()
    if content?
      $.ajax
        type: 'PUT'
        url: "/#{id}"
        data: content
        success: ->
          $('form', element).hide()
    false

  nodes = {}

  addNode = (id, parentId, content) ->
    parent = nodes[parentId]
    element = makeLeafElement id, content
    nodes[id] = element: element
    parent.element.append(element)

  makeLeafElement = (id, content) ->
    element = $ '<div/>'
    element.addClass('node')
      .append($('<div/>').addClass('controls')
        .append($('<a href="#"/>').text('⤸').click(toggleReply(element))))
      .append($('<div/>').addClass('content').text(content))
      .append($('<form>').addClass('reply')
        .append($('<textarea name="text" placeholder="Your reply...">')
          .attr('rows', 5).attr('cols', 50))
        .append($ '<input type="submit" value="Reply">')
        .submit(makeReplyFunction(id, element))
        .hide())

  nodes['0'] = element: makeLeafElement 0, ''

  $("#tree").append nodes[0].element

  $.getJSON "/history", (data) ->
    addNode event... for event in data

    socket = new WebSocket("ws://#{location.hostname}:8080")
    socket.onopen = (event) ->
      socket.onmessage = (event) ->
        addNode (JSON.parse event.data)...

  $("#login-container button").click ->
    username = $("#login-container input").val()
    $.ajax
      type: 'POST'
      url: "/login/#{username}"
      success: ->
        finishLoggingInAs username
    false

  finishLoggingInAs = (username) ->
    $("#login-container").empty()
    $("#login-container").append(
      $("<p class=\"navbar-text\">Logged in as <i>#{username}</i></p>"))
    