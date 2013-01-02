$ ->

  makeReplyFunction = (id) -> () ->
    content = window.prompt 'Reply', 'Write your reply'
    if content?
      $.ajax
        type: 'PUT'
        url: "/#{id}"
        data: content
    false

  nodes = {}

  addNode = (id, parentId, content) ->
    parent = nodes[parentId]
    element = makeLeafElement id, content
    nodes[id] = element: element
    parent.element.append element

  makeLeafElement = (id, content) ->
    $('<div/>').addClass('node')
      .append($('<div/>').addClass('controls')
        .append($('<a href="#"/>').text('⤸').click(makeReplyFunction(id))))
      .append($('<div/>').addClass('content').text(content))
      .append($('<div/>').addClass('branches'))

  nodes['0'] = element: makeLeafElement 0, ''

  $("body").append nodes[0].element

  $.getJSON "/history", (data) ->
    addNode event... for event in data

    socket = new WebSocket("ws://#{location.hostname}:8080")
    socket.onopen = (event) ->
      socket.onmessage = (event) ->
        addNode (JSON.parse event.data)...
      