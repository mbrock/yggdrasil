$ ->

  makeReplyFunction = (id) ->
    () ->
      content = window.prompt('Reply', 'Write your reply')
      $.ajax
        type: 'PUT'
        url: "/#{id}"
        data: content

  nodes = {}

  addNode = (id, parentId, content) ->
    parent = nodes[parentId]
    element = makeLeafElement(id, content)
    nodes[id] = { element: element }
    parent.element.append(element)

  makeLeafElement = (id, content) ->
    $('<div/>').addClass('node')
      .append($('<div/>').addClass('controls')
        .append($('<a href="#"/>').text('⤸').click(makeReplyFunction(id))))
      .append($('<div/>').addClass('content').text(content))
      .append($('<div/>').addClass('branches'))

  nodes['0'] =
    element: makeLeafElement(0, '')

  $("body").append nodes[0].element

  socket = new WebSocket("ws://#{location.hostname}:9160")
  socket.onopen = (event) ->
    socket.onmessage = (event) ->
      [id, parentId, content] = JSON.parse event.data
      addNode(id, parentId, content)
      