$ ->

  makeReplyFunction = (tree) ->
    () ->
      content = window.prompt('Reply', 'Write your reply')
      $.ajax
        type: 'PUT'
        url: "/#{tree.id}"
        data: content

  renderTree = (tree) ->
    $('<div/>').addClass('branches')
      .append($('<div/>').addClass('controls')
        .append($('<a href="#"/>').text('⤸').click(makeReplyFunction(tree))))
      .append($('<div/>').addClass('content').text(tree.content))
      .append([renderTree(tree)[0] for tree in tree.branches])
  
  $.getJSON('/0').success (tree) ->
    $("body").append renderTree(tree)

  socket = new WebSocket("ws://#{location.hostname}:9160")
  socket.onopen = (event) ->
    socket.onmessage = (event) ->
      alert(event.data)
      