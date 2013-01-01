$ ->

  clickReply = ->
    alert('reply!')

  renderTree = (tree) ->
    $('<div/>').addClass('branches')
      .append($('<div/>').addClass('controls')
        .append($('<a href="#"/>').text('⤸').click(clickReply)))
      .append($('<div/>').addClass('content').text(tree.content))
      .append([renderTree(tree)[0] for tree in tree.branches])
  
  $.getJSON('/0').success (tree) ->
    $("body").append renderTree(tree)
