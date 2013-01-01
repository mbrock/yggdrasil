$ ->

  renderTree = (tree) ->
    if tree.children is []
      $('<li/>').text(tree.content)
    else
      $('<ul/>')
        .append($('<span/>').text(tree.content))
        .append([renderTree(tree)[0] for tree in tree.branches])
  
  $.getJSON('/0').success (tree) ->
    $("body").append renderTree(tree)
