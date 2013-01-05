$ ->

  class Node extends Backbone.Model

  class NodeView extends Backbone.View
    className: 'node'
    template: _.template $('#node-template').html()

    initialize: =>
      _.bindAll @
      @render()

    render: =>
      @$el.append @template(@model.toJSON())
      @replyForm = @$el.children('.node-reply-form')
      @replyForm.hide()

      $('.node-reply-link', @$el).click @toggleReplyForm
      @replyForm.submit @submitReply

    toggleReplyForm: =>
      @replyForm.toggle()
      if @replyForm.is(':visible')
        $('textarea', @replyForm).focus()
      false

    submitReply: =>
      content = $('textarea', @replyForm).val()
      if content?
        $.ajax
          type: 'PUT'
          url: "/#{@model.get('id')}"
          data: content
          success: =>
            @replyForm.hide()
      false
  
  nodes = {}

  addNode = (id, parentId, content) ->
    parent = nodes[parentId]
    element = makeLeafElement id, content
    nodes[id] = element: element
    parent.element.append(element)

  makeLeafElement = (id, content) ->
    node = new Node id: id, content: content
    view = new NodeView model: node
    view.$el

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
    