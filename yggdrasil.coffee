$ ->

  class Node extends Backbone.Model
    addBranch: (node) =>
      this.trigger 'new-child', node

  class NodeView extends Backbone.View
    className: 'node'
    template: _.template $('#node-template').html()

    initialize: =>
      _.bindAll @
      @render()
      @model.on 'new-child', (child) =>
        childView = new NodeView model: child
        @$el.append childView.el

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
    leaf = makeLeaf id, content
    nodes[id] = leaf
    parent.addBranch leaf

  makeLeaf = (id, content) ->
    new Node id: id, content: content

  rootNode = makeLeaf 0, ''
  $("#tree").append(new NodeView(model: rootNode).el)
  nodes['0'] = rootNode

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
    