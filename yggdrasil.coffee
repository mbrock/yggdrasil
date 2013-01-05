$ ->

  class App extends Backbone.Model

  class Node extends Backbone.Model
    initialize: =>
      @branches = new Branches
      
    addBranch: (node) =>
      @branches.add node

  class Branches extends Backbone.Collection
    model: Node

  class NodeView extends Backbone.View
    className: 'node'
    template: _.template $('#node-template').html()

    initialize: =>
      @render()
      @model.branches.on 'add', @addChild

    addChild: (child) =>
      childView = new NodeView model: child
      @$el.append childView.el

    render: =>
      htmlContent = new Showdown.converter().makeHtml(@model.get('content'))
      @$el.append @template(_.extend(@model.toJSON(), htmlContent: htmlContent))
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
      if content? and app.get('sessionId')?
        $.ajax
          type: 'POST'
          url: "/#{@model.get('id')}"
          data:
            content: content
            sessionId: app.get('sessionId')
          success: =>
            @replyForm.hide()
      false
  
  nodes = {}

  addNode = (nodeInfo) ->
    parent = nodes[nodeInfo.parentId]
    leaf = makeLeaf nodeInfo.id, nodeInfo.content, nodeInfo.userId
    nodes[nodeInfo.id] = leaf
    parent.addBranch leaf

  makeLeaf = (id, content, username) ->
    new Node id: id, content: content, username: username

  rootNode = makeLeaf 0, '', 'yggdrasil'
  $("#tree").append(new NodeView(model: rootNode).el)
  nodes['0'] = rootNode

  app = new App
  document.yggdrasil = app

  $.getJSON "/history", (data) ->
    addNode event for event in data

    socket = new WebSocket("ws://#{location.hostname}:8080")
    socket.onopen = (event) ->
      socket.onmessage = (event) ->
        addNode (JSON.parse event.data)

  $("#login-container button").click ->
    username = $("#login-container input").val()
    $.ajax
      type: 'POST'
      dataType: 'json'
      url: "/login/#{username}"
      success: (sessionId) ->
        finishLoggingInAs username, sessionId
    false

  finishLoggingInAs = (username, sessionId) ->
    $("#login-container").empty()
    $("#login-container").append(
      $("<p class=\"navbar-text\">Logged in as <i>#{username}</i></p>"))
      
    app.set('sessionId', sessionId)