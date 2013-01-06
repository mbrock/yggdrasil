$ ->

  class App extends Backbone.Model

  class Node extends Backbone.Model
    initialize: =>
      @branches = new Branches
      if parent = @get('parent')
        @set 'level', parent.get('level') + 1
      else
        @set 'level', 0
      
    addBranch: (node) =>
      @branches.add node

    lastNode: () =>
      if @branches.isEmpty()
        @
      else
        @branches.last().lastNode()

  class Branches extends Backbone.Collection
    model: Node

    add: (child) =>
      @trigger 'before-add', child
      super child

  class NodeView extends Backbone.View
    className: 'node'
    template: _.template $('#node-template').html()

    initialize: =>
      @render()
      @model.view = @
      @model.branches.on 'before-add', @addChild

    addChild: (child) =>
      childView = new NodeView model: child
      @model.lastNode().view.$el.after childView.$el

    render: =>
      htmlContent = new Showdown.converter().makeHtml(@model.get('content'))
      @$el.append @template(_.extend(@model.toJSON(), htmlContent: htmlContent))
      @setIndent()
      @replyForm = @$el.children('.node-reply-form')
      @replyForm.hide()

      $('.node-reply-link', @$el).click @toggleReplyForm
      @replyForm.submit @submitReply

    setIndent: () =>
      @$el.css 'margin-left', "#{@model.get('level')}em"

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
          url: "/#{@model.get('nodeId')}"
          data:
            content: content
            sessionId: app.get('sessionId')
          success: =>
            @replyForm.hide()
      false
  
  nodes = {}

  addNode = (nodeInfo) ->
    parent = nodes[nodeInfo.parentId]
    leaf = makeLeaf parent, nodeInfo.nodeId, nodeInfo.content, nodeInfo.userId
    nodes[nodeInfo.nodeId] = leaf
    parent.addBranch leaf

  makeLeaf = (parent, id, content, username) ->
    new Node parent: parent, nodeId: id, content: content, username: username

  rootNode = makeLeaf null, 0, '', 'yggdrasil'
  
  $("#tree").append(new NodeView(model: rootNode).el)
  nodes[0] = rootNode

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