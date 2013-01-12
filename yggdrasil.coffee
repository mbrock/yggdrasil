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

    getUser: () =>
      app.get('users')[@model.get('userId')]

    addChild: (child) =>
      childView = new NodeView model: child
      @model.lastNode().view.$el.after childView.$el

    render: =>
      htmlContent = new Showdown.converter().makeHtml(@model.get('content'))
      user = @getUser().toJSON()
      gravatarUrl = "http://www.gravatar.com/avatar/#{user.gravatarHash}"
      gravatarUrl += "?s=48&d=monsterid"
      @$el.append @template(_.extend(@model.toJSON(),
        htmlContent: htmlContent,
        user: user,
        gravatarUrl: gravatarUrl))
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

  processEvent = (event) ->
    switch event.eventType
      when 'NodeAdded' then addNode event
      when 'UserRegistered' then addUser event
      when 'UserNameSet' then setUserName event
      when 'UserGravatarHashSet' then setUserGravatarHash event

  addUser = (event) ->
    app.get('users')[event.aggregateId] = new Backbone.Model
      userName: 'unnamed'
      gravatarHash: CryptoJS.MD5(event.aggregateId)
      hasCustomGravatarHash: false

  setUserName = (event) ->
    user = app.get('users')[event.aggregateId]
    user.set userName: event.userName
    unless user.get 'hasCustomGravatarHash'
      user.set gravatarHash: CryptoJS.MD5(event.userName)

  setUserGravatarHash = (event) ->
    user = app.get('users')[event.aggregateId]
    user.set
      gravatarHash: event.gravatarHash
      hasCustomGravatarHash: true

  addNode = (nodeInfo) ->
    parent = nodes[nodeInfo.parentId]
    leaf = makeLeaf parent, nodeInfo.aggregateId, nodeInfo.content,
      nodeInfo.userId, nodeInfo.creationDate
    nodes[nodeInfo.aggregateId] = leaf
    parent.addBranch leaf

  makeLeaf = (parent, id, content, userId, creationDate) ->
    new Node
      parent: parent,
      nodeId: id,
      content: content,
      userId: userId,
      creationDate: creationDate

  rootId = '1cb24849-2565-40eb-9b41-ea65daa6b271'

  app = new App
  document.yggdrasil = app
  app.set users:
    '478de2d4-b41d-47fa-9ce8-3934e412a61b':
      new Backbone.Model
        userName: 'mbrock'
        gravatarHash: 'f1640f0869d52c8fb9f5554d960f7eb0'

  rootNode = makeLeaf null, rootId,
    '', '478de2d4-b41d-47fa-9ce8-3934e412a61b', "2013-01-01T00:00:00.000Z"
  
  $("#tree").append(new NodeView(model: rootNode).el)
  nodes[rootId] = rootNode

  $.getJSON "/history", (data) ->
        
    processEvent event for event in data

    socket = new WebSocket("ws://#{location.hostname}:8080")
    socket.onopen = (event) ->
      socket.onmessage = (event) ->
        processEvent (JSON.parse event.data)

  $(".login-button").click ->
    username = $("#login-container input").val()
    $.ajax
      type: 'POST'
      dataType: 'json'
      url: "/login/#{username}"
      success: (sessionId) ->
        finishLoggingInAs username, sessionId
    false

  $(".register-button").click ->
    username = $("#login-container input").val()
    $.ajax
      type: 'POST'
      dataType: 'json'
      url: "/register/#{username}"
      success: (sessionId) ->
        finishLoggingInAs username, sessionId
    false

  finishLoggingInAs = (username, sessionId) ->
    $("#login-container").empty()
    $("#login-container").append(
      $("<p class=\"navbar-text\">Logged in as <i>#{username}</i></p>"))
      
    app.set sessionId: sessionId