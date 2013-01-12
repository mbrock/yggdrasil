define ['jquery', 'bootstrap',
        'backbone', 'underscore',
        'moment', 'showdown', 'md5',

        'cs!ygg'],
        
  ($, Bootstrap, Backbone, _, moment, Showdown, CryptoJS, Ygg) -> $ ->  
    
    nodes = {}
  
    processEvent = (event) ->
      switch event.eventType
        when 'NodeAdded' then addNode event
        when 'UserRegistered' then addUser event
        when 'UserNameSet' then setUserName event
        when 'UserGravatarHashSet' then setUserGravatarHash event
  
    addUser = (event) ->
      Ygg.App.get('users')[event.aggregateId] = new Backbone.Model
        userName: 'unnamed'
        gravatarHash: CryptoJS.MD5(event.aggregateId)
        hasCustomGravatarHash: false
  
    setUserName = (event) ->
      user = Ygg.App.get('users')[event.aggregateId]
      user.set userName: event.userName
      unless user.get 'hasCustomGravatarHash'
        user.set gravatarHash: CryptoJS.MD5(event.userName)
  
    setUserGravatarHash = (event) ->
      user = Ygg.App.get('users')[event.aggregateId]
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
      new Ygg.Node.Node
        parent: parent,
        nodeId: id,
        content: content,
        userId: userId,
        creationDate: creationDate
  
    rootId = '1cb24849-2565-40eb-9b41-ea65daa6b271'
  
    document.yggdrasil = Ygg.App
    Ygg.App.set users:
      '478de2d4-b41d-47fa-9ce8-3934e412a61b':
        new Backbone.Model
          userName: 'mbrock'
          gravatarHash: 'f1640f0869d52c8fb9f5554d960f7eb0'
  
    rootNode = makeLeaf null, rootId,
      '', '478de2d4-b41d-47fa-9ce8-3934e412a61b', "2013-01-01T00:00:00.000Z"
    
    $("#tree").append(new Ygg.Node.View(model: rootNode).el)
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
        
      Ygg.App.set sessionId: sessionId