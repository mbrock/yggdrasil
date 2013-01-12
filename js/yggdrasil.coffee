define ['jquery', 'bootstrap',
        'backbone', 'underscore',
        'moment', 'showdown', 'md5',

        'cs!ygg'],
        
  ($, Bootstrap, Backbone, _, moment, Showdown, CryptoJS, Ygg) -> $ ->  

    rootId = '1cb24849-2565-40eb-9b41-ea65daa6b271'
    rootUserId = '478de2d4-b41d-47fa-9ce8-3934e412a61b'
    rootDate = "2013-01-01T00:00:00.000Z"
  
    Ygg.App.set users:
      '478de2d4-b41d-47fa-9ce8-3934e412a61b':
        new Backbone.Model
          userName: 'mbrock'
          gravatarHash: 'f1640f0869d52c8fb9f5554d960f7eb0'

    rootNode = Ygg.EventProcessor.addRootNode rootId, rootUserId, rootDate
      
    $("#tree").append(new Ygg.Node.View(model: rootNode).el)
  
    $.getJSON "/history", (data) ->
          
      Ygg.EventProcessor.process event for event in data
  
      socket = new WebSocket("ws://#{location.hostname}:8080")
      socket.onopen = (event) ->
        socket.onmessage = (event) ->
          Ygg.EventProcess.process (JSON.parse event.data)
  
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