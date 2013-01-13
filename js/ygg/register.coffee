define ['jquery', 'cs!ygg/app'], ($, YggApp) ->
  $ ->
    
    $(".register-button").click ->
      username = $("#login-container input").val()
      
      $.ajax
        type: 'POST'
        dataType: 'json'
        url: "/register/#{username}"
        success: ([sessionId, userId]) ->
          YggApp.on 'user-added', (user) ->
            if user.id is userId
              YggApp.login userId, sessionId
          
      false
  