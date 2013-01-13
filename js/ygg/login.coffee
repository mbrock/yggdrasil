define ['jquery', 'cs!ygg/app'], ($, YggApp) ->
  $ ->
    
    $(".login-button").click ->
      username = $("#login-container input").val()
      $.ajax
        type: 'POST'
        dataType: 'json'
        url: "/login/#{username}"
        success: ([sessionId, userId]) ->
          YggApp.login userId, sessionId
          
      false
