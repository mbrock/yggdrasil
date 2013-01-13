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

    YggApp.on 'logged-in', ->
      userName = YggApp.getLoggedInUserName()
      $("#login-container").empty()
      $("#login-container").append(
        $("<p class=\"navbar-text\">Logged in as <i>#{userName}</i></p>"))
