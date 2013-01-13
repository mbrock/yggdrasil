define ['cs!ygg/app'], (YggApp) ->
  notify = (message) ->
    console.log message
    if window.Notification and Notification.permissionLevel
      switch Notification.permissionLevel()
        when 'default'
          Notification.requestPermission ->
            notify(message)
        when 'granted'
          new Notification 'Yggdrasil',
            body: message
  
  $ ->
    YggApp.on 'user-added', (model) ->
      notify "New user #{model.id} added"
      model.on 'change:userName', () ->
        user = model.toJSON()
        notify "User #{user.id} changed to name #{user.userName}"

    $('.yggdrasil-link').click ->
      notify 'Welcome!'
              
  return \
    show: notify
