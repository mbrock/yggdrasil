define ['jquery', 'bootstrap', 'backbone', 'underscore',
        'cs!ygg/app'],
  ($, Bootstrap, Backbone, _, YggApp) -> $ ->

    class UserCardView extends Backbone.View
      el: '#sidebar-user-card'
      template: _.template($('#user-card-template').html())

      initialize: () =>
        YggApp.on 'change:userId', @render

      render: () =>
        user = @getUser()
        gravatarUrl = "http://www.gravatar.com/avatar/#{user.gravatarHash}"
        gravatarUrl += "?s=72&d=monsterid"

        @$el.html @template(_.extend(user,
          gravatarUrl: gravatarUrl)) if @loggedIn()
        return @

      loggedIn: () =>
        YggApp.get('userId') isnt undefined

      getUser: () =>
        userId = YggApp.get('userId')
        if userId
          YggApp.get('users')[userId].toJSON()

    userCardView = new UserCardView

    return {
      View: userCardView
    }