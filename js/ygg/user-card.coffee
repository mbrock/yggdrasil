define ['jquery', 'lib/bootstrap', 'lib/backbone', 'lib/underscore',
        'cs!ygg/app'],
  ($, Bootstrap, Backbone, _, YggApp) -> $ ->

    class UserCardView extends Backbone.View
      el: '#sidebar-user-card'
      template: _.template($('#user-card-template').html())

      initialize: () =>
        @listenTo YggApp, 'logged-in', @render

      render: () =>
        user = @getUser()

        @listenTo user, 'change', @render

        @$el.html @template(_.extend(user.toJSON(),
          gravatarUrl: @getGravatarUrl())) if @loggedIn()

        @updateHeader()
          
        return @

      loggedIn: () =>
        YggApp.get('userId') isnt undefined

      getUser: () =>
        YggApp.getLoggedInUser()

      getUserName: () =>
        @getUser().get('userName')

      getGravatarUrl: () =>
        hash = @getUser().get('gravatarHash')
        "http://www.gravatar.com/avatar/#{hash}?s=72&d=monsterid"

      updateHeader: () =>
        userName = @getUserName()
        $("#login-container").empty()
        $("#login-container").append(
          $("<p class=\"navbar-text\">Logged in as <i>#{userName}</i></p>"))

    userCardView = new UserCardView

    return {
      View: userCardView
    }