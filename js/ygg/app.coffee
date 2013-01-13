define ['lib/backbone'], (Backbone) ->

  class User extends Backbone.Model
  
  class App extends Backbone.Model

    initialize: =>
      @users = new Backbone.Collection model: User
    
    login: (userId, sessionId) =>
      @set \
        sessionId: sessionId,
        userId: userId
      @trigger 'logged-in'

    addUser: (model) =>
      @users.add model
      @trigger 'user-added', model

    getUserById: (id) =>
      @users.get id

    getLoggedInUserName: () =>
      @getLoggedInUser().get('userName')

    getLoggedInUser: () =>
      @users.get @get('userId')

  new App
  
