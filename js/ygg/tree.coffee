define ['jquery', 'lib/backbone', 'lib/underscore', 'cs!ygg/app'],
  ($, Backbone, _, App) ->
    
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
        App.get('users')[@model.get('userId')]
  
      addChild: (child) =>
        childView = new NodeView model: child
        @model.lastNode().view.$el.after childView.$el
  
      render: =>
        htmlContent = new Showdown.converter().makeHtml(@model.get('content'))
        user = @getUser().toJSON()
        gravatarUrl = "http://www.gravatar.com/avatar/#{user.gravatarHash}"
        gravatarUrl += "?s=52&d=monsterid"
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
        if content? and App.get('sessionId')?
          $.ajax
            type: 'POST'
            url: "/#{@model.get('nodeId')}"
            data:
              content: content
              sessionId: App.get('sessionId')
            success: =>
              @replyForm.hide()
        false

    return {
      Node: Node
      Branches: Branches
      View: NodeView
    }