define ['cs!ygg/app', 'cs!ygg/tree', 'backbone', 'md5'], (YggApp, YggTree, Backbone, CryptoJS) ->
  
  class EventProcessor
    constructor: () ->
      @nodes = {}

    process: (event) =>
      switch event.eventType
        when 'NodeAdded' then @addNode event
        when 'UserRegistered' then @addUser event
        when 'UserNameSet' then @setUserName event
        when 'UserGravatarHashSet' then @setUserGravatarHash event

    addUser: (event) =>
      YggApp.get('users')[event.aggregateId] = new Backbone.Model
        userName: 'unnamed'
        gravatarHash: CryptoJS.MD5(event.aggregateId)
        hasCustomGravatarHash: false

    setUserName: (event) =>
      user = YggApp.get('users')[event.aggregateId]
      user.set userName: event.userName
      unless user.get 'hasCustomGravatarHash'
        user.set gravatarHash: CryptoJS.MD5(event.userName)
  
    setUserGravatarHash: (event) =>
      user = YggApp.get('users')[event.aggregateId]
      user.set
        gravatarHash: event.gravatarHash
        hasCustomGravatarHash: true
  
    addNode: (nodeInfo) =>
      parent = @nodes[nodeInfo.parentId]
      leaf = @makeLeaf parent, nodeInfo.aggregateId, nodeInfo.content,
        nodeInfo.userId, nodeInfo.creationDate
      @nodes[nodeInfo.aggregateId] = leaf
      parent.addBranch leaf
  
    makeLeaf: (parent, id, content, userId, creationDate) =>
      new YggTree.Node
        parent: parent,
        nodeId: id,
        content: content,
        userId: userId,
        creationDate: creationDate

    addRootNode: (id, userId, creationDate) =>
      @nodes[id] = @makeLeaf null, id, '', userId, creationDate
      