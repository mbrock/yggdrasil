define ['cs!ygg/app', 'cs!ygg/tree', 'cs!ygg/notify',
        'lib/backbone', 'lib/md5'],
 (YggApp, YggTree, YggNotify, Backbone, CryptoJS) ->
  
  class EventProcessor
    constructor: () ->
      @nodes = {}

    process: (event) =>
      YggNotify.show "Processing event: #{event.eventType}"
      switch event.eventType
        when 'NodeAdded' then @addNode event
        when 'UserRegistered' then @addUser event
        when 'UserNameSet' then @setUserName event
        when 'UserGravatarHashSet' then @setUserGravatarHash event

    addUser: (event) =>
      YggApp.addUser (new Backbone.Model
        id: event.aggregateId
        userName: 'unnamed'
        gravatarHash: CryptoJS.MD5(event.aggregateId)
        hasCustomGravatarHash: false)

    setUserName: (event) =>
      user = YggApp.getUserById event.aggregateId
      user.set userName: event.userName
      unless user.get 'hasCustomGravatarHash'
        user.set gravatarHash: CryptoJS.MD5(event.userName)
  
    setUserGravatarHash: (event) =>
      user = YggApp.getUserById event.aggregateId
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
      