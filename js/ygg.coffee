define ['cs!ygg/tree', 'cs!ygg/app', 'cs!ygg/event-processor'],
  (tree, app, EventProcessor) ->
    Tree: tree
    App: app
    EventProcessor: new EventProcessor
    