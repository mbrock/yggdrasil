define ['cs!ygg/node', 'cs!ygg/app', 'cs!ygg/event-processor'],
  (node, app, EventProcessor) ->
    Node: node
    App: app
    EventProcessor: new EventProcessor
    