// Generated by CoffeeScript 1.4.0
(function() {

  $(function() {
    var addNode, makeLeafElement, makeReplyFunction, nodes, toggleReply;
    toggleReply = function(element) {
      return function() {
        var form;
        form = element.find('form :first');
        form.toggle();
        if (form.is(':visible')) {
          $('textarea', form).focus();
        }
        return false;
      };
    };
    makeReplyFunction = function(id, element) {
      return function() {
        var content;
        content = $('textarea', element).val();
        if (content != null) {
          $.ajax({
            type: 'PUT',
            url: "/" + id,
            data: content,
            success: function() {
              return $('form', element).hide();
            }
          });
        }
        return false;
      };
    };
    nodes = {};
    addNode = function(id, parentId, content) {
      var element, parent;
      parent = nodes[parentId];
      element = makeLeafElement(id, content);
      nodes[id] = {
        element: element
      };
      return parent.element.append(element);
    };
    makeLeafElement = function(id, content) {
      var element;
      element = $('<div/>');
      return element.addClass('node').append($('<div/>').addClass('controls').append($('<a href="#"/>').text('⤸').click(toggleReply(element)))).append($('<div/>').addClass('content').text(content)).append($('<form>').addClass('reply').append($('<textarea name="text" placeholder="Your reply...">').attr('rows', 5).attr('cols', 50)).append($('<input type="submit" value="Reply">')).submit(makeReplyFunction(id, element)).hide());
    };
    nodes['0'] = {
      element: makeLeafElement(0, '')
    };
    $("#tree").append(nodes[0].element);
    return $.getJSON("/history", function(data) {
      var event, socket, _i, _len;
      for (_i = 0, _len = data.length; _i < _len; _i++) {
        event = data[_i];
        addNode.apply(null, event);
      }
      socket = new WebSocket("ws://" + location.hostname + ":8080");
      return socket.onopen = function(event) {
        return socket.onmessage = function(event) {
          return addNode.apply(null, JSON.parse(event.data));
        };
      };
    });
  });

}).call(this);
