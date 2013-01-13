requirejs.config
  shim:
    'lib/bootstrap':
      deps: ['jquery']
      exports: '$.fn.popover'
  
    'lib/backbone':
      deps: ['lib/underscore', 'jquery']
      exports: 'Backbone'

    'lib/md5':
      exports: 'CryptoJS'

    'lib/underscore':
      exports: '_'

define ['cs!yggdrasil'], (Yggdrasil) -> { }
