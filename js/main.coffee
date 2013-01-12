requirejs.config
  shim:
    bootstrap:
      deps: ['jquery']
      exports: '$.fn.popover'
  
    backbone:
      deps: ['underscore', 'jquery']
      exports: 'Backbone'

    md5:
      exports: 'CryptoJS'

    underscore:
      exports: '_'

define ['cs!yggdrasil'], (Yggdrasil) -> { }
