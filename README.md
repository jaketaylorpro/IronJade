# IronJade
Implementation of the jade templating language in F#
The goal is to implement all of the jade language, but we are going to start with a modest feature set and work our way up. 
## Roadmap

### v0.1
* no javascript, conditionals, iteration
  * except single level variable environment passing
  * except string interpolation of attributes and text by variable in environment
* no inheritance, includes, or mixin support
* no comment support
* most of tag support
  * not implementing inline nested tags
  * not implementing self closing tags
  * not implementing block text
* limited attribute support
  * must be string literals (optionally with interpolations)
  
### v0.1.1
* comment support
* full tag support
  * inline nested tags
  * self closing tags
  * block text
* doctype support

### v0.2
* inheritance, include and mixin support
* iteration, conditional, and case support

### v0.3
* full javascript support
* full attribute support
...
### v1.0 feature compatible with jade language
