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
* limited attribute support
  * must be string literals
    * must be enclosed in double quotes (single not allowed)
    * must have trailing comma
    * (optionally with interpolations)
* doctype support
* limited error handling
* simple indentation detection
  * supports tab or any number of spaces based on the first indented line

### v0.1.1
* comment support
* full tag support
  * inline nested tags
  * self closing tags
* good attributes support
  * will not require (or allow) trailing comma
  * will accept only matching single or double quotes
* good error handling
* full indentation checking and validation
* 
### v0.2
* inheritance, include and mixin support

### v0.3
* nested object interpolation
* iteration, conditional, and case support

### v0.4
* full javascript support
* full attribute support

### v1.0 feature compatible with jade language when configured to be
