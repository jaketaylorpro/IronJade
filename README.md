# IronJade
Implementation of the jade templating language in F#
The goal is to implement all of the jade language, but we are going to start with a modest feature set and work our way up. 
## Issues
Please submit 
* [bug](https://github.com/jaketaylorpro/IronJade/issues/new?title=when%20%5Battempting%20behavior%5D%2C%20%5Berror%5D%20occurs%2C%20instead%20of%20%5Bexpected%5D&body=%23%23%23%20example%0A%60%60%60jade%0A%60%60%60%0A%0A%23%23%23%20error%20result%0A%60%60%60html%0A%60%60%60%0A%23%23%23%20expected%0A%60%60%60html%0A%60%60%60%0A%0A%23%23%23%20possible%20cause%0A*%20%0A%0A%23%23%23%20suggested%20fix%0A*%20)
* [feature requests](https://github.com/jaketaylorpro/IronJade/issues/new?title=in%20order%20to%20%5Bbenefit%5D%2C%20as%20a%20%5Brole%5D%2C%20i%20want%20%5Bgoal%5D) 

## Roadmap

### v0.0.9
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

### v0.1.0
* comment support
* full tag support
  * inline nested tags
  * self closing tags
* good attributes support
  * will not require (or allow) trailing comma
  * will accept only matching single or double quotes
* good error handling
* full indentation checking and validation

### v0.1.1
* asp.net mvc 5 integration

### v0.2
* inheritance, include and mixin support

### v0.3
* nested object interpolation
* iteration, conditional, and case support

### v0.4
* full javascript support
* full attribute support

### v1.0 feature compatible with jade language when configured to be so
