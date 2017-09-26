# colourpicker 1.0

2017-09-27

MAJOR NEW FEATURES

- `colourInput()` now supports an alpha channel, to allow selecting semi-transparent colours, using the `allowTransparent` boolean parameter. Note that this is a **BREAKING CHANGE** because previously `allowTransparent=TRUE` resulted in a checkbox, and now it displays the alpha opacity selector.  
- The value of a colour in `colourInput()` can be specified either using a colour name ("blue"), HEX codes ("#0000FF"), RGB codes ("rgb(0, 0, 255)"), or HSL codes ("hsl(240, 100, 50)")
- It is now possible to type the value of a colour (using any of the above versions) directly into the input field. For example, you can type "rgb(0, 0, 255)" directly into the input field to select the green colour.

BREAKING CHANGES

- The behaviour of `allowTransparent=TRUE` has been modified, as mentioned above
- The `transparentText` parameter from `colourInput()` has been removed

BUG FIXES

- Fix issue #7: setting a colourinput to background-only and then back to text does not let you click into the input

# colourpicker 0.3

2016-12-05

- Added an awesome `plotHelper()` gadget+addin that makes it easy to pick colours in a plot and see in real time the updated plot as you choose new colours (#1)
- Added keyboard shortcuts for `colourPicker()` (left/right arrows to navigate the colours, 1-9 to select a colour, spacebar to add a colour...)
- don't error out if a HEX value containing alpha transparency is passed to a `colourInput()` (#4 - thanks @ddiez)

# colourpicker 0.2.1

2016-10-31

- Slight changes to colour picker gadget UI

# colourpicker 0.2

2016-09-06

- Fix vignette source to have an output (CRAN reminded me to do this) 

# colourpicker 0.1.1

2016-08-15

- upgrade to newer version of JS library that fixed bugs with new jquery
- add `runExample()` function to run the example shiny app


# colourpicker 0.1

2016-08-11

- initial version (mostly copied over from `shinyjs` package)
