This small Shiny app is an example of how to create an app that has navigation bars, tabs, and many other options to customize within outputs, such as:

* slider inputs, 
* checkbox inputs, 
* selection inputs,
* color inputs,
* radio buttons,
* action buttons,
* conditional panels...

Regarding outputs, it shows usage for

* `reactive()` content as well as,
* `eventReactive()` (to work with `actionButton()`),
* `renderPlot()`,
* `renderPlotly()`,
* `renderTable()`

This app is made with three Rscripts:

**1. `global.R`**

This script is run at the very beginning, so anything here will be available for the `ui.R` and `server.R` files. It is a good way to clean your code and do some of the work from the start (data import, preprocessing...).

**2. `ui.R`**

This is where all the **user interface** is created. This script doesn't do any data analysis or plotting. It justs lays the format and inputs your app will get.

**3. `server.R`**

This is where **R** interacts with the user-defined inputs and computes the results. Whenever the user interacts with the app, the corresponding code that is affected will get highlighted for a couple of seconds.
