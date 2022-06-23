# A web recipe viewer app
This app lets you view predefined recipes, or you can also enter your own by
following the recipe syntax. The app can then parse your recipe, and perform
operations on it, such as scaling the ingredients by some chosen factor.

## Implementation
This app is basically just a thin UI wrapper over the `sci_tools` library. The
whole app (`sci_tools` and the UI) is compiled by idris into a single
javascript executable (and wrapped into an HTML file for convenience).

## Building
First you have to build and install `sci_tools`. (See `sci_tools/README.md` for
instructions.)

Then this app can simply be build with `idris2 --build`.

## Running
Simply open `build/exec/index.html` in a web browser.
