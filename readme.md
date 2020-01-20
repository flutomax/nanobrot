# Nanobrot Fractal Renderer

Nanobrot is a program for high-quality rendering of Mandelbrot fractals with a huge zoom value.

[![Download Latest](https://img.shields.io/badge/download-latest-green.svg)](https://github.com/flutomax/nanobrot/releases/)
[![Donate](https://img.shields.io/badge/donate-paypal-blue.svg)](https://paypal.me/flutomax)

This is a fork of the original Kalles Fraktaler by Claude Heiland-Allen.
The original source is available on Claude's website. 
I spent a considerable amount of time preparing this for the release of Github.

In the course of long work, it was decided to stop at nanoMB version 1, 
since version 2 proved to be not the best way.

Initially, there was an attempt to implement the code completely on the 
Pascal Object, but the native C ++ code turned out to be faster, so all 
the fractal calculation functions were transferred to the dynamic library - nmblib.

To build the project, you need Delphi XE8 and newer, as well as the component 
libraries [Graphics32](http://www.graphics32.org/) and 
[SpTBXLib](http://www.silverpointdevelopment.com/sptbxlib/index.htm). 