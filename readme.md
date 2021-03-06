﻿# Nanobrot Fractal Renderer

Nanobrot is a program for high-quality rendering of Mandelbrot fractals with a huge zoom value.

[![Download Latest](https://img.shields.io/badge/download-latest-green.svg)](https://github.com/flutomax/nanobrot/releases/)
[![Donate](https://img.shields.io/badge/donate-paypal-blue.svg)](https://paypal.me/flutomax)

This is a fork of the original code by Karl Runmo and Kalles Fraktaler by Claude Heiland-Allen.
The original source is available on Claude's [website](https://mathr.co.uk/kf/kf.html). 
I spent a considerable amount of time preparing this for the release of Github.

![ScreenShot](/screenshots/v1.0.png)

## Features

* High quality fractal image rendering using supersampling (up to 16x).
* Export fractal images in PNG, JPEG, TIFF, GIF, BMP and WDP formats.
* Fractal Image Printout. 
* Import and export Kalles Fraktaler files.
* Over 2300 color maps for coloring fractal images.
* Various methods for coloring fractal images, including slope shading.
* Ability to save level maps for later use (using ZLIB or LZMA compression).
* Fractal rendering in a untwisted coordinate system (used for export to the [SoundArt](http://stone-voices.ru/application/soundart?lang=en) program).
* Examples

## Details

In the course of long work, it was decided to stop at nanoMB version 1, 
since version 2 proved to be not the best way.

Initially, there was an attempt to implement the code completely on the Object Pascal, 
but the native C++ code turned out to be faster, so all the fractal calculation 
functions were transferred to the dynamic library - nmblib.
The remaining code modules, including GUI, are implementated at the Object Pascal in the Delphi IDE.

To build the project, you need Delphi XE8 and newer, as well as the component 
libraries [Graphics32](http://www.graphics32.org/) and 
[SpTBXLib](http://www.silverpointdevelopment.com/sptbxlib/index.htm). 

Wishes and suggestions, as well as discovered inaccuracies in the program, please inform the developer.

Download the version compiled for Windows x64 and all the files necessary for the program to work in one archive [here](https://github.com/flutomax/nanobrot/releases/).