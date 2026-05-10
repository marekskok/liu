---
layout: default
title: "Installation"
nav_order: 2
parent: "Home"
---
### Installation
##### Windows
To installl this packege you need to have Rtools installed on your system.
Download it from [CRAN](https://cran.r-project.org/bin/windows/Rtools/). Then run in R:
```R
install.packages("remotes")
remotes::install_github("marekskok/liu")
library(liu)
```
##### Linux (Ubuntu/Debian):
Ensure you have:
```Bash
sudo apt-get install r-base-dev
```
Then in R:
```R
remotes::install_github("marekskok/liu")
library(liu)
```
##### macOS 
To compile C code on macOS, you need Xcode Commnad Line Tools.
Opend your terminal and run: ```xcode-select --install```
Then in R:
```R
install.packages("remotes")
remotes::install_github("marekskok/liu")
library(liu)
```