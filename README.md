# physiospace_shiny

A Rmarkdown shiny front-end to [JRC-COMBINE/PhysioSpaceMethods](github.com/JRC-COMBINE/PhysioSpaceMethods).

## Installation

You need docker installed, then execute
```
docker build -f Dockerfile -t physiospace:latest .
```

Be warned that this takes quite some time as many dependencies have to be compiled and installed.

## Start Rmarkdown / Shiny server with Physiospace

Just execute
```
docker run -p 3838:3838 -it physiospace:latest
```
and in your web browser go to [localhost:3838](http://localhost:3838).


## Further documentation and input 

This docker image is inspired by [github.com/rocker-org/shiny](https://github.com/rocker-org/shiny).
