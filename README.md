## Halyard Multimedia Engine

Halyard is a hybrid 2D/3D multimedia engine that was used to build
interactive training simulations.  It started life as a 16-bit Windows
application the late 1980s, and eventually wound up supporting rich
scripting in Scheme, a custom IDE built around Scintilla, and a 3D engine
based on Quake 2.

!(editor-small.png) !(overlays-small.png) !(quake2-small.png)

Unfortunately, we only have one [small, low-framerate YouTube
video][youtube] of the 3D engine in action, though as of Spring 2014, we're
looking to capture a much higher-quality video:

[youtube]: https://www.youtube.com/watch?v=1DR6WrGEqVs

Some important directories include:

```
Common: backend code
wx: wxWidgets-based UI
runtime/collects/halyard: Scheme runtime
```
