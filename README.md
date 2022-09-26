# pucks

A clojure project for development and simulation of a laser projector.
Work in progress, but 100% done in Clojure (my favorite language).
One day will be converted to an opensource library for CAD and simulation of any type of physical project.
Basically OpenSCAD + Clojure + Physics Simulation + Realtime Graphics!

Now featuring \*raytracing\*!!!
And OpenGL support!

## Demo

This project features a raytracing engine for predicting the path of the laser beam. Unfortunately, the rendering is done in OpenSCAD still. I plan on switching to OpenGL for native support in the future, but in the meantime, please enjoy the following gifs of the raytracing functionality. I'm pretty proud of it -- it computes the normals of the surface, and the vector of reflection. It uses the clojure (lazy-seq) macro. Check out the code :).

A demo of the laser-projector emitting a beam from a laser diode, reflecting off two galvos, and hitting a backplate

![Render1](render1.gif?raw=true "Render1")

A close-up of the galvo action

![Render2](render2.gif?raw=true "Render2")

Ray tracing demo with an arbitrary number of objects.

![Render3](render3.png?raw=true "Render3")






## Usage


## License

Copyright © 2022 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
