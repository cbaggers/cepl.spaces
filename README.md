# CEPL.Spaces

It provides a way to define a graph of vector-spaces and to query the matrix4 that describes the transform between any two spaces.

In addition it provides gpu-functions & macros to all the use of the spaces in shaders. Performing a compile time analysis to ensure vectors from conflicting spaces are not added/multipled/etc together.
