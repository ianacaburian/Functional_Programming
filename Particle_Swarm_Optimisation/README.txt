UNE - COSC250 - Assignment 3
Written by Christian Caburian
=============================

Particle Swarm Optimization
---------------------------
This program is intended to find the location and height
of the highest peak on a region of terrain.

The highest peak is found by employing a swarm of particles
to traverse across the region of terrain.
They iteratively report their altitudes to each other, 
in order to lead the swarm to the point on the terrain 
with the highest peak.

The terrain is implemented as a mathematical function of 
an (x, y) coordinate.
The region is limited to a surface with dimensions
640 x 580.
The particles are represented as white circles, where
the highest point found so far is marked in red on the 
surface.

The altitudes of each particle are displayed in a series
of bar charts and updated in realtime.
=========================================================

Operation
----------
This program consists of the files:
- Main.scala
- Particles.scala

Run the program and follow the prompt to select the
number of particles to use in the swarm.
Performance may decrease with a higher number of
particles.
===================================================

Program Design
--------------
The program has been written in two files:
Particles.scala contains all fields and methods 
related to particles, as well as the definition
of the terrain.
Main.scala handles all UI and timing tasks.

Particles.scala
---------------
Key components:
- height() defines the terrain to search.
- Methods and mutable variables to continually 
  update and track search values
  (i.e. the highest points seen).
- Particle() defines how each particle behaves 
  and stores its own highest points seen.
- SwarmState() defines how the entire swarm
  is updated and progressed.

Main.scala
----------
Key components:
- UI is implemented via JavaFX
- The program is updated via AnimationTimer
- Dialog to enable the user to select how 
  many particles to run.
- Barcharts for particle altitude feedback.
- The search values are updated here and
  logged to standard out as:
	- Best Location
	- Best Altitude
	- Five Highest Particles

For more info, see code comments.
==========================================

Assignment Discussion
----------------------
For reference, the best point found was:
- Best Location:(0.4509937205624561,0.4509937203201346) 
- Best Altitude: 0.9509934038460642

The optimization is defined by the given equations in 
the assignment but it was found that there are 
performance differences in the way the velocity equation 
is implemented that warranted some discussion.

In updateParticle(), a particle's velocity is updated 
by passing the particle's position, velocity and its
highest point seen into updateVelocity().
For a swarm of 100 particles and higher, the best
point was found by passing the parameters
newPosition, velocity and newHighestAlt.
newPosition can be considered the next state's position, 
so it's technically the "future" position.

For swarms of around 10 particles, these
parameters lead to the swarm being stuck at various
positions that were very far from the best point.
This was fixed when passing position (the current
position), rather than newPosition into 
updateVelocity().

However, this implementation hampered the performance
for larger swarms. That is, although they moved much 
faster, they continually overshot the best point.

Therefore, the slower way was implemented (passing
newPosition, rather than position) while also 
restricting the minimum number of particles to
100 to prevent the swarm from getting stuck at
locations very far from the best point.

This choice was justified as it consistently 
achieved the main purpose of the program -- to find 
the best point, where efforts for efficiency are 
considered outside the scope of the instructions.