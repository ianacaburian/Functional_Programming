# Week 10 tutorial

This week, there's not a set of tests.
Let's put in a slightly higher level mission...

First, watch the video on backpressure (on Moodle) and then read the link to reactive-streams.org

Akka provides a reactive streams implementation called... Akka Streams.

This is the guide we'll work through:

http://doc.akka.io/docs/akka/2.4.4/scala/stream/stream-quickstart.html#stream-quickstart-scala

Your mission (in MyApp.scala):

1. To create a source producing random numbers (ok, start with just a source of numbers in order as they do in the tutorial)

2. To create a flow that will perform Fizz Buzz on the Source

3. To "materialse" the flow, printing out the stream to standard out.

4. Now insert something into the flow to limit it to printing five outputs per second

Last modified: Wednesday, 11 May 2016, 2:53 PM
