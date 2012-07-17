/*
Defines several data types representing data centers and their state.
*/

package edu.caltech.glb.svgmap
/** This trait exists to work around a corner case in Scala's package system. It should not be used directly.

Scala allows you to split a package over multiple files.
It also lets you put values (incl. functions) directly in a package by declaring a package object.
The problem is that objects <em>cannot</em> be split over multiple files.
As a workaround, we declare a package-private trait in this file, and in the other file declare a package object that extends this trait
(and thereby inherits all the variables declared herein).
*/
private[svgmap] trait svgmap_types {

// This is cool
/** Enables mapping over 2-tuples. */
// http://stackoverflow.com/a/4022510/319931
implicit def t2mapper[X, X0 <: X, X1 <: X](t: (X0, X1)) = new {
	def map[R](f: X => R) = (f(t._1), f(t._2))
}
implicit def t3mapper[X, X0 <: X, X1 <: X, X2 <: X](t: (X0, X1, X2)) = new {
	def map[R](f: X => R) = (f(t._1), f(t._2), f(t._3))
}
implicit def t4mapper[X, X0 <: X, X1 <: X, X2 <:X, X3 <:X](t: (X0, X1, X2, X3)) = new {
	def map[R](f: X => R) = (f(t._1), f(t._2), f(t._3), f(t._4))
}
implicit def t5mapper[X, X0 <: X, X1 <: X, X2 <:X, X3 <:X, X4 <:X](t: (X0, X1, X2, X3, X4)) = new {
	def map[R](f: X => R) = (f(t._1), f(t._2), f(t._3), f(t._4), f(t._5))
}
implicit def t3asSeq[X, X0 <: X, X1 <: X, X2 <: X](t: (X0, X1, X2)) = new {
	def asSeq : Seq[X] = List(t._1, t._2, t._3)
}

def parseInt(str : String) : Option[Int] = try {Some(str.toInt)} catch {case _ : NumberFormatException ⇒ None}

/** A real-world point, specified by a latitude and longitude in degrees. */
case class WorldPt(lat : Double, long : Double)

/** A point in device coordinates. Note that y increases as you go <em>down</em>. */
case class DevicePt(x : Double, y : Double)

/**
Describes a data center, and encapsulates various statistics for it over time.
*/
case class DataCenter(coords : WorldPt, stats : Seq[DataCenterState])

/** Represents arbitrary values for each data center statistic. Used to ensure that colors and actual values have the same schema. */  
case class DataCenterVals[T](demand : T, supplies : (T, T, T))

/** Represents the current state of a particular data center. Each statistic should be in [0, 1].
@param demand the data center's current energy demand
@param supplies a list of the amounts of energy from various source types (e.g. solar, wind, grid) that are available now
*/
type DataCenterState = DataCenterVals[Double]
val DataCenterState = DataCenterVals[Double] _
type DataCenterColors = DataCenterVals[String]
val DataCenterColors = DataCenterVals[String] _
type DataCenterLegendText = DataCenterVals[String]
val DataCenterLegendText = DataCenterVals[String] _

/**
Describes a connection between two locations. */
case class Line(p1 : WorldPt, p2 : WorldPt, stats : Seq[LineState])

/**
Describe a line's states, including the various statistics for it over time.
*/
case class LineState(opacity : Double, width : Double)
}