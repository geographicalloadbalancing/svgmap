/*
Defines several data types representing data centers and their state.
*/

package edu.caltech.glb.svgmap
class svgmap_types {

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

private val IMAGE_WIDTH = 1181
private val IMAGE_HEIGHT = 731

/** A real-world point, specified by a latitude and longitude in degrees. */
case class WorldPt(lat : Double, long : Double) {
	def toDevicePt = {
		// This transform is given on the Wikipedia page http://en.wikipedia.org/wiki/Template:Location_map_USA2
		val xscaled = 50.0 + 124.03149777329222 * ((1.9694462586094064-(lat * math.Pi / 180)) * math.sin(0.6010514667026994 * (long + 96) * math.Pi / 180))
		val yscaled = 50.0 + 1.6155950752393982 * 124.03149777329222 * (0.02613325650382181 - (1.3236744353715044 - (1.9694462586094064 - (lat * math.Pi / 180)) * math.cos(0.6010514667026994 * (long + 96) * math.Pi / 180)))
		// According to the docs, this maps onto a 100×100 square, so we must scale to the actual image size
		DevicePt(xscaled * IMAGE_WIDTH / 100, yscaled * IMAGE_HEIGHT / 100)
	}
}

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

/**
Describes a connection between two locations. */
case class Line(p1 : WorldPt, p2 : WorldPt, stats : Seq[LineState])

/**
Describe a line's states, including the various statistics for it over time.
*/
case class LineState(opacity : Double, width : Double)
}