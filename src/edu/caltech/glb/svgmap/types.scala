/*
Defines several data types representing data centers and their state.
*/

package edu.caltech.glb.svgmap
class svgmap_types {

// This is cool
/** Enables mapping over 2-tuples. */
// http://stackoverflow.com/questions/11198074/build-xml-literal-containing-anti-xml-object/11198223#11198223
implicit def t2mapper[X, X0 <: X, X1 <: X](t: (X0, X1)) = new {
	def map[R](f: X => R) = (f(t._1), f(t._2))
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

/** Represents the current state of a particular data center. Each statistic should be in [0, 1].
@param sector_stats a list of the statistics to be drawn in the sector chart
*/
case class DataCenterState(sector_stats : Seq[Double])


}