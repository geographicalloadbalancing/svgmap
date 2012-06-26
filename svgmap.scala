#!/bin/sh
# Need to expand stack size due to deep recursion in the XML library
exec env JAVA_OPTS='-Xss32M' scala -classpath 'anti-xml_2.9.1-0.3.jar' "$0" "$@"
!#
import com.codecommit.antixml._
import scala.collection.GenSeq

// This is cool
/** Enables mapping over 2-tuples. */
// http://stackoverflow.com/questions/11198074/build-xml-literal-containing-anti-xml-object/11198223#11198223
implicit def t2mapper[X, X0 <: X, X1 <: X](t: (X0, X1)) = new {
	def map[R](f: X => R) = (f(t._1), f(t._2))
}

/** Convenience function to allow easy use of Anti-XML in XML literals.
<p>Usage: {@code
val antixml = <child attr="val">...</child>.convert
<xmlliteral>{U(antixml)}</xmlliteral>.convert
}</p>**/
private def U(antixml : Node) : scala.xml.NodeSeq = scala.xml.XML.loadString(antixml.toString)
private def U(antixml : Group[Node]) : scala.xml.NodeSeq = scala.xml.XML.loadString(antixml.toString)

/** The original blank map */
val BACKGROUND_MAP : Elem = {
	XML fromInputStream (ClassLoader getSystemResourceAsStream "base_map.svg")
}

/** The total time the animation should run, if there are <var>n</var> time steps to display. */
private def animation_duration(n : Int) : String = (0.5 * n) + "s"

/** Method to use for animation. (For example, "linear" ⇒ smoothly interpolate between data points; "discrete" ⇒ jump) */
val CALC_MODE = "linear"

/** Represents the current state of a particular data center. Each statistic should be in [0, 1]. */
case class DataCenterState(stat0 : Double, stat1 : Double, stat2 : Double)

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

/** Draws an animated data center indicator at the specified coordinates, displaying the given stats over time. Returns an SVG fragment to be inserted into the SVG document. */
def draw_datacenter(dc : DataCenter) : Group[Node] = {
	val DataCenter(coords, stats) = dc
	val DevicePt(x, y) = coords.toDevicePt
	
	// The various parts of the data center indicator are specified relative to (0, 0); they are then translated to the appropriate spot.
	
	// Dot indicating the exact location of the data center
	val dot = <circle cx="0" cy="0" r="2" style="fill:rgb(0,0,0)" />.convert
	
	// Example animation based on http://www.w3.org/TR/2011/REC-SVG11-20110816/animate.html#AnimationElementsExample
	val label = <text x="15" y="0">
		I'm a data center!
	</text>.convert
	
	// Draw the sector chart, indicating some data center statistics.
	val sector_stats = stats map (_ match {
		case DataCenterState(a, b, c) ⇒ List(a, b, c)
	})
	val sector_g = <g>{
		val NUM_SECTORS = sector_stats(0).length
		// Evenly distributed around the color wheel
		val COLORS = 0 until NUM_SECTORS map (360.0 / NUM_SECTORS * _) map ("hsla(" + _ + ", 100%, 70%, 0.7)")
		0 until NUM_SECTORS map { s ⇒ {
			/** Radius of a full sector (for value = 1.0) */
			val r = 30
			
			// Calculate sector's endpoints
			val (start_angle, end_angle) = (s, s + 1) map (_ * 2 * math.Pi / NUM_SECTORS)
			val (start_x, end_x) = (start_angle, end_angle ) map (+r * math.sin(_))
			val (start_y, end_y) = (start_angle, end_angle ) map (-r * math.cos(_))
			
			// Defines outline of sector
			val path : String = (
				/* center */ "M 0,0" +
				/* draw line */ " L " + start_x + "," + start_y +
				/* draw arc */ " A " + r + "," + r + " 0 0 1 " + end_x + "," + end_y +
				/* close with line */ " Z"
			)
			// Animate. We use <animateTransform> to scale the sector according to the corresponding value.
			<path d={path} style={"fill: " + COLORS(s) + "; stroke: black; stroke-width: 1px"}>
				<animateTransform
					attributeName="transform" attributeType="XML"
					type="scale" calcMode={CALC_MODE}
					values={sector_stats map (_(s)) mkString ";"}
					dur={animation_duration(stats.length)} fill="freeze"
				/>
			</path>
		}}
	}</g>.convert
	
	// Translate everything to the desired data center location
	<g transform={"translate(" + x + "," + y + ")"}>
		{U(dot)}{U(label)}{U(sector_g)}
	</g>.convert
}

def generate_visualization(indata : Seq[DataCenter]) : Array[Byte] = {
	val overlay = <g>
		{indata map (dc ⇒ U(draw_datacenter(dc)))}
	</g>.convert
	
	// Append to the svg document as child
	val doc = BACKGROUND_MAP.copy(children = BACKGROUND_MAP.children :+ overlay)
	
	val os = new java.io.ByteArrayOutputStream
	XMLSerializer(outputDeclaration = true).serializeDocument(doc, os)
	os.toByteArray
}

//@@@test
def test = {
	def randomStats = {
		def randDC = DataCenterState(math.random, math.random, math.random)
		(GenSeq fill 20)(randDC).seq
	}
	val dcs = List(
		DataCenter(WorldPt(41, -104), randomStats),
		DataCenter(WorldPt(34, -84), randomStats),
		DataCenter(WorldPt(42, -87), randomStats)
	)
	System.out write generate_visualization(dcs)
}

test