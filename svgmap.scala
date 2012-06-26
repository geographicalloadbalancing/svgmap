#!/bin/sh
# Need to expand stack size due to deep recursion in the XML library
exec env JAVA_OPTS='-Xss16M' scala -classpath 'anti-xml_2.9.1-0.3.jar' "$0" "$@"
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

//@@@@SVG = ElementMaker(namespace="http://www.w3.org/2000/svg")


/** The original blank map */
val BACKGROUND_MAP : Elem = {
	XML fromInputStream (ClassLoader getSystemResourceAsStream "base_map.svg")
}

/** The total time the animation should run, if there are <var>n</var> time steps to display. */
private def animation_duration(n : Int) : String = (0.5 * n) + "s"

/** Method to use for animation. (For example, "linear" ⇒ smoothly interpolate between data points; "discrete" ⇒ jump) */
val CALC_MODE = "linear"

// @@@@ Placeholder
private case object Blahblahblah

/** Represents the current state of a particular data center. Each statistic should be in [0, 1]. */
case class DataCenterState(stat0 : Double, stat1 : Double, stat2 : Double)

/** A real-world point, specified by a latitude and longitude in degrees. */
case class WorldPt(lat : Double, long : Double) {
	def toDevicePt = {
		//@@@@@ Currently just a dummy transform. To do: Replace with the real transform
		val x = long + 100
		val y = 100 - lat
		DevicePt(x, y)
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
	
	//@@@@ To do: set " rdf:resource="http://purl.org/dc/dcmitype/StillImage" />" to animation
	
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
		DataCenter(WorldPt(0, 0), randomStats),
		DataCenter(WorldPt(-200, 100), randomStats),
		DataCenter(WorldPt(-50, 300), randomStats)
	)
	System.out write generate_visualization(dcs)
}

test