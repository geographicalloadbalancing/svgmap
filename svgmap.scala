#!/bin/sh
# Need to expand stack size due to deep recursion in the XML library
exec env JAVA_OPTS='-Xss4M' scala -classpath 'anti-xml_2.9.1-0.3.jar' "$0" "$@"
!#
import com.codecommit.antixml._

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
def U(antixml : Node) : scala.xml.NodeSeq = scala.xml.XML.loadString(antixml.toString)
def U(antixml : Group[Node]) : scala.xml.NodeSeq = scala.xml.XML.loadString(antixml.toString)

//@@@@SVG = ElementMaker(namespace="http://www.w3.org/2000/svg")


/** The original blank map */
val BACKGROUND_MAP : Elem = {
	XML fromInputStream (ClassLoader getSystemResourceAsStream "Blank_US_Map.svg")
}

// @@@@ Placeholder
case object Blahblahblah

/** Represents the current state of a particular data center. */
case class DataCenterState(foo : Blahblahblah.type, bar : Blahblahblah.type)

/** Draws an animated data center indicator at the specified coordinates, displaying the given stats over time. Returns an SVG fragment to be inserted into the SVG document. */
def draw_datacenter(x : Double, y : Double, stats : Blahblahblah.type) : Group[Node] = {
	// The various parts of the data center indicator are specified relative to (0, 0); they are then translated to the appropriate spot.
	
	// Dot indicating the exact location of the data center
	val dot = <circle cx="0" cy="0" r="2" style="fill:rgb(0,0,0)" />.convert
	
	// Example animation based on http://www.w3.org/TR/2011/REC-SVG11-20110816/animate.html#AnimationElementsExample
	val label = <text id="TextElement" x="15" y="0">
		I'm a data center!
	</text>.convert
	
	// Draw the sector chart, indicating some data center statistics.
	val sector_g = <g>{
		val COLORS = List("rgb(255,128,128)", "rgb(128,255,128)", "rgb(128,128,255)")
		val NUM_SECTORS = COLORS.length
		0 until NUM_SECTORS map { s ⇒ {
			// @@@@ Dummy data
			/** Radius of a full sector (for value = 1.0) */
			val r = 30
			
			val (start_angle, end_angle) = (s, s + 1) map (_ * 2 * math.Pi / NUM_SECTORS)
			val (start_x, end_x) = (start_angle, end_angle ) map (+r * math.sin(_))
			val (start_y, end_y) = (start_angle, end_angle ) map (-r * math.cos(_))
			
			val path : String = (
				/* center */ "M 0,0" +
				/* draw line */ " L " + start_x + "," + start_y +
				/* draw arc */ " A " + r + "," + r + " 0 0 1 " + end_x + "," + end_y +
				/* close with line */ " Z"
			)
			// @@@@ Animate. Strategy: use <animateTransform> to scale the sector according to the value.
			<path d={path} style={"fill: " + COLORS(s) + "; stroke: black; stroke-width: 1px"}>
				<animateTransform
					attributeName="transform" attributeType="XML"
					type="scale" calcMode="linear"
					values="0.2; 0.6; 0.3; 1; 0.5; 0.4; 0; 0.7; 0.4"
					dur="6s" fill="freeze"
				/>
			</path>
		}}
	}</g>.convert
	
	// Translate everything to the desired data center location
	<g transform={"translate(" + x + "," + y + ")"}>
		{U(dot)}{U(label)}{U(sector_g)}
	</g>.convert
}

def generate_visualization(indata : Blahblahblah.type) : Group[Node] = {
	
	//@@@@ To do: set " rdf:resource="http://purl.org/dc/dcmitype/StillImage" />" to animation
	
	// Use an overlay that transforms real-world coordinates (lat-long) into the SVG input's space.
	// Append the overlay to the SVG document.
	//@@@@@ Currently just a dummy transform. To do: Replace with the real transform
	val overlay = <g transform="translate(100,100)">
		{U(draw_datacenter(0, 0, Blahblahblah))}
		{U(draw_datacenter(100, 200, Blahblahblah))}
		{U(draw_datacenter(300, 50, Blahblahblah))}
	</g>.convert
	
	// Append to the svg document as child
	val doc = BACKGROUND_MAP.copy(children = BACKGROUND_MAP.children :+ overlay)
	
	
	// Output
	//@@@
	println(doc)
	
	null
}

//@@@test
generate_visualization(Blahblahblah)
