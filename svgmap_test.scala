#!/bin/sh
# Need to expand stack size due to deep recursion in the XML library
exec env JAVA_OPTS='-Xss4M' scala -classpath 'anti-xml_2.9.1-0.3.jar' "$0" "$@"
!#
import com.codecommit.antixml._

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
	println("dot{"+dot+"}")
	
	// Example animation based on http://www.w3.org/TR/2011/REC-SVG11-20110816/animate.html#AnimationElementsExample
	val label = <text id="TextElement" x="5" y="0" visibility="hidden">
		I'm a data center!
		<set attributeName="visibility" attributeType="CSS" to="visible" begin="1s" dur="6s" fill="freeze" />
		<animate
			calcMode="linear"
			values="rgb(0,0,255); rgb(255,0,0); rgb(0,0,255); rgb(255,0,0); rgb(0,0,255); rgb(255,0,0)"
			attributeName="fill" attributeType="CSS"
			begin="1s" dur="6s" fill="freeze"
		/>
	</text>.convert
	println("label{"+label+"}")
	
	// This would be a bar indicating some data center statistic
	val some_bar = <rect style="fill:rgb(0,0,255)" x="-50" y="3" height="10" width="100">
		<animate
			calcMode="linear"
			values="75;45;100;10;50;30;90;20"
			attributeName="width"
			begin="0s" dur="6s" fill="freeze"
		/>
	</rect>.convert
	val another_bar = <rect style="fill:rgb(0,0,255)" x="-50" y="13" height="10" width="100">
		<animate
			calcMode="linear"
			values="15;80;100;60;80;0;20;90;50;75"
			attributeName="width"
			begin="0s" dur="6s" fill="freeze"
		/>
	</rect>.convert
	println("some_bar{"+some_bar+"}")
	
	// Transform everything
	<g transform={"translate(" + x + "," + y + ")"}>
		{dot}{label}{some_bar}{another_bar}
	</g>.convert
}

def generate_visualization(indata : Blahblahblah.type) : Group[Node] = {
	
	//@@@@ To do: set " rdf:resource="http://purl.org/dc/dcmitype/StillImage" />" to animation
	
	// Use an overlay that transforms real-world coordinates (lat-long) into the SVG input's space.
	// Append the overlay to the SVG document.
	//@@@@@ Currently just a dummy transform. To do: Replace with the real transform
	val overlay = <g transform="translate(100,100)">
		{draw_datacenter(0, 0, Blahblahblah)}
		{draw_datacenter(100, 200, Blahblahblah)}
		{draw_datacenter(300, 50, Blahblahblah)}
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
