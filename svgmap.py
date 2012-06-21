#!/usr/bin/env python3

from lxml import etree
from lxml.builder import ElementMaker
SVG = ElementMaker(namespace="http://www.w3.org/2000/svg")

from os import path
from collections import namedtuple
from sys import stdout as stdout ; stdout = stdout.buffer

BACKGROUND_MAP_FILE = path.join(path.dirname(__file__), 'Blank_US_Map.svg')

# Represents the current state of a particular data center
DataCenterState = namedtuple('DataCenterState', ('foo', 'bar',))

def draw_datacenter(x, y, stats):
	'''Draws an animated data center indicator at the specified coordinates, displaying the given stats over time. Returns an SVG fragment to be inserted into the SVG document.'''
	
	# Make sure x and y are valid numbers
	float(x)
	float(y)
	
	# The various parts of the data center indicator are specified relative to (0, 0); they are then translated to the appropriate spot.
	
	label = SVG.text("I'm a data center!",
		# Example animation based on http://www.w3.org/TR/2011/REC-SVG11-20110816/animate.html#AnimationElementsExample
		SVG.set(attributeName="visibility", attributeType="CSS", to="visible", begin="3s", dur="6s", fill="freeze"),
		SVG.animate(
			calcMode='linear',
			values='rgb(0,0,255); rgb(255,0,0); rgb(0,0,255); rgb(255,0,0); rgb(0,0,255); rgb(255,0,0)',
			attributeName="fill", attributeType="CSS",
			begin="3s", dur="6s", fill="freeze"
		),
		id='TextElement',
		x='0', y='0',
		visibility='hidden',
	)
	# This would be a bar indicating some data center statistic
	some_bar = SVG.rect(
		SVG.animate(
			calcMode='linear',
			values='75;45;100;10;50;30;90;20',
			attributeName="width",
			begin="3s", dur="6s", fill="freeze"
		),
		x='0', y='10', height='10', width='100',
	)
	another_bar = SVG.rect(
		SVG.animate(
			calcMode='linear',
			values='15;80;100;60;80;0;20;90;50;75',
			attributeName="width",
			begin="3s", dur="6s", fill="freeze"
		),
		x='0', y='20', height='10', width='100',
	)
	
	# Transform everything
	return SVG.g(
		label,
		some_bar,
		another_bar,
		transform="translate({x},{y})".format(x=x, y=y)
	)

def generate_visualization(indata):
	# Load the blank map
	bg = etree.parse(BACKGROUND_MAP_FILE, etree.XMLParser(remove_blank_text=True))
	root = bg.getroot()
	
	#@@@@ To do: set " rdf:resource="http://purl.org/dc/dcmitype/StillImage" />" to animation
	
	
	# Use an overlay that transforms real-world coordinates (lat-long) into the SVG input's space.
	# Append the overlay to the SVG document.
	#@@@@@ Currently just a dummy transform. To do: Replace with the real transform
	overlay = SVG.g(transform="translate(100,100)")
	root.append(overlay)
	
	# Example animated thingy @@@@@@@
	overlay.append(draw_datacenter(0, 0, 'meow'))
	overlay.append(draw_datacenter(100, 200, 'meow'))
	overlay.append(draw_datacenter(300, 50, 'meow'))
	
	# @@@ add xmlns xmlns:xlink="http://www.w3.org/1999/xlink"
	#from IPython import embed; embed()
	
	# Output
	#@@@
	stdout.write(etree.tostring(
		bg,
		encoding='utf8',
		pretty_print=True,
		xml_declaration=True,
	))


#@@@test
generate_visualization(None)