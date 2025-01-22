"
I have some samples of exporting an image.

These are all class-size scripts.
"
Class {
	#name : 'ASImageExporterTrial',
	#superclass : 'ASImageExporter',
	#category : 'Zag-Exporter-Tests-Tests',
	#package : 'Zag-Exporter-Tests',
	#tag : 'Tests'
}

{ #category : 'writing' }
ASImageExporterTrial class >> inspect42 [

	<script>
	self imageFolder.
	self new writeImageSending: #inspect to: 42
]
