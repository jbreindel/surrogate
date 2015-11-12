/************************************
 * 
 * FILE: 		ui.js
 * AUTHOR: 		Jake Breindel 
 * DATE: 		10-30-15
 * 
 * DESCRIPTION:
 * 
 * Contains ui functions
 * 
 ************************************/
 
 $(function() {
 
 	'use strict';
 	
 	// open a websocket
 	var managerSocket = 
 		new WebSocket('ws://' + location.host + '/websocket/manager');
 	
 	// called when socket opens
 	managerSocket.onopen = function(e) {
 		
 		console.log('onOpen');
 	};
 	
 	// called when an error occurs with the socket
 	managerSocket.onerror = function(e) {
 		
 		console.log('onerror');
 	};
 	
 	// called when the socket closes
 	managerSocket.onclose = function(e) {
 		
 		console.log('onclose');
 	};
 	
 	// called when we receive a message
 	managerSocket.onmessage = function(e) {
 		
 		console.log('onmessage');
 	};
 	
 	// adds new downloads
 	$('#add-downloads').click(function(e) {
 	
 		// get the urls
 		var urls = $('#download-url-area').val().split('\n');
 		
 		// TODO check urls
 		
 		// send the urls to the manager
 		managerSocket.send(JSON.stringify({
 			downloads: urls
 		}));
 		
 		// set the downloads back to nothing
 		$('#download-url-area').val('');
 	});
 
 });