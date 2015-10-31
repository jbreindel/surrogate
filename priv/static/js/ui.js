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
 	var managerSocket = new WebSocket('ws://' + location.host + '/');
 	
 	// called when socket opens
 	managerSocket.onopen = function(e) {
 		
 	};
 	
 	// called when an error occurs with the socket
 	manager.onerror = function(e) {
 		
 	};
 	
 	// called when the socket closes
 	managerSocket.onclose = function(e) {
 		
 	};
 	
 	// called when we receive a message
 	manager.onmessage = function(e) {
 		
 	};
 	
 	// adds new downloads
 	$('#add-downloads').click(function(e) {
 	
 		// get the urls
 		var urls = $('#download-url-area').val();
 		
 		// TODO check urls
 		
 		// post the urls
 		$.post('/download/downloads/', 
 			{ downloads: urls }, function(data, status) {

 			console.log(data);
 		});
 	});
 
 })();