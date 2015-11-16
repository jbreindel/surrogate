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
 	
 	// download arrays
 	var activeDownloads = null;
 	
 	// websocket handle
 	var managerWebSocket = null;
 	
 	// called when websocket opens
 	function onWebSocketOpen(e) {
 		
 		// TODO
 	}
 	
 	// called when websocket errors
 	function onWebSocketError(e) {
 		
 		// TODO
 	}
 	
 	// called when the websocket closes
 	function onWebSocketClose(e) {
 		
 		// TODO
 	}
 	
    // handles download messages
 	function onDownloadsMessage(downloads) {
 		
 	}
 	
    // handles download complete messages
 	function onDownloadCompleteMessage(download) {
 		
 	}
 	
    // handles failed download messages
 	function onDownloadFailedMessage(download) {
 		
 	}
 	
    // called when we get a download progress message
 	function onDownloadProgressMessage(download) {
 		
 	}
 	
 	// called when the websocket recieves a message
 	function onWebSocketMessage(e) {
 		
// 		// IF there is no data
// 		if (!e.hasOwnProperty('data')) {
// 			
// 			// exit
// 			return;
// 		}
 		
 		// get the json message
 		var data = JSON.parse(e.data);
 		
 		// SWITCH on the message type
 		switch (data.message) {
 		
 		// downnloads message
 		case 'downloads':
 			
            // call download handler
            onDownloadsMessage(data.downloads);
 		}
 	}
 	
 	// called to connect and initiate the websocket
 	function connectWebSocket() {
 	
 		// IF the websocket already exists
 		if (managerWebSocket != null) {
 			
 			// exit
 			return;
 		}
 		
 		// open a websocket
 	 	managerWebSocket = 
 	 		new WebSocket('ws://' + location.host + '/websocket/manager');
 	 	
 	 	// set websocket callbacks
 	 	managerWebSocket.onopen = onWebSocketOpen;
 	 	managerWebSocket.onerror = onWebSocketError;
 	 	managerWebSocket.onclose = onWebSocketClose;
 	 	managerWebSocket.onmessage = onWebSocketMessage;
 	}
 	
 	// load the pending table
 	$('#pending-table-container').load('/download/downloads/?status=pending', connectWebSocket);
 	
 	// load the completed table
 	$('#completed-table-container').load('/download/downloads/?status=completed');
 	
 	// load the failed table
 	$('#failed-table-container').load('/download/downloads/?status=failed');
 	
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