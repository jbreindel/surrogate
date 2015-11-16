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
 	var downloads = {
 		pending: [],
 		completed: [],
 		failed: []
 	};
		
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
 	
 	// called when data is loaded via rest
 	function onDownloadDataLoaded(data, downloadType) {
 		
 		// SWITCH on the tab name
 		switch (downloadType) {
 			
 		// pending downloads
 		case 'pending':
 			
 			// set the pending downloads
 			downloads.pending = data;
 			
 			// exit
 			break;
 			
 		// completed downloads
 		case 'completed':
 			
 			// set the pending downloads
 			downloads.completed = data;

 			// exit
 			break;
 			
 		// failed downloads
 		case 'failed':
 			
 			// set the pending downloads
 			downloads.failed = data;
 		}
 	}
 	
 	// called with tab vars
 	function onTabClick($table, statusType) {
 		
 		// $tbody table body
 		var $tBody = $table.children('tbody');
 		
        // IF we have rows
 		if ($tbody.find('tr').index() > 0) {
        
			// exit
			return;
        }
		
		// get the rows
		$.get('/download/downloads/?status=' 
			+ statusType + '&limit=15', function(data) {
			
				// call the download data handler
				onDownloadDataLoaded(data, statusType);
			});
 	}
 	
 	// tab callbacks
 	$('#download-tabs').on('toggled', function (event, tab) {
 	    
 		// get the tab name
 		var tabName = tab.context.text;
 		
 		// tab vars
 		var $table = null;
 		var statusType = null;
 		
 		// SWITCH on the tab name
 		switch (tabName) {
 			
 		// add links
 		case 'Add':
 			
 			// exit
 			return;
 		
 		// pending downloads
 		case 'Pending':
 			
 			// set tab vars
 			$table = $('pending-table');
 			statusType = 'pending';
 			
 			// exit
 			break;
 			
 		// completed downloads
 		case 'Completed':
 			
 			// set tab vars
 			$table = $('completed-table');
 			statusType = 'completed';

 			// exit
 			break;
 			
 		// failed downloads
 		case 'Failed':
 			
 			// set tab vars
 			$table = $('failed-table');
 			statusType = 'failed';
 		}
 		
 		// call the tab click handler
 		onTabClick($table, statusType);
 	});
 	
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
 	
 	// on initial page load
 	$.get('/download/downloads/?status=pending&limit=15', function(data) {
 		
 		// call the page loaded handler
 		onDownloadDataLoaded(data, 'pending');
 	});
 
 });