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
 	
 	// returns readable download status
 	function downloadStatus(download) {
 		
 		// SWITCH on download status
 		switch (download.status) {
 		
 		case 0:
 			return 'pending';
 		case 1:
 			return 'acquired'; 			
 		case 2:
 			return 'active'; 			
 		case 3:
 			return 'paused'; 			
 		case 4:
 			return 'completed'; 			
 		case 5:
 			return 'failed'; 			
 		case 6:
 			return 'not found';
 		}
 	}
 	
 	// calculate download progress
 	function calcDownloadProgress(download) {
 		
 		// ref percentage
 		return (download.progress / download.length) * 100;
 	}
 	
 	// calculates download speed
 	function calcDownloadSpeed(download) {
 		
 		// ref download speed prop
 		return download.speed;
 	}
 	
 	// build download table row
 	function buildDownloadTableRow(download) {
 		
 		// calc download percent
 		var percent = calcDownloadProgress(download);
 		var speed = calcDownloadSpeed(download);
 		var status = downloadStatus(download);
 		
 		// ref the built row
 		return '<tr>' +
 					'<td>' + download.id + '</td>' +
 					'<td>' + download.file + '</td>' +
 					'<td>' + 
 						(percent != null ? 
 						'<span class="meter green" style="width: ' + percent + '%">' +
 							'<p class="percent">' + percent + '%</p>' +
 						'</span>' : 
 						' - ') +
 					'</td>' +
 					'<td>' +  + '</td>' +
 					'<td>' +
 						'<span class="' +
 					
 						(function() {
 							
 							// SWITCH on download status
 							switch (download.status) {
 							
 							// pending
 							case 'pending':
 								return 'info';
 								
 							case 'acquired':
 								return 'secondary'
 								
 							case 'active':
 								return 'success'
 								
 							case 'completed':
 								return 'secondary'
 							}
 						
 						})() +
 						
 						'">' +
 							status +
 						'</span>'
 					'</td>' +
 				'</tr>';
 	}
 	
 	// rebuilds a table accordingly
 	function rebuildTable($table, downloadArray) {
 		
 		// find the table body
 		var $tbody = $table.find('tbody');
 		
 		// delete all rows
 		$tbody.empty();
 		
 		// FOR all of the downloads in the array
 		for (var i = 0; i < downloadArray.length; i++) {
 			
 			// ref the download
 			var download = downloadArray[i];
 			
 			// build the row
 			var row = buildDownloadTableRow(download);
 			
 			// add a row to the body
 			$('#pending-table-body').append(row);
 		}
 	}
		
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
 	
 	// watch pending downloads
 	watch(downloads, 'pending', function(prop, action, newvalue, oldvalue) {
 		
 		// don't track changes
 		WatchJS.noMore = true;
 		
 		// IF either of the old or new values are empty
 		if (newvalue.length == 0 || oldvalue.length == 0) {
 			
 	 		// rebuild the table
 	 		rebuildTable($('pending-table'), downloads.pending);
 			
 			// exit
 			return;
 		}
 		
 		// FOR all of the new values
 		for (var i = 0; i < newvalue.length; i++) {
 			
 			// ref the new download
 			var newDownload = newvalue[i];
 			
 			// IF the new value has a speed value
 			if (newDownload.hasOwnProperty('speed') || 
 					newDownload.status != 2) {
 				
 				// next
 				continue;
 			}
 			
 			// find the corresponding old download
 			var oldDownload = _.findWhere(oldvalue, {id: newDownload.id});
 			
 			// IF it doesn't exist
 			if (oldDownload == null || 
 					typeof (oldDownload) == 'undefined' ) {
 				
 				// next
 				continue;
 			}
 			
 			// set the new speed value
 			newDownload.speed = oldDownload.speed;
 		}
 		
 		// rebuild the table
 		rebuildTable($('pending-table'), downloads.pending);
 	});
 	
 	// watch completed downloads
 	watch(downloads, 'completed', function(prop, action, newvalue, oldvalue) {
 		
 		console.log('completed changed');
 	});
 	
 	// watch failed downloads
 	watch(downloads, 'failed', function(prop, action, newvalue, oldvalue) {
 		
 		console.log('failed changed');
 	});
 	
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