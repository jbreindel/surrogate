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
 	var activeDownloads = [];
 	
 	// websocket handle
 	var managerWebSocket = null;
 	
 	// called when websocket opens
 	function onWebSocketOpen(e) {

 		// TODO populate connected alert
 	}
 	
 	// called when websocket errors
 	function onWebSocketError(e) {

 		// TODO populate error alert
 	}
 	
 	// called when the websocket closes
 	function onWebSocketClose(e) {

 		// TODO populate closed alert
 	}
 	
 	// finds the download's row
 	function findDownloadRow(download) {
 		
 		// IF the download already found the table row
 		if (download.hasOwnProperty('$tr')) {
 			
 			// exit
 			return;
 		}
 		
 		// select the tr
 		download.$tr = $('#' + download.id + '-tr');
 	}
 	
 	// called when download progress changes
 	function onDownloadProgressChange(download, progress) {
 		
 		// find the download row
 		findDownloadRow(download);
 		
 		// find the percentage
 		var progressPercent = (download.progress / download.length) * 100;
 		var roundedPercent = Math.round(progressPercent * 100) / 100;
 		
 		// ref the child table cell for progress
 		var $downloadPercentageTd = 
 			download.$tr.children('.download-percentage');
 		
 		// find the meter
 		var $meter = $downloadPercentageTd.find('.meter');
 		// find the percent
 		var $percent = $downloadPercentageTd.find('.percent');
 		
 		// set the meter's width
 		$meter.css('width', roundedPercent + '%');
 		// set percent value
 		$percent.text(roundedPercent + '%');
 	}
 	
 	// called when download speed changes
 	function onDownloadSpeedChange(download, speed) {

 		// set the download's speed
 		download.speed = speed;
 		
 		// find the download row
 		findDownloadRow(download);
 		
 		// TODO more logic here to switch to smaller intervals when necessary
 		
 		// ref the speed in MB
 		var speedMB = download.speed / 1000000;
 		var roundedSpeedMB = Math.round(speedMB * 100) / 100;
 		
 		// ref the child cell for speed
 		var $downloadSpeedTd = 
 			download.$tr.children('.download-speed');
 		
 		// set the text
 		$downloadSpeedTd.text(roundedSpeedMB + ' MB/s');
 	}
 	
 	// called to update the active download
 	function updateActiveDownload(refreshedDownload) {
 		
		// find the active download
		var activeDownload = 
			_.findWhere(activeDownloads, {id: refreshedDownload.id});
		
		// IF the download cannot be found
		if (typeof (activeDownload) !== 'undefined') {
			
			// set the active download props
			activeDownload.progress = refreshedDownload.progress;
			activeDownload.speed = refreshedDownload.speed;
			
			// exit
			return;
		}

		// add the download
		activeDownloads.push(refreshedDownload);
		
		// watch the download progress
		watch(refreshedDownload, 'progress', 
				function(prop, action, newvalue, oldvalue) {
			
			// prevent further changes
			WatchJS.noMore = true;
			
			// call progress handler
			onDownloadProgressChange(refreshedDownload, newvalue);
		});
		
		// observe download speed
		watch(refreshedDownload, 'speed', 
				function(prop, action, newvalue, oldvalue) {
			
			// prevent further changes
			WatchJS.noMore = true;
			
			// call speed handler
			onDownloadSpeedChange(refreshedDownload, newvalue);
		});
 	}
 	
    // handles download messages
 	function onDownloadsMessage(refreshedDownloads) {
 		
 		// FOR all of the downloads
 		for (var i = 0; i < refreshedDownloads.length; i++) {
 			
 			// ref the refreshed download
 			var refreshedDownload = refreshedDownloads[i];
 			
 			// update the active download
 			updateActiveDownload(refreshedDownload);
 		}
 	}
 	
 	// reloads all tables
 	function reloadTables() {
 		
 		// call reload all tables
 		reloadTables(['pending', 'completed', 'failed']);
 	}
 	
 	// called to reload all the tables
 	function reloadTables(tables) {
 	 	
 		// IF the tables contains pending
 		if (_.contains(tables, 'pending')) {
 	 		
 	 		// set the active downloads
 	 		activeDownloads = [];
 	 		
 	 	 	// load the pending table
 	 	 	$('#pending-table-container').load('/download/downloads/?status=pending');	
 	 	 	
 	 	 	// send a message to the websocket to refresh
 	 		managerSocket.send(JSON.stringify({
 	 			refresh: 'refresh'
 	 		}));
 		}
 		
 		// IF the tables contain completed
 		if (_.contains(tables, 'completed')) {
 			
 	 	 	// load the completed table
 	 	 	$('#completed-table-container').load('/download/downloads/?status=completed');
 		}
 		
 		// IF the tables contain failed
 		if (_.contains(tables, 'failed')) {

 	 	 	// load the failed table
 	 	 	$('#failed-table-container').load('/download/downloads/?status=failed');
 		}
 	}
 	
 	// handles call when downloads are saved
 	function onDownloadsSavedMessage() {
 		
 		// TODO populate saved alert
 		
 		// refresh the tables
 		reloadTables();
 	}
 	
    // handles download complete messages
 	function onDownloadCompleteMessage(download) {

 		// refresh the tables
 		reloadTables(['pending', 'completed']);
 	}
 	
    // handles failed download messages
 	function onDownloadFailedMessage(download) {

 		// refresh the tables
 		reloadTables(['pending', 'failed']); 		
 	}
 	
 	function onDownloadAcquiredMessage(download) {
 		
 		// refresh tables
 		reloadTables(['pending']);
 	}
 	
    // called when we get a download progress message
 	function onDownloadProgressMessage(download) {
 		
 		// update the active download
 		updateActiveDownload(download);
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
            
            // exit
            break;
            
        // download progress
 		case 'download_progress':
 			
 			// call the download progress handler
 			updateActiveDownload(data.download);
 			
 			// exit
 			break;
            
        // downloads saved successfully
 		case 'downloads_save':
 			
 			// call download save handler
 			onDownloadsSavedMessage(data.downloads);

            // exit
            break;
            
 		// download has been acquired
 		case 'download_acquired':
 			
 			// call download acquired handler
 			onDownloadAcquiredMessage(data.download);

            // exit
            break;
            
        // download has complete
 		case 'download_complete':
 			
 			// call download completed handler
 			onDownloadCompleteMessage(data.download);
 			
 			// exit
 			break;
 			
 		// download has failed
 		case 'download_failed':
 		case 'download_error':
 			
 			// call download failed handler
 			onDownloadFailedMessage(data.download);
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
 		managerWebSocket.send(JSON.stringify({
 			downloads: urls
 		}));
 		
 		// set the downloads back to nothing
 		$('#download-url-area').val('');
 	});
 	
 });