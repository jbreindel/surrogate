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