/************************************
 * 
 * FILE: 		gulpfile.js
 * AUTHOR: 		Jake Breindel 
 * DATE: 		10-30-15
 * 
 * DESCRIPTION:
 * 
 * Gulpfile for compiling and
 * deploying dependencies.
 * 
 ************************************/

'use strict';

// dependencies
var gulp = require('gulp');
var sass = require('gulp-sass');
var concat = require('gulp-concat');
var bower = require('gulp-bower');
var streamqueue = require('streamqueue');
var rename = require('gulp-rename');

// directories
var SASS_DIR = './priv/static/sass/';
var JS_DIR = './priv/static/js/';
var CSS_DIR = './priv/static/css/';
var FONTS_DIR = './priv/static/fonts/';
var BOWER_DIR = './priv/static/bower_components/';

// lib dirs
var FOUND_SASS_DIR = BOWER_DIR + 'foundation/scss/';
var FONT_AWE_SASS_DIR = BOWER_DIR + 'font-awesome/scss/';

// bower install
gulp.task('bower', function() {
	
	// run bower
	return bower();
});

// css files
gulp.task('css', ['bower'], function() {
	
	// concatenate the streams
    return gulp.src(SASS_DIR + '*.scss')
	    .pipe(sass({
	     	includePaths: [FOUND_SASS_DIR, FONT_AWE_SASS_DIR] 
	     }))
        .pipe(concat('app.css'))
        .pipe(gulp.dest(CSS_DIR));
});

// necessary library files
gulp.task('jsLibs', ['bower'], function() {
	
	// copy modernizer and libs
	return gulp.src(BOWER_DIR + 'foundation/js/vendor/modernizr.js')
		.pipe(gulp.dest(JS_DIR));
});

// js files
gulp.task('js', ['bower'], function() {

	// javascript libs
	return streamqueue({ objectMode: true },
            gulp.src(BOWER_DIR + 'jquery/dist/jquery.js'),
            gulp.src(BOWER_DIR + 'fastclick/lib/fastclick.js'),
            gulp.src(BOWER_DIR + 'foundation/js/foundation.js')
        )
        .pipe(concat('app.js'))
        .pipe(gulp.dest(JS_DIR));
});

// font files
gulp.task('font', ['bower'], function() {
	
	// copy font files
	return gulp.src(BOWER_DIR + 'font-awesome/fonts/*')
		.pipe(gulp.dest(FONTS_DIR));
});

// default gulp task
gulp.task('default', ['css', 'jsLibs', 'js', 'font']);
