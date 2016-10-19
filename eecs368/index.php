<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">

<head>
<meta content="text/html; charset=utf-8" http-equiv="Content-Type" />
<title>Homework 11</title>

<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
<script>
	function CommunicateWithServer(){
		var ajaxFilename = "ajax.php"; // This is the name of the file that is going to get the variables we send
		var data = {
			inp: $('#searchterm').val(),
			add: $('#addTerm').val()
		}; // Get the value of the search term box and put it in our data object. Call it 'inp'

		var jqxhr = $.post(ajaxFilename, data, function(datafromserver) {	// Send the data to the file specified (A)
			// At this point the server has finished working with the data we sent and has sent us a response (B)
			// the 'datafromserver' variable contains the data sent from the server.

			// Lets dump everything the server sent us into the 'matches' div
			$('#matches').html(datafromserver);
		});
	}

	$( document ).ready(function() {
		// Function called when the value of the "searchterm" box is changed
		$('#searchterm').keyup(function(){
			CommunicateWithServer();
		});

		$('button').click(function() {
			CommunicateWithServer();
			$('#added').html('<br/>added new term to end of array');
		});
	});
</script>


</head>

<body>
	<form>
		<label for="searchterm">Search Term</label>
		<input type="text" id="searchterm" name="searchterm" />
		<br />

		<label for="searchterm">Add Term</label>
		<input type="text" id="addTerm" name="addTerm" />
		<button type="button" name="button">Add</button>
		<div id="added"></div>
		<br />

		<p>Possible Matches:</p>
		<div id="matches"></div>
	</form>

</body>

</html>
