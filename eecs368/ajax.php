<?php

//=========== SET TIMEZONE =======================
date_default_timezone_set('America/Denver');

//=========== PHP ERROR REPORTING ================
error_reporting(E_ALL);
ini_set("display_errors", 1);

// Declare the array that holds all our possible values
$arrayToMatch = array("The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog");

if(!isset($_POST)){ // Make sure that something was sent from the javascript
	return; // If nothing was sent then stop everything
}

// Get the data that JavaScript posted to us in index.php at Line (A)
$inputValue = $_POST['inp'];

$termToAdd = $_POST['add'];
array_push($arrayToMatch, $termToAdd);

// Create a new string to hold our results
$outputString = "";

// Loop through the array and add each element that contains the input
for($i = 0; $i < sizeof($arrayToMatch); $i++){
	if(strlen(trim($inputValue)) == 0){
		// Since the input value is blank we just return all the elements
		$outputString = $outputString . $arrayToMatch[$i] . "<br />";
	}else{
		$contains = strpos($arrayToMatch[$i],$inputValue);
		if($contains !== false){
			// The string contains our input value
			$outputString = $outputString . $arrayToMatch[$i] . "<br />";// Append to our output string. (In php '.' means concatenate)
		}
	}
}

// Send the string back to Javascript where it will apear in the "datafromserver" variable in Line (B)
echo $outputString;

?>
