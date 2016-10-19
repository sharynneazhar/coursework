<?php

    //=========== SET TIMEZONE =======================
    date_default_timezone_set('America/Chicago');

    //=========== PHP ERROR REPORTING ================
    error_reporting(E_ALL);
    ini_set("display_errors", 1);

    //========== BEGIN POST =========================
    if(!isset($_POST)) { // Make sure that something was sent from the javascript
    	return; // If nothing was sent then stop everything
    }

    $username = $_POST['user'];
    $password = $_POST['password'];

    $addUser = $username . ',' . $password . "\n";

    // write to file
    $filename = "usersList.txt";
    $file = fopen($filename, 'a');
    fwrite($file, $addUser);
    fclose($file);
	
    echo '1 record added for user: ' . $username . "." . " Refresh page to see it on the table";
?>
