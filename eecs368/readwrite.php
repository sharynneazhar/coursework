<!DOCTYPE html>
<html>
<head>
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Visitors | Sharynne Azhar</title>

    <link rel="stylesheet" href="css/style.css">
    <link href="css/team.css" rel="stylesheet" type="text/css" />

    <!-- Latest compiled and minified CSS -->
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
      integrity="sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7" crossorigin="anonymous">

    <!-- Optional theme -->
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css"
      integrity="sha384-fLW2N01lMqjakBkx3l/M9EahuwpSfeNvV63J5ezn3uZzapT0u7EYsXMjQV+0En5r" crossorigin="anonymous">

    <!-- Latest compiled and minified JavaScript -->
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
      integrity="sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS" crossorigin="anonymous"></script>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.2/jquery.min.js"></script>

    <script src="js/scripts.js" type="text/javascript"></script>
</head>
<body>
    <div class="container">
        <nav class="navbar navbar-default">
            <div class="container-fluid">
                <a class="navbar-brand" href="index.html">Sharynne Azhar</a>
                <ul class="nav nav-pills pull-right">
                    <li><a href="index.html">Home</a></li>
                    <li><a href="team.html">Team</a></li>
                    <li><a href="othergroups.html">Other Groups</a></li>
                    <li><a href="projectideas.php">Project Ideas</a></li>
                    <li  class="active"><a href="readwrite.php">Visitors</a></li>
                </ul>
            </div>
        </nav>

        <div class="team-area wrapper">
            <?php

                $filename = "rw_file.txt";
                date_default_timezone_set("America/Chicago");

                $file = fopen($filename, "r") or die("can't open file");
                $text = fread($file, filesize($filename));

                $ipAddress = "IP Address: " . $_SERVER["REMOTE_ADDR"];
                $date = "Date/Time: " . date("Y/m/d") . " " . date("h:ia");

                echo $text;
                echo "<p>" . $ipAddress . "<br>" . $date . "</p>";

                fclose($file);
                $file = fopen($filename, "a");
                fwrite($file, "<p>". $ipAddress. "<br>" . $date . "</p>");

                fclose($file);

            ?>
        </div>

        <div class="footer">Sharynne Azhar &copy; 2016 | All Rights Reserved</div>

    </div>
</body>
</html>
