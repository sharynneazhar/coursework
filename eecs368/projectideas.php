<!doctype html>

<?php
    function Get_sazharProjectIdea($index) {
        if ($index == 0) {
            return "Campus Ride";
        } else if ($index == 1) {
            return "2048 Replica";
        } else if ($index == 2) {
            return "Slack";
        } else {
            return "K.I.S.S";
        }
    }
?>

<html>
<head>
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Project Ideas | Sharynne Azhar</title>

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
                    <li  class="active"><a href="projectideas.php">Project Ideas</a></li>
                    <li><a href="readwrite.php">Visitors</a></li>
                </ul>
            </div>
        </nav>

        <div class="team-area wrapper">
            <h2>Project Ideas</h2>
            <p>
                Below are some ideas for the final class project. Click on each idea to find out more!
            </p>

            <div class="list-group">
                <a href="#" class="list-group-item" data-toggle="campusRide" data-trigger="focus" title="Campus Ride"><?php echo Get_sazharProjectIdea(0); ?></a>
                <a href="#" class="list-group-item" data-toggle="2048" data-trigger="focus" title="2048 Replica"><?php echo Get_sazharProjectIdea(1); ?></a>
                <a href="#" class="list-group-item" data-toggle="slack" data-trigger="focus" title="Slack"><?php echo Get_sazharProjectIdea(2); ?></a>
                <a href="#" class="list-group-item" data-toggle="kiss" data-trigger="focus" title="Keep It Simple, Stupid"><?php echo Get_sazharProjectIdea(3); ?></a>
            </div>
        </div>

        <div class="footer">Sharynne Azhar &copy; 2016 | All Rights Reserved</div>

    </div>
</body>
</html>
