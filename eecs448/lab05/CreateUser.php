<!DOCTYPE html>
<html>
  <head>
    <title>EECS 448 | Lab 05</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <!-- Bootstrap CSS Plugin -->
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
      integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"
      integrity="sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp" crossorigin="anonymous">

    <link rel="stylesheet" href="css/main.css">

    <!-- jQuery Plugin -->
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js"></script>

    <!-- Bootstrap JS Plugin -->
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
      integrity="sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" crossorigin="anonymous"></script>

  </head>
  <body>

    <?php
      $mysqli = new mysqli("mysql.eecs.ku.edu", "sazhar", "VjXzuJuPUBCDXwDp", "sazhar");

      /* check connection */
      if ($mysqli->connect_errno) {
        printf("Connect failed: %s\n", $mysqli->connect_error);
        exit();
      }

      $query = "SELECT user_id FROM Users";

      if ($result = $mysqli->query($query)) {

        /* fetch associative array */
        while ($row = $result->fetch_assoc()) {
          echo "<br /><br /><br />";
          echo $row["user_id"];

        }

        /* free result set */
        $result->free();
      }

      /* close connection */
      $mysqli->close();
    ?>

  </body>
</html>
