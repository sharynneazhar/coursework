<!DOCTYPE html>
<html>
  <?php $title = 'EECS 448 | Lab 05'; include('../_templates/head.php'); ?>
  <body>
    <nav class="navbar navbar-default">
      <div class="container">
        <a class="navbar-brand" href="#">Lab 5 Exercises</a>
        <ul class="nav pull-right">
          <li><a href="./CreateUser.html">Back</a></li>
        </ul>
      </div>
    </nav>

    <div class="container">
      <?php
        // Debugging purposes
        error_reporting(E_ALL);
        ini_set("display_errors", 1);

        $mysqli = new mysqli("mysql.eecs.ku.edu", "sazhar", "VjXzuJuPUBCDXwDp", "sazhar");

        /* check connection */
        if ($mysqli->connect_errno) {
          printf("Connect failed: %s\n", $mysqli->connect_error);
          exit();
        }

        $user = $mysqli->real_escape_string($_POST["username"]);

        if (!isset($user) || !strlen(trim($user))) {
          echo 'Username cannot be empty<br />';
          exit();
        }

        $query = "INSERT INTO Users (user_id) VALUES ('" . $user . "')";
        if ($result = $mysqli->query($query)) {
          printf("%s successfully created!", $user);
        } else {
          if ($mysqli->errno == 1062) {
            printf("User %s already exists!", $user);
          }
        }

        /* close connection */
        $mysqli->close();
      ?>
    </div>

  </body>
</html>
