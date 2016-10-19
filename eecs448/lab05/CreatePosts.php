<!DOCTYPE html>
<html>
  <?php $title = 'EECS 448 | Lab 05'; include('../_templates/head.php'); ?>
  <body>
    <nav class="navbar navbar-default">
      <div class="container">
        <a class="navbar-brand" href="#">Lab 5 Exercises</a>
        <ul class="nav pull-right">
          <li><a href="./CreatePosts.html">Back</a></li>
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
        $content = $mysqli->real_escape_string($_POST["content"]);

        if (!isset($user) || !strlen(trim($user))) {
          echo 'Username cannot be empty<br />';
          exit();
        }

        if (!isset($content) || !strlen(trim($content))) {
          echo 'Post contents cannot be empty<br />';
          exit();
        }

        $query = "SELECT * FROM Users WHERE user_id = '" . $user. "'";
        $userExists = $mysqli->query($query)->fetch_assoc();
        if (!$userExists) {
          echo "User does not exist. Please create a new user.";
          exit();
        }

        $query = "INSERT INTO Posts (content, author_id) VALUES ('" . $content . "', '" . $user . "')";
        if ($result = $mysqli->query($query)) {
          echo "Post successfully added!";
        } else {
          echo "Oops, something went wrong.";
        }

        /* close connection */
        $mysqli->close();
      ?>
    </div>

  </body>
</html>
