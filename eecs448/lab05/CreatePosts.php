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

        if (!isset($user) || empty($user)) {
          echo 'Username cannot be empty<br />';
          exit();
        }

        if (!isset($content) || empty($content)) {
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
