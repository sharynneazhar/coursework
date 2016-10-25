<!DOCTYPE html>
<html>
  <?php $title = 'EECS 448 | Lab 05'; include('../_templates/head.php'); ?>
  <body>
    <nav class="navbar navbar-default">
      <div class="container">
        <a class="navbar-brand" href="#">Lab 5 Exercises</a>
        <ul class="nav pull-right">
          <li><a href="./DeletePost.html">Back</a></li>
        </ul>
      </div>
    </nav>

    <div class="container">
      <?php
        // Debugging purposes
        error_reporting(E_ALL);
        ini_set("display_errors", 1);

        $mysqli = new mysqli("mysql.eecs.ku.edu", "sazhar", "VjXzuJuPUBCDXwDp", "sazhar");
        if ($mysqli->connect_errno) {
          printf("Connect failed: %s\n", $mysqli->connect_error);
          exit();
        }

        if (isset($_POST["delete"]) && is_array($_POST['delete'])) {
          foreach ($_POST["delete"] as $post) {
            $query = "DELETE FROM Posts WHERE post_id = '".$post."'";
            if ($mysqli->query($query)) {
              echo "Deleted post #".$post." successfully <br />";
            } else {
              echo "Oops, something went wrong deleting post #".$post."!";
            }
          }
        } else {
          echo "None selected!";
        }

        $mysqli->close();
      ?>
    </div>

  </body>
</html>
