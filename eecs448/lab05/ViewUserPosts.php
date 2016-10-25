<!DOCTYPE html>
<html>
  <?php $title = 'EECS 448 | Lab 05'; include('../_templates/head.php'); ?>
  <body>
    <nav class="navbar navbar-default">
      <div class="container">
        <a class="navbar-brand" href="#">Lab 5 Exercises</a>
        <ul class="nav pull-right">
          <li><a href="./ViewUserPosts.html">Back</a></li>
        </ul>
      </div>
    </nav>

    <div class="container">
      <table class="table table-hover">
        <thead>
          <tr>
            <th><?php echo $_POST['username']?>'s Post</th>
          </tr>
        </thead>
        <tbody>
          <?php
            // Debugging purposes
            error_reporting(E_ALL);
            ini_set("display_errors", 1);

            $mysqli = new mysqli("mysql.eecs.ku.edu", "sazhar", "VjXzuJuPUBCDXwDp", "sazhar");
            if ($mysqli->connect_errno) {
              printf("Connect failed: %s\n", $mysqli->connect_error);
              exit();
            }

            $query = "SELECT * FROM Posts WHERE author_id = '" . $_POST['username'] . "'";
            if ($result = $mysqli->query($query)) {
              if ($result->num_rows === 0) {
                echo "<tr><td>User has no posts.</td></tr>";
              }
              while ($row = $result->fetch_assoc()) {
                echo "<tr><td>".$row["content"]."</td></tr>";
              }
              $result->free();
            } else {
              echo "Oops, something went wrong!";
            }
            
            $mysqli->close();
          ?>
        </tbody>
      </table>
    </div>

  </body>
</html>
