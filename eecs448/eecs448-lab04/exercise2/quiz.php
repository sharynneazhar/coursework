<!DOCTYPE html>
<html>
  <head>
    <title>EECS 448 | Lab 04</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css" integrity="sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp" crossorigin="anonymous">
    <link rel="stylesheet" href="../css/main.css">
  </head>
  <body>
    <nav class="navbar navbar-default">
      <div class="container">
        <a class="navbar-brand" href="#">KU Trivia | Results</a>
        <ul class="nav pull-right">
          <li><a href="quiz.html">Back to Quiz</a></li>
        </ul>
      </div>
    </nav>
    <div class="container">
      <?php
        // Debugging purposes
        error_reporting(E_ALL);
        ini_set("display_errors", 1);

        $nCorrect = 0;

        $q1 = $_POST["q1"];
        $q2 = $_POST["q2"];
        $q3 = $_POST["q3"];
        $q4 = $_POST["q4"];
        $q5 = $_POST["q5"];

        if (isset($_POST["q1"]) && $_POST["q1"] == "Lawrence") {
          $nCorrect++;
        }

        if (isset($_POST["q2"]) && $_POST["q2"] == "Jayhawks") {
          $nCorrect++;
        }

        if (isset($_POST["q3"]) && $_POST["q3"] == "Campanile") {
          $nCorrect++;
        }

        if (isset($_POST["q4"]) && $_POST["q4"] == "Clyde Tombaugh") {
          $nCorrect++;
        }

        if (isset($_POST["q5"]) && $_POST["q5"] == "Phog") {
          $nCorrect++;
        }

        $percent = round((($nCorrect / 5) * 100), 2);
        echo "<h4>Total Correct: " . $nCorrect . "/5</h4>";
        echo "<h4 style='margin-bottom: 30px;'>Percentage: " . $percent . "%</h4>";
      ?>

      <div class="well">
        <div>Question 1: Where is the main campus located?</div>
        <div>You answered: <?php echo $q1; ?></div>
        <div>Correct answer: Lawrence</div>
      </div>
      <div class="well">
        <div>Question 2: What is the school mascot?</div>
        <div>You answered: <?php echo $q2; ?></div>
        <div>Correct answer: Jayhawks</div>
      </div>
      <div class="well">
        <div>Question 3: If you walk through this building before your graduation day, you'll never graduate.</div>
        <div>You answered: <?php echo $q3; ?></div>
        <div>Correct answer: Campanile</div>
      </div>
      <div class="well">
        <div>Question 4: This KU student was an amateur astronomer who discovered Pluto in 1930.</div>
        <div>You answered: <?php echo $q4; ?></div>
        <div>Correct answer: Clyde Tombaugh</div>
      </div>
      <div class="well">
        <div>Question 5: All I gotta say: Beware of the ________!</div>
        <div>You answered: <?php echo $q5; ?></div>
        <div>Correct answer: Phog</div>
      </div>


    </div>


    <!-- jQuery Plugin -->
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js"></script>
    <!-- Bootstrap JS Plugin -->
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
      integrity="sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" crossorigin="anonymous"></script>
  </body>
</html>
