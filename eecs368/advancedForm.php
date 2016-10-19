<!DOCTYPE html>
<html>
    <head>
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Advanced Form | Sharynne Azhar</title>

        <link rel="stylesheet" href="css/form.css">

        <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.2/jquery.min.js"></script>
        <script src="js/advancedForm.js" charset="utf-8"></script>

    </head>
    <body>
        <div class="logo">
            <img src="img/KU_Logo.gif" alt="University of Kansas" />
        </div>
        <form class="form-area">
            <h1 class="title">EECS 368: Programming Paradigms</h1>
            <span id="error"></span>
            <div class="form-field">
                <label for="username">Username</label>
                <input type="text" id="username" name="username">
            </div>
            <div class="form-field">
                <label for="pasword">Password</label>
                <input type="password" id="password" name="password">
            </div>
            <div class="form-field">
                <label for="pasword">Confirm</label>
                <input type="password" id="confirm-pwd" name="confirm-pwd">
            </div>
            <div class="form-field">
                <label for="image">Image Link</label>
                <input type="text" id="image" name="image">
            </div>
            <div class="button-area" style="margin-top: 10px">
                <button type="submit" name="submit">Submit</button>
            </div>
            <div class="button-area">
                <button type="reset" name="clear">Clear</button>
            </div>
        </form>
        <hr>
        <div class="users-area">
            <h2>User Listing</h2>
            <?php

                echo "<table class=\"users-table\">";
                echo "<tr><th>Username</th><th>Password</th></tr>";

                $file = fopen("usersList.txt", "r");
                while (!feof($file)) {
                    $line = fgets($file);
                    $array = explode(",", $line);
                    echo "<tr><td>$array[0]</td><td>$array[1]</td></tr>";
                }

                echo "</table>";
                fclose($file);
            ?>
        </div>

        <div class="test"></div>
</body>
</html>
