<!DOCTYPE html>
<html>
<head>
  <title>EECS 448 | Lab 04</title>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css" integrity="sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp" crossorigin="anonymous">
  <link rel="stylesheet" href="../css/main.css">
  <link rel="stylesheet" href="style.css">
</head>
<body>
  <nav class="navbar navbar-default">
    <div class="container">
      <a class="navbar-brand" href="#">The Practical Developer Web Store</a>
      <ul class="nav pull-right">
        <li><a href="customerFrontend.html">Back to Store</a></li>
      </ul>
    </div>
  </nav>
  <div class="container">

    <?php
      // Debugging purposes
      error_reporting(E_ALL);
      ini_set("display_errors", 1);

      // set the default timezone to use. Available since PHP 5.1
      date_default_timezone_set('UTC');

      // let's print the international format for the en_US locale
      setlocale(LC_MONETARY, 'en_US');
      function toMoney($value) {
        return money_format('%.2n', $value);
      }

      $email = $_POST['email'];
      $password = $_POST['password'];
      $shipping = $_POST['shipping'];

      $username = substr($email, 0, strrpos($email, '@'));

      $shippingCost = 0;
      switch ($shipping) {
        case 'threeDay':
          $shippingCost = toMoney(5);
          $shippingMethod = 'Three Day';
          break;
        case 'overnight':
          $shippingCost = toMoney(50);
          $shippingMethod = 'Overnight';
          break;
        default:
          $shippingCost = toMoney(0);
          $shippingMethod = 'Free';
          break;
      }

      $item1 = $_POST['item1']; // $14
      $item2 = $_POST['item2']; // $10
      $item3 = $_POST['item3']; // $10
      $item4 = $_POST['item4']; // $18
      $item5 = $_POST['item5']; // $40
      $item6 = $_POST['item6']; // $21

      $orderTotal = toMoney(($item1 * 14) + ($item2 * 10) + ($item3 * 10) +
                            ($item4 * 18) + ($item5 * 40) + ($item6 * 21));
    ?>

    <h2>Welcome, <?php echo $username ?></h2>
    <div class="userInfo">
      <h4><b>Email:</b> <?php echo $email ?></h4>
      <h4><b>Password:</b> <?php echo $password ?></h4>
    </div>

    <hr>

    <div class="receipt">
      <div class="order-details">
        <p><b>Order Placed:</b> <?php echo date("F j, Y"); ?></p>
        <p><b>Order Number:</b> <?php echo uniqid() ?></p>
        <p><b>Order Total:</b> <?php echo $orderTotal ?></p>
      </div>
      <div class="receipt-details">
        <div class="table-responsive">
          <table class="table table-bordered">
            <tbody>
              <tr>
                <th>Item</th>
                <th>Quantity</th>
                <th>Price</th>
                <th>Subtotal</th>
              </tr>
              <tr>
                <th>
                  Copying and Pasting from Stack Overflow <br>
                  <p>Cutting corners to meet arbitrary management deadlines.</p>
                </th>
                <td><?php echo $item1 ?></td>
                <td>$14.00</td>
                <td><?php echo toMoney($item1 * 14) ?></td>
              </tr>
              <tr>
                <th>
                  Pointless Meetings <br>
                  <p>How to survive all the pointless meetings.</p>
                </th>
                <td><?php echo $item2 ?></td>
                <td>$10.00</td>
                <td><?php echo toMoney($item2 * 10) ?></td>
              </tr>
              <tr>
                <th>
                  Blaming the User <br>
                  <p>You're a 10x hacker and it must be someone else's fault.</p>
                </th>
                <td><?php echo $item3 ?></td>
                <td>$10.00</td>
                <td><?php echo toMoney($item3 * 10) ?></td>
              </tr>
              <tr>
                <th>
                  Resume Driven Development <br>
                  <p>The passionate, functional, micro-serviced approach.</p>
                </th>
                <td><?php echo $item4 ?></td>
                <td>$18.00</td>
                <td><?php echo toMoney($item4 * 18) ?></td>
              </tr>
              <tr>
                <th>
                  Googling the Error Message <br>
                  <p>The internet will make those bad words go away.</p>
                </th>
                <td><?php echo $item5 ?></td>
                <td>$40.00</td>
                <td><?php echo toMoney($item5 * 40) ?></td>
              </tr>
              <tr>
                <th>
                  "Temporary" Workarounds <br>
                  <p>Who are you kidding?</p>
                </th>
                <td><?php echo $item6 ?></td>
                <td>$21.00</td>
                <td><?php echo toMoney($item6 * 21) ?></td>
              </tr>
              <tr>
                <th colspan="2" class="text-center">Shipping &amp; Handling</th>
                <td><?php echo $shippingMethod ?></td>
                <td><?php echo $shippingCost ?></td>
              </tr>
              <tr>
                <th colspan="3" class="text-center">Total Cost</th>
                <td class="text-danger"><b><?php echo $orderTotal ?></b></td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>
  </div>


  <!-- jQuery Plugin -->
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js"></script>
  <!-- Bootstrap JS Plugin -->
  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
  integrity="sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" crossorigin="anonymous"></script>

  <script type="text/javascript" src="formChecker.js"></script>
  <script type="text/javascript" src="main.js"></script>
</body>
</html>
