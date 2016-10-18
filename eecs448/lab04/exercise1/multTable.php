<?php

  // Debugging purposes
  error_reporting(E_ALL);
  ini_set("display_errors", 1);

  function generateMultTable($x, $y) {
    $table = '<table border="1" style="table-layout: fixed; text-align: center;">';

    for ($i = 0; $i <= $y; $i++) {
      if ($i == 0) {
        $table .= '<th></th>';
      } else {
        $table .= '<th>' . $i . '</th>';
      }
    }

    for ($i = 1; $i <= $x; $i++) {
      $table .= '<tr>';
      $table .= '<th>' . $i . '</th>';
      for ($j = 1; $j <= $y; $j++) {
        $table .= '<td>' . ($i * $j) . '</td>';
      }
      $table .= '</tr>';
    }
    $table .= '</table>';
    return $table;
  }

  echo generateMultTable(100, 100);

?>
