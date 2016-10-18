function goToError(item) {
  alert('Quantities must be a non-negative number!')
  item.focus();
}

function validateForm() {
  for (var i = 1; i <= 6; i++) {
    var item = $('input[name="item' + i + '"]');
    var itemValue = item.val();
    if (itemValue < 0 || isNaN(itemValue)) {
      goToError(item);
      return false;
    }
  }

  return true;
}

$('form').keyup(function() {
  var quantities = [];
  for (var i = 1; i <= 6; i++) {
    var item = $('input[name="item' + i + '"]');
    quantities.push(item.val());
  }
  var quantityChanged = quantities.some(function(item) { return item > 0 });
  if (quantityChanged) {
    $(":submit").removeAttr("disabled");
  } else {
    $(":submit").attr("disabled", true);
  }
});
