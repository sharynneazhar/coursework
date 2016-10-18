//---------------------------------------------------------
// Password Validator
//---------------------------------------------------------

// Used to toggle the popup password hint
$('[data-toggle="popover"]').popover();

function clearErrors() {
  $('input').parent().removeClass('has-error has-success');
  $('.message').html('');
}

function validate() {
  var password = $('input[name="password"]').val();
  var passwordConfirm = $('input[name="password-confirm"]').val();
  if (password.length < 8) {
    $('.message').html('Must be at least 8 characters').addClass('text-danger');
    $('input[name="password-confirm"]').parent().addClass('has-error');
  } else if (password != passwordConfirm) {
    $('.message').html('Passwords entered do not match').addClass('text-danger');
    $('input[name="password-confirm"]').parent().addClass('has-error');
  } else {
    $('.message').html('Passwords match!').removeClass('text-danger').addClass('text-success');
    $('input[name="password-confirm"]').parent().removeClass('has-error').addClass('has-success');
    $("form").trigger('reset');
  }
}

//---------------------------------------------------------
// Slideshow
//---------------------------------------------------------

var imgIndex = 1;
var images = [
  '../img/img1.jpg',
  '../img/img2.jpg',
  '../img/img3.jpg',
  '../img/img4.png',
  '../img/img5.jpg',
  '../img/img6.jpg',
  '../img/img7.jpg',
  '../img/img8.jpg',
  '../img/img9.png'
];

function prevImg() {
  var img = document.getElementById('slideshow');
  (imgIndex > 0) ? imgIndex-- : imgIndex = 8;
  img.src = images[imgIndex];
}

function nextImg() {
  var img = document.getElementById('slideshow');
  (imgIndex < 8) ? imgIndex++ : imgIndex = 0;
  img.src = images[imgIndex];
}


//--------------------------------------------------------
// Profile
//--------------------------------------------------------

$('.avatar img').hover(
  function() {
    $(this).attr('src', '../img/cartoon.jpg');
  }, function() {
    $(this).attr('src', '../img/profile.jpg');
  }
);

$('.chevron').click(function() {
  $('html, body').animate({
      scrollTop: $("#video").offset().top
  }, 1000);
});


//--------------------------------------------------------
// CSS Manipulation
//--------------------------------------------------------

var width = 0;
var red = 0;
var blue = 0;
var green = 0;

// background container hidden by default
$('.bg-container').hide();

function checkRange(val) {
  return (val >= 0 && val <= 255);
}

function toggleBorder() {
  $('.btn-bg').removeClass('btn-primary').addClass('btn-default');
  $('.btn-border').removeClass('btn-default').addClass('btn-primary');
  $('.border-container').show();
  $('.bg-container').hide();
  $('.border-code').html(
    `<pre>{<br>&nbsp;&nbsp;border: ${width}px solid rgb(${red}, ${green}, ${blue});<br>}</pre>`
  );
}

function toggleBg() {
  $('.btn-border').removeClass('btn-primary').addClass('btn-default');
  $('.btn-bg').removeClass('btn-default').addClass('btn-primary');
  $('.bg-container').show();
  $('.border-container').hide();
  $('.bg-code').html(
    `<pre>{<br>&nbsp;&nbsp;background-color: rgb(${red}, ${green}, ${blue});<br>}</pre>`
  );
}

function changeBorder() {
  width = $('#border input[name="width"]').val() || 0;
  red = $('#border input[name="red"]').val() || 0;
  blue = $('#border input[name="blue"]').val() || 0;
  green = $('#border input[name="green"]').val() || 0;
  $('.paragraph').css({
    'border': `${width}px solid rgb(${red},${green},${blue})`
  });
  $('.border-code').html(
    `<pre>{<br>&nbsp;&nbsp;border: ${width}px solid rgb(${red}, ${green}, ${blue});<br>}</pre>`
  );
}

function changeBg() {
  red = $('#background input[name="bg-red"]').val() || 255;
  blue = $('#background input[name="bg-blue"]').val() || 255;
  green = $('#background input[name="bg-green"]').val() || 255;
  $('.paragraph').css({
    'background-color': `rgb(${red},${green},${blue})`
  });
  $('.bg-code').html(
    `<pre>{<br>&nbsp;&nbsp;background-color: rgb(${red}, ${green}, ${blue});<br>}</pre>`
  );
}
