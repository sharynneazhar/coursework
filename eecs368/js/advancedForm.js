$(function() {

    function addUser() {
        var ajaxFilename = 'users.php';
        var data = {
            user: $('#username').val(),
            password: $('#password').val(),
        };

        var jqxhr = $.post(ajaxFilename, data, function(datafromserver) {
            $('.test').html(datafromserver);
        });

        $('form').trigger('reset');
    }

    $("#error").hide(); // hide the error element until needed
    function displayError(errorMsg) {
        console.log(errorMsg);

        $('#error').css({
            'margin-bottom': '10px',
            'background-color': '#FF3333'
        });

        $('#error').show().delay(2000).fadeOut();
        $('#error').append(errorMsg);
    }

    function checkPassword() {
        if ($('input[name=password]').val() !== $('input[name=confirm-pwd]').val())
            return false;

        return true;
    }

    // function addUser() {
    //     var username = $('#username').val();
    //     var password = $('#password').val();
    //
    //     var avatar;
    //     if ($('input[name=image]').val() === "") {
    //         avatar = 'img/user.png';
    //     } else {
    //         avatar = $('#image').val();
    //     }
    //
    //     var newRow = "";
    //     newRow += "<tr><td><img src=\"" + avatar + "\" height=\"32px\" /></td><td>" + username + "</td><td>" + password + "</td></tr>";
    //     $(newRow).appendTo('.users-table');
    //
    //     $('form').trigger('reset');
    // }

    // begin submit process
    $('button[name=submit]').click(function(e) {
        e.preventDefault();

        if ($('input[name=username]').val() != "" && $('input[name=password]').val() != "") {
            if (checkPassword()) {
                addUser();
            }
            else {
                displayError('Passwords do not match.');
            }
        }

    });

});
