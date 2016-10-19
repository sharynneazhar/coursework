$(document).ready(function(){

    var campusRide = {
        content: 'This is an idea for a mock web-app that allows users, in this case KU students, to request campus rides 24/7. Similar to what Uber does. SafeRide (as far as I know) only provides night rides. There are a lot of complexities to this project idea, but it will definitely be an awesome learning experience.',
        placement: 'bottom'
    };

    var twenty48 = {
        content: 'A web-app replica of the 2048 game. One variation is to make it HTML based. For example, <p> goes to <h1> goes to <div> goes to <body> goes to <html>.',
        placement: 'bottom'
    };

    var twenty48 = {
        content: 'A web-app replica of the 2048 game. One variation is to make it HTML based. For example, <p> goes to <h1> goes to <div> goes to <body> goes to <html>.',
        placement: 'bottom'
    };

    var slack = {
        content: 'Create a social networking interface like Slack. Allow users to chat and upload random giphys -- this probably involves API integrations which could be more complex that it seems.',
        placement: 'bottom'
    };

    var kiss = {
        content: 'Design a web-app that users can use to clean up their code styles. No more sloppy coders! Perhaps, start with HTTML support and add more later.',
        placement: 'bottom'
    };

    $('[data-toggle="campusRide"]').popover(campusRide);
    $('[data-toggle="2048"]').popover(twenty48);
    $('[data-toggle="slack"]').popover(slack);
    $('[data-toggle="kiss"]').popover(kiss);

});

/***** Dismiss all popovers by clicking outside, don't dismiss if clicking inside the popover content  **************/

$('html').on('click', function(e) {
    if (typeof $(e.target).data('original-title') == 'undefined' &&
    !$(e.target).parents().is('.popover.in')) {
        $('[data-original-title]').popover('hide');
    }
});
