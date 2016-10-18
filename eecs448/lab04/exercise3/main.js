$('.store img').click(function() {
   $('#imgZoom').attr('src', $(this).attr('src'));
   $('#imgModal').modal('show');
});
