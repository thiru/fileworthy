ui.ready(function() {
  var loginSuccessEl = ui.get('login-successful');
  if (loginSuccessEl)
    Page.goBackTo(loginSuccessEl.dataset.goBackToUrl);

  $('#email').focus();
});

var Page = {
  goBackTo: function(url) {
    url = url || '/';
    setTimeout(
      function() { window.location.href = url },
      1000);
  }
}
