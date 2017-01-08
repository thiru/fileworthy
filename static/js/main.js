ui.ready(function() {
  hljs.initHighlightingOnLoad();

  if (ui.get("user-detail-page"))
    page.initUserDetailPage();
  else if (ui.get("fs-path-page"))
    page.initFileSystemPathPage();
});

// Site-Wide -------------------------------------------------------------------
var site = {
  toggleMenu: function() {
    ui.toggleHidden(ui.get('main-menu'));
  },

  showLogin: function() {
    ui.unhide(ui.get('overlay'));
    ui.get('login-dialog').style.display = 'flex';
    ui.get('login-email-address').focus();
  },
  closeLogin: function() {
    ui.hide(ui.get('overlay'));
    ui.get('login-dialog').style.display = 'none';
    ui.clear(ui.get('login-result'));
    ui.get('login-btn').disabled = false;
    ui.get('forgot-pwd').classList.remove('disabled');
  },
  login: function() {
    // Init
    var login = {
      email: ui.get('login-email-address').value,
      pwd: ui.get('login-pwd').value,
      loginBtn: ui.get('login-btn'),
      forgotPwdBtn: ui.get('forgot-pwd'),
      resultEl: ui.get('login-result')
    };

    ui.clear(login.resultEl);

    // Validate
    if (utils.isBlank(login.email))
      return ui.showResult(
          login.resultEl,
          Result.error('No email addresss provided.'));
    else if (utils.isBlank(login.pwd))
      return ui.showResult(
          login.resultEl,
          Result.error('No password provided.'));

    // Loading
    ui.showLoading(login.resultEl, 'Logging in...');
    login.loginBtn.disabled = true;
    login.forgotPwdBtn.classList.add('disabled');

    var formData = new FormData();
    formData.append('email', login.email);
    formData.append('pwd', login.pwd);

    // Send request
    utils.post(
        '/fileworthy/api/login',
        formData,
        function (result) {
          ui.showResult(login.resultEl, result);
          ui.get('login-btn').disabled = false;
          ui.get('forgot-pwd').classList.remove('disabled');

          if (result.succeeded()) {
            setTimeout(function() {
              if (_.endsWith(window.location.pathname.toLowerCase(),
                    "logout"))
                window.location = "/";
              else
                window.location.reload(false);
            }, 1000);
          }
        });
  }
};
// Site-Wide -------------------------------------------------------------------

var page = {};

// Admin - User Detail Page ----------------------------------------------------
page.initUserDetailPage = function() {
  page.save = function() {

    var els = {
      saveBtn: ui.get('save-btn'),
      saveRes: ui.get('save-result'),
    };

    var user = {
      id: parseInt(ui.get('name-heading').dataset.userId),
      name: ui.get('user-name').value,
      email: ui.get('email-address').value,
      isAdmin: (ui.get('is-admin') || {}).checked,
      currentPwd: ui.get('current-pwd').value,
      newPwd: ui.get('new-pwd').value,
      newPwdConfirm: ui.get('new-pwd-confirm').value
    };

    ui.clear(els.saveRes);

    // Validate
    if (utils.isBlank(user.name))
      return ui.showResult(
          els.saveRes,
          Result.error('No name provided.'));
    else if (utils.isBlank(user.email))
      return ui.showResult(
          els.saveRes,
          Result.error('No email address provided.'));
    else if (user.newPwd != user.newPwdConfirm)
      return ui.showResult(
          els.saveRes,
          Result.error('New passwords don\'t match.'));
    else if (utils.isBlank(user.currentPwd) && !utils.isBlank(user.newPwd))
      return ui.showResult(
          els.saveRes,
          Result.error('Current password required when setting a new ' +
                       'password.'));

    // Loading
    ui.showLoading(els.saveRes, 'Saving user...');
    els.saveBtn.disabled = true;

    var formData = new FormData();
    formData.append('name', user.name);
    formData.append('email', user.email);
    formData.append('isAdmin', user.isAdmin);
    formData.append('currentPwd', user.currentPwd);
    formData.append('newPwd', user.newPwd);

    // Send request
    utils.post(
        '/fileworthy/api/users/' + user.id,
        formData,
        function (result) {
          ui.showResult(els.saveRes, result);
          els.saveBtn.disabled = false;
        });
  };

  page.toggleChangePwd = function() {
    ui.toggleHidden(ui.get('password-fields'));
    ui.toggleHidden(ui.get('show-pwds-btn'));
    ui.toggleHidden(ui.get('hide-pwds-btn'));

    if (ui.get('password-fields').classList.contains('hidden')) {
      ui.get('current-pwd').value = '';
      ui.get('new-pwd').value = '';
      ui.get('new-pwd-confirm').value = '';
    }
  };
}
// Admin - User Detail Page ----------------------------------------------------

// File-System Path Page -------------------------------------------------------
page.initFileSystemPathPage = function() {
  page.displayFileContent = function() {
    var rawFileEl = ui.get('raw-file-content');
    if (!rawFileEl)
      return;

    var selectedFileEl = ui.getQ('#files .selected');
    if (!selectedFileEl)
      return;

    var filePath = selectedFileEl.innerText.trim();

    if (filePath && filePath.endsWith('.md')) {
      var genMarkdown = marked(rawFileEl.innerText);
      ui.get('gen-file-content').innerHTML = genMarkdown;
    }
    else {
      rawFileEl.classList.remove('hidden');
    }
  }

  page.displayFileContent();
}
// File-System Path Page -------------------------------------------------------
