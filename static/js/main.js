ui.ready(function() {
  site.rrp = document.body.dataset.rrp;

  if (ui.get('user-detail-page'))
    page.initUserDetailPage();
  else if (ui.get('fs-path-page'))
    page.initFileSystemPathPage();
  else if (ui.get('settings-page'))
    page.initSettingsPage();
});

// Site-Wide -------------------------------------------------------------------
var site = {
  toggleMenu: function() {
    ui.toggleHidden(ui.get('info-menu'));
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
        '/' + site.rrp + '/api/login',
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

// Admin - Settings Page -------------------------------------------------------
page.initSettingsPage = function() {
  page.save = function() {
    var els = {
      saveBtn: ui.get('save-btn'),
      saveRes: ui.get('save-result'),
    };

    var settings = {
      siteName: ui.get('site-name').value,
      rootDir: ui.get('root-dir').value,
      port: parseInt(ui.get('port').value),
      anonRead: (ui.get('anon-read') || {}).checked,
      rrp: ui.get('rrp').value.replace(/^\/+|\/+$/g, '')
    };

    ui.clear(els.saveRes);

    // Validate
    if (isNaN(settings.port) || settings.port <= 0)
      return ui.showResult(
          els.saveRes,
          Result.error('Port must be a positive integer.'));
    else if (utils.isBlank(settings.rrp))
      return ui.showResult(
          els.saveRes,
          Result.error('Reserved Resource Path is required.'));

    // Loading
    ui.showLoading(els.saveRes, 'Saving settings...');
    els.saveBtn.disabled = true;

    var formData = new FormData();
    formData.append('siteName', settings.siteName);
    formData.append('rootDir', settings.rootDir);
    formData.append('port', settings.port);
    formData.append('anonRead', settings.anonRead);
    formData.append('rrp', settings.rrp);

    // Send request
    utils.post(
        '/' + site.rrp + '/api/settings/',
        formData,
        function (result) {
          ui.showResult(els.saveRes, result);
          els.saveBtn.disabled = false;
        });

  }
}
// Admin - Settings Page -------------------------------------------------------

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
      rootDir: (ui.get('root-dir') || {}).value,
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

    // Loading
    ui.showLoading(els.saveRes, 'Saving user...');
    els.saveBtn.disabled = true;

    var formData = new FormData();
    formData.append('name', user.name);
    formData.append('email', user.email);
    formData.append('rootDir', user.rootDir);
    formData.append('isAdmin', user.isAdmin);
    formData.append('currentPwd', user.currentPwd);
    formData.append('newPwd', user.newPwd);

    // Send request
    utils.post(
        '/' + site.rrp + '/api/users/' + user.id,
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

  page.searchTypeEl = ui.get('search-type');
  page.searchEl = ui.get('search');
  page.searchInfoEl = ui.get('search-info');
  page.searchResultsEl = ui.get('search-results');
  page.rawFileEl = ui.get('raw-file-content');
  page.selectedFileEl = ui.getQ('#files .selected');

  if (page.selectedFileEl) {
    page.filePath = page.selectedFileEl.innerText.trim() || '';
    page.fileIsMarkdown = /\.mk?d$/i.test(page.filePath);
  }

  page.displayFileContent = function() {
    if (!page.rawFileEl || !page.selectedFileEl)
      return;

    if (page.fileIsMarkdown) {
      marked.setOptions({
        highlight: function(code, lang, callback) {
          callback(null, hljs.highlightAuto(code).value);
        }
      });

      var genHtmlEl = ui.get('gen-file-content');
      marked(page.rawFileEl.innerText, function (err, content) {
        if (err) throw err;
        genHtmlEl.innerHTML = content;
      });
    }
    else {
      page.rawFileEl.classList.remove('hidden');
    }
  }

  page.runSyntaxHighlight = function() {
    if (!page.filePath || page.fileIsMarkdown)
      return;

    var codeEl = page.rawFileEl;
    var msgObj = {
      code: codeEl.textContent,
      rrp: site.rrp
    };
    var worker = new Worker('/' + site.rrp + '/js/highlight-worker.js');
    worker.onmessage = function(event) { codeEl.innerHTML = event.data; }
    worker.postMessage(msgObj);
  }

  page.initSearch = function() {
    Rx.Observable.fromEvent(page.searchEl, "keyup")
      .map(function(event) { return event.target.value.trim(); })
      .debounceTime(400)
      .distinctUntilChanged()
      .subscribe(page.search);
  }

  page.search = function(searchTxt) {
    var searchType = page.searchTypeEl.value;

    if (!searchTxt)
      searchTxt = page.searchEl.value;

    searchTxt = (searchTxt || '').trim();

    // Don't show any results if blank, and clear any existing results
    if (utils.isBlank(searchTxt)) {
      page.searchInfoEl.classList.add('hidden');
      page.searchResultsEl.classList.add('hidden');
      page.searchResultsEl.innerHTML = '';
      return;
    }

    var formData = new FormData();
    formData.append('search-type', searchType);
    formData.append('search', searchTxt);

    ui.showLoading(page.searchInfoEl, 'Searching...');

    // Send request
    utils.post(
        '/' + site.rrp + '/api/search',
        formData,
        function (result) {
          page.searchInfoEl.innerHTML = '';
          if (result.succeeded())
            page.fillSearchResults(result.data);
          else
            page.showNoMatches();
        });
  }

  page.showNoMatches = function() {
    page.searchResultsEl.innerHTML = '';
    page.searchResultsEl.classList.add('hidden');
    page.searchInfoEl.innerText = 'No matches';
    page.searchInfoEl.className = 'warning';
  }

  page.fillSearchResults = function(newItems) {
    if (!newItems || !newItems.length) {
      page.showNoMatches();
      return;
    }

    if (newItems.length == 1)
      page.searchResultsEl.size = 2;
    else
      page.searchResultsEl.size =
        Math.min(newItems.length, page.searchResultsEl.dataset.defaultSize);

    page.searchResultsEl.innerHTML = '';
    page.searchResultsEl.classList.remove('hidden');

    for (var i = 0; i < newItems.length; i++) {
      var optionEl = document.createElement('option');
      optionEl.value = newItems[i];
      optionEl.text = newItems[i];
      page.searchResultsEl.appendChild(optionEl);
    }
  }

  page.onSearchTypeChange = function(event) {
    var idx = page.searchTypeEl.selectedIndex;
    page.searchEl.placeholder = page.searchTypeEl.options[idx].dataset.longText;
  }

  page.onSearchTxtClick = function(event) {
    if (page.searchResultsEl.options.length)
      page.searchResultsEl.classList.toggle('hidden');
  }

  page.onSearchTxtKeyDown = function(event) {
    // Hide search results on ESC
    if (event.key == 'Escape' || event.key == 'Esc') {
      event.preventDefault();
      page.searchResultsEl.classList.add('hidden');
    }
    // Choose previous search type (if search text is blank)
    else if (event.key == 'ArrowUp' && utils.isBlank(page.searchEl.value)) {
      event.preventDefault();
      if (page.searchTypeEl.selectedIndex <= 0)
        page.searchTypeEl.selectedIndex = page.searchTypeEl.options.length - 1;
      else
        page.searchTypeEl.selectedIndex--;
      page.onSearchTypeChange();
    }
    // Choose next search type (if search text is blank)
    else if (event.key == 'ArrowDown' && utils.isBlank(page.searchEl.value)) {
      event.preventDefault();
      if (page.searchTypeEl.selectedIndex >=
          (page.searchTypeEl.options.length - 1))
        page.searchTypeEl.selectedIndex = 0;
      else
        page.searchTypeEl.selectedIndex++;
      page.onSearchTypeChange();
    }
    // Vim-like motion to select first item
    else if (event.ctrlKey && event.key == 'j' &&
             page.searchResultsEl.options.length) {
      event.preventDefault();
      page.searchResultsEl.classList.remove('hidden');
      page.searchResultsEl.selectedIndex = 0;
      page.searchResultsEl.focus();
    }
  }

  page.onSearchTxtKeyUp = function(event) {
    if (event.key == 'Enter') {
      if (utils.isBlank(page.searchEl.value))
        page.searchEl.value = '*';
      else
        page.search();
    }
    // Focus first search item on down arrow
    else if (event.key == 'ArrowDown' &&
             page.searchResultsEl.options.length) {
      page.searchResultsEl.classList.remove('hidden');
      page.searchResultsEl.selectedIndex = 0;
      page.searchResultsEl.focus();
    }
  }

  page.onSearchResultsKeyDown = function(event) {
    // Close on ESC
    if (event.key == 'Escape' || event.key == 'Esc') {
      page.searchResultsEl.classList.add('hidden');
    }
    // Go to path
    else if (event.key == 'Enter') {
      window.location = '/' + page.searchResultsEl.value;
    }
    // Vim-like motion to move down
    else if (event.ctrlKey && event.key == 'j') {
      event.preventDefault();
      if (page.searchResultsEl.selectedIndex < (page.searchResultsEl.options.length + 1))
        page.searchResultsEl.selectedIndex += 1;
    }
    // Escape and focus search text
    else if (event.key == 'ArrowUp' && page.searchResultsEl.selectedIndex == 0) {
      page.searchEl.focus();
    }
    // Vim-like motion to escape and focus search text or move up
    else if (event.ctrlKey && event.key == 'k') {
      event.preventDefault();
      if (page.searchResultsEl.selectedIndex == 0)
        page.searchEl.focus();
      else if (page.searchResultsEl.selectedIndex > 0)
        page.searchResultsEl.selectedIndex -= 1;
    }
  }

  page.onSearchResultsClick = function(event) {
    window.location = '/' + page.searchResultsEl.value;
  }

  page.displayFileContent();
  page.runSyntaxHighlight();
  page.initSearch();
}
// File-System Path Page -------------------------------------------------------
