/*
 * Generic utils.
 */

// Language-Level

var Level = {
  success: 2,
  info: 1,
  debug: 0,
  warning: -1,
  error: -2,
  fatal: -3
};

function Result(level, msg, data)
{
  if (_.isNumber(level))
    this.level = level;
  else if (_.isString(level)) {
    var parsedLevel = Number(level);
    if (_.isNumber(parsedLevel) && !_.isNaN(parsedLevel))
      this.level = parsedLevel;
    else {
      level = level.toLowerCase();
      if (level == "success")
        this.level = Level.success;
      else if (level == "info")
        this.level = Level.info;
      else if (level == "debug")
        this.level = Level.debug;
      else if (level == "warning")
        this.level = Level.warning;
      else if (level == "error")
        this.level = Level.error;
      else if (level == "fatal")
        this.level = Level.fatal;
    }
  }
  else
    this.level = Level.error;

  this.message = msg || '';
  this.data = data;
}
Result.prototype.succeeded = function()
{
  return this.level >= 0;
}
Result.prototype.failed = function()
{
  return !this.succeeded();
}
Result.prototype.levelName = function()
{
  if (this.level >= 2)
    return "success";
  if (this.level < 2 && this.level >= 1)
    return "info";
  if (this.level < 1 && this.level >= 0)
    return "debug";
  if (this.level < 0 && this.level >= -1)
    return "warning";
  if (this.level < -1 && this.level >= -2)
    return "error";
  return "fatal";
}
Result.success = function(msg, data) {
  return new Result(Level.success, msg, data);
}
Result.info = function(msg, data) {
  return new Result(Level.info, msg, data);
}
Result.debug = function(msg, data) {
  return new Result(Level.debug, msg, data);
}
Result.warning = function(msg, data) {
  return new Result(Level.warning, msg, data);
}
Result.error = function(msg, data) {
  return new Result(Level.error, msg, data);
}
Result.fatal = function(msg, data) {
  return new Result(Level.fatal, msg, data);
}
Result.prototype.success = new Result(Level.info, '');
Result.prototype.failure = new Result(
    Level.failure,
    'An unspecified error occurred');

var utils =
{
  isBlank: function(text) {
    return !text || text.trim().length <= 0;
  },

  pluralize: function(text, count, customPlural) {
    if (count == 1)
      return text;
    else
      return text + (customPlural == undefined ? "s" : customPlural);
  },

  parseJson: function(str) {
    try {
      var obj = JSON.parse(str);
      return Result.success('JSON successfully parsed.', obj);
    }
    catch (ex) {
      console.log(ex);
      return Result.error(str, str);
    }
  },

  // Post `formData` to `url`.
  // The server response is expected to be a JSON object.
  post: function(url, formData, onComplete) {
    var request = new XMLHttpRequest();
    request.open('POST', url, true);

    request.onload = function() {
      var jsonResult = utils.parseJson(request.responseText);
      if (jsonResult.succeeded())
        jsonResult =
          new Result(
              jsonResult.data.level,
              jsonResult.data.message,
              jsonResult.data.data);

      if (request.status < 200 || request.status >= 400)
        jsonResult.message += '(HTTP status code ' + request.status + ')';

      onComplete(jsonResult);
    };

    request.onerror = function() {
      onComplete(Result.error('Connection error.'));
    };

    request.send(formData);
  }
};
// Language-Level ----------------------------------------------------------- END

// UI-Level
var ui = {
  entityMap: {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': '&quot;',
    "'": '&#39;',
    "/": '&#x2F;'
  },

  ready: function(fn) {
    if (document.readyState != 'loading')
      fn();
    else
      document.addEventListener('DOMContentLoaded', fn);
  },

  get: function(id) {
    return document.getElementById(id);
  },
  getQ: function(query) {
    return document.querySelector(query);
  },

  clear: function(el) {
    el.innerHTML = '';
  },

  hide: function(el) {
    el.classList.add('hidden');
  },
  unhide: function(el) {
    el.classList.remove('hidden');
  },
  isHidden: function(el) {
    return el.classList.contains('hidden');
  },
  toggleHidden: function(el) {
    el.classList.toggle('hidden');
  },

  escapeHtml: function(text) {
    return String(text).replace(/[&<>"'\/]/g, function (s) {
        return entityMap[s];
        });
  },

  showResult: function(el, result) {
    this.unhide(el);
    el.innerHTML = result.message;
    el.className = '';
    el.classList.add(result.levelName());
  },

  showLoading: function(el, msg) {
    el.className = '';
    el.innerHTML = '<i class="fa fa-spinner fa-pulse"></i> ' + msg;
  },

  disableBtn: function(el) {
  },

  onEnter: function(event, func) {
    if (event && func && event.keyCode === 13)
      func();
  },
};
// UI-Level ----------------------------------------------------------------- END
