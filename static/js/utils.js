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

function isBlank(text) {
  return !text || text.trim().length <= 0;
}

function pluralize(text, count, customPlural) {
  if (count == 1)
    return text;
  else
    return text + (customPlural == undefined ? "s" : customPlural);
}
// Language-Level ----------------------------------------------------------- END

// UI-Level
function get(id) {
  return document.getElementById(id);
}

var entityMap = {
  "&": "&amp;",
  "<": "&lt;",
  ">": "&gt;",
  '"': '&quot;',
  "'": '&#39;',
  "/": '&#x2F;'
};
function escapeHtml(text) {
  return String(text).replace(/[&<>"'\/]/g, function (s) {
      return entityMap[s];
      });
}

function onEnter(event, func) {
  if (event && func && event.keyCode === 13)
    func();
}
// UI-Level ----------------------------------------------------------------- END
