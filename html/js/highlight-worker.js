onmessage = function(event) {
  importScripts('/' + event.data.rrp + '/deps/highlightjs/highlight.pack.js');
  var result = self.hljs.highlightAuto(event.data.code);
  postMessage(result.value);
}
