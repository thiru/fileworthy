displayFileContent();
hljs.initHighlightingOnLoad();

function displayFileContent() {
  var rawFileEl = document.getElementById('raw-file-content');
  if (!rawFileEl)
    return;

  var filePath = document.querySelector('#files .selected').innerText.trim();

  if (filePath && filePath.endsWith('.md')) {
    var genMarkdown = marked(rawFileEl.innerText);
    document.getElementById('gen-file-content').innerHTML = genMarkdown;
  }
  else {
    rawFileEl.classList.remove('hidden');
  }
}
