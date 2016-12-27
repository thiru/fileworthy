displayFileContent();
hljs.initHighlightingOnLoad();

function displayFileContent() {
  var filePath = document.querySelector('#files .selected').innerText.trim();
  var rawFileEl = document.getElementById('raw-file-content');

  if (filePath && filePath.endsWith('.md')) {
    var genMarkdown = marked(rawFileEl.innerText);
    document.getElementById('gen-file-content').innerHTML = genMarkdown;
  }
  else {
    rawFileEl.classList.remove('hidden');
  }
}
