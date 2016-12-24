displayFileContent();

function displayFileContent() {
  var filePath = document.getElementById('file-path').innerText;
  var rawFileEl = document.getElementById('raw-file-content');

  if (filePath && filePath.endsWith('.md')) {
    var genMarkdown = marked(rawFileEl.innerText);
    document.getElementById('gen-file-content').innerHTML = genMarkdown;
  }
  else {
    rawFileEl.classList.remove('hidden');
  }
}
