var searchMode = false;

const sep = document.createElement('span');
sep.classList.add('seperator');
sep.appendChild(document.createTextNode("/"));

const searchButton = document.createElement('a');
searchButton.classList.add('searchButton');
searchButton.appendChild(document.createTextNode("search"));
searchButton.setAttribute('href', '#');

const searchInput = document.createElement('input');
searchInput.setAttribute('type', 'text');
searchInput.setAttribute('placeholder', 'Query');

var times = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
times.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 5.29167 10.58333" height="40" width="20"><g fill="none" stroke="#404040" stroke-width=".665" stroke-linecap="round"><path d="M.32573 4.0299l2.52354 2.52354M2.84927 4.0299L.32573 6.55344" stroke-width=".43228325"/></g></svg>`
times = times.children[0];

const searchBar = document.createElement('div');
searchBar.classList.add('searchBar');
searchBar.appendChild(times);
searchBar.appendChild(searchInput);

var content;
var header;

// for holding the document content
var stash = [];
// index is in the form of [[title, path]]
var index = [];

const writeIndex = (i) => {
  i.forEach(x => {
    var e = document.createElement('div');
    e.classList.add('searchItem');
    var a = document.createElement('a');
    a.appendChild(document.createTextNode(x[0]));
    a.setAttribute('href', "./" + x[1] + ".html");
    e.appendChild(a);
    content.appendChild(e);
  })
}

const updateIndex = (i) => {
  while (content.childElementCount > 2) {
    var c = content.children[2];
    content.removeChild(c);
  }
  writeIndex(i);
}

// extremely quick and dirty english-specific searching
const handleSearch = (e) => {
  var text = e.target.value.toLowerCase().trim();
  var query = index;
  if (text == "") {
    updateIndex(index);
    return;
  }
  query = query.filter(([title, path]) => {
    var t = title.toLowerCase();
    return t.includes(text);
  });
  query = query.sort((a, b) => {
    var t = a[0].toLowerCase();
    var i = t.indexOf(text);
    if (i == 0) {
      return false;
    } else if (i > 0 && t[i - 1] == " "){
      return false;
    } else {
      return a > b;
    }
  })
  updateIndex(query);
}

const searchOn = () => {
  // stash and remove document
  while (content.childElementCount > 1) {
    var c = content.children[1];
    stash.push(c);
    content.removeChild(c);
  }
  content.appendChild(searchBar);
  fetch('./index.json')
    .then(res => res.json())
    .then(x => {
      index = x;
      index.sort((a, b) => a > b);
      writeIndex(index);
      searchInput.onkeyup = handleSearch;
    });
  searchMode = true;
}

const searchOff = () => {
  // remove search content
  while (content.childElementCount > 1) {
    var c = content.children[1];
    content.removeChild(c);
  }
  // restore usual document
  while (stash.length > 0) {
    content.appendChild(stash.shift());
  }
  searchInput.onkeyup = () => { };
  searchInput.value = "";
  searchMode = false;
}

const toggleSearch = (e) => {
  if (searchMode) {
    searchOff();
  } else {
    searchOn();
  }
  e.preventDefault();
}

window.onload = () => {
  header = document.getElementById('header');
  content = document.getElementsByClassName('content')[0];

  header.appendChild(sep);
  header.appendChild(searchButton);
  searchButton.onclick = toggleSearch;
  times.onclick = searchOff;
}
