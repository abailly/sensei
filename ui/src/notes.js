import { config } from "./config";
import { get } from './request';
import { dom, clear, clearElement } from './dom';
import { pagination } from './page';
import { formatISODateTime } from "./date";
import { LocalDateTime } from "@js-joda/core";
import markdown from "./markdown";


export function formatNote(note) {
  return "<div class='note'>" + markdown(note.noteView, note.noteStart) + "</div>";
}

function formatTags(tags) {
  return <h4 class='note-tags'>
    {tags.map(tag => (<span class='note-tag'>{tag}</span>))}
  </h4>;
};

function formatNoteDiv(note) {
  const noteDate = formatISODateTime(LocalDateTime.parse(note.noteStart));
  const noteDiv =
    <div class='note-full'>
      <h3><a href={'/notes/' + noteDate}>{noteDate}</a></h3>
      <h4>{note.noteProject}</h4>
      {formatTags(note.noteTags)}
    </div>;

  const content = <div class='note-content'>
  </div>;

  content.innerHTML = markdown(note.noteView);
  noteDiv.appendChild(content);
  return noteDiv;
}

/**
   Draw a timeline containing notes
*/
export function drawNotes(container, notesData) {
  const chart = new google.visualization.Timeline(container);
  const dataTable = new google.visualization.DataTable();

  dataTable.addColumn({ type: 'string', id: 'Role' });
  dataTable.addColumn({ type: 'string', id: 'dummy bar label' });
  dataTable.addColumn({ type: 'string', role: 'tooltip', p: { 'html': true } });
  dataTable.addColumn({ type: 'date', id: 'Start' });
  dataTable.addColumn({ type: 'date', id: 'End' });
  notesData.forEach(note => {
    let start = new Date(note.noteStart);
    dataTable.addRow(['Notes', '', formatNote(note), start, new Date(start.getTime() + 60000)]);
  });
  var options = {
    tooltip: { isHtml: true },
  };
  chart.draw(dataTable, options);
}

function list(router, container, page) {
  clearElement(container);
  get(router, `/api/flows/${config.user}/${page}/notes`, (notesList, links) => {
    const notesPage = pagination('notes', router, links);
    const notesDiv =
      <div id='daily-notes'>
        <h2>Notes for {page} </h2>
        {
          notesList.map(formatNoteDiv)
        }
      </div>;
    container.appendChild(notesPage);
    container.appendChild(notesDiv);
  });
};

function search(router, container) {
  clearElement(container);
  var debounce = false;
  function do_search(e) {
    const q = e.target.value;
    const results = document.getElementById('search-results');
    clearElement(results);

    if (q.length > 0 && !debounce) {
      debounce = true;
      get(router, `/api/notes/${config.user}?search=${encodeURI(q)}`, (searchResult) => {
        const resList = <div>
          {
            searchResult.map(formatNoteDiv)
          }
        </div>;
        results.appendChild(resList);
        debounce = false;
      });
    }
  };
  const searchDiv = <div id='notes-search'>
    <div id='search-input'><label for='search-query'>Search</label><input name='search-query' id='search-query' type='text' oninput={do_search} /></div>
    <div id='search-results'></div>
  </div>;
  container.appendChild(searchDiv);
};

/*
  Display (latest) event log entries from the server
*/
export default { list, search };
