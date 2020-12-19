import showdown from 'showdown';
import { config } from "./config";
import { get } from './request';
import { dom, clearElement } from './dom';
import { pagination } from './page';

function markdownNote(note) {
  return new showdown.Converter({ simplifiedAutoLink: true })
    .makeHtml('#### ' + new Date(note.noteStart).toLocaleTimeString() + '\n\n' + note.noteContent);
}

function formatNote(note) {
  return "<div class='note'>" +
    markdownNote(note) +
    "</div>";
}

function formatNoteDiv(note) {
  const noteDiv = <div class='note-full'></div>;
  noteDiv.innerHTML = markdownNote(note);
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

/*
  Display (latest) event log entries from the server
*/
export default function notes(router, container, page) {
  clearElement(container);
  get(`/flows/${config.user}/2020-12-18/notes`, (notesList, links) => {
    const notesPage = pagination('notes', router, links);
    const notesDiv =
      <div id='notes-list'>
        {
          notesList.map(formatNoteDiv)
        }
      </div>;
    container.appendChild(notesPage);
    container.appendChild(notesDiv);
  });
};
