import showdown from 'showdown';

function formatNote(note) {
  return "<div class='note'>" +
    new showdown.Converter({ simplifiedAutoLink: true }).makeHtml('#### ' + new Date(note.noteStart).toLocaleTimeString() + '\n\n' + note.noteContent) +
    "</div>";
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
