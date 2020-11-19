// Handle displaying data about commands executed
import showdown from 'showdown';

/**
   Draw a timeline containing commands
*/
export function drawCommands(container, commandsData) {
  const chart = new google.visualization.Timeline(container);
  const dataTable = new google.visualization.DataTable();

  dataTable.addColumn({ type: 'string', id: 'Role' });
  dataTable.addColumn({ type: 'string', id: 'Command' });
  dataTable.addColumn({ type: 'date', id: 'Start' });
  dataTable.addColumn({ type: 'date', id: 'End' });
  commandsData.forEach(command => {
    let start = new Date(command.commandStart);
    dataTable.addRow(['Commands', command.commandProcess, start, new Date(start.getTime() + command.commandElapsed)]);
  });
  var options = {
    tooltip: { isHtml: true },
  };
  chart.draw(dataTable, options);
}
