import {get} from './request.js';
import {config} from "./config";


/* Transform summary data grouped by day into data points
   grouped by type
*/
function groupByTypes(summaryData) {
  const types = {};
  summaryData.forEach(grp => {
    const day = new Date(grp.groupTime);
    grp.subGroup.leafViews.forEach(summary => {
      if (types[summary[0]] === undefined) {
        types[summary[0]] = [];
      }
      types[summary[0]].push([day, summary[1]]);
    });
  });
  return types;
}

function createSummaryContainer(name) {
  const container = <div id={name} class='summary' />;
  document.getElementById('summaries').appendChild(container);
  return container;
}

function drawChart(name, container, data) {
  var dataTable = new google.visualization.DataTable();
  dataTable.addColumn({ type: 'date', id: 'Date' });
  dataTable.addColumn({ type: 'number', id: 'Seconds' });
  dataTable.addRows(data);

  var chart = new google.visualization.Calendar(container);

  var options = {
    title: name + " Flow Summary",
    calendar: { cellSize: 10 }
  };

  chart.draw(dataTable, options);
}

function drawCharts(summaryData) {
  const types = groupByTypes(summaryData);
  for (var k in types) {
    const container = createSummaryContainer('summary_' + k);
    drawChart(k, container, types[k]);
  };
}

function fetchAllSummaryData() {
  get(`/flows/${config.user}/summary`, drawCharts);
}

export default function summary() {
  const obj = {};

  obj.fetchAllSummaryData = fetchAllSummaryData;

  return obj;
}
