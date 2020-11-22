import { get } from './request.js';
import { config } from "./config";
import { dom } from './dom.js';


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

function makeSummaryFlowsTable(summaryData) {
  var titles = ["Flow Types"];
  var data = ["Flows"];
  summaryData.summaryFlows.forEach(flow => {
    titles.push(flow[0]);
    data.push(flow[1]);
  });

  return [titles, data];
}

function makeSummaryCommandsTable(summaryData) {
  var titles = ["Commands"];
  var data = ["Commands"];
  summaryData.summaryCommands.forEach(flow => {
    titles.push(flow[0]);
    data.push(flow[1]);
  });
  titles.push({ role: 'annotation' });
  data.push('');
  return [titles, data];
}

function drawSummaryChart(container, summaryTable) {
  var flowsData = google.visualization.arrayToDataTable(summaryTable);
  var options = {
    height: 150,
    width: 1000,
    legend: { position: 'top', maxLines: 3 },
    bar: { groupWidth: '75%' },
    isStacked: true
  };
  var chart = new google.visualization.BarChart(container);

  chart.draw(flowsData, options);
}

export function drawSummary(container, summaryData) {
  const fdiv = <div class='summaryChart'></div>;
  const cdiv = <div class='summaryChart'></div>;

  drawSummaryChart(fdiv, makeSummaryFlowsTable(summaryData));
  drawSummaryChart(cdiv, makeSummaryCommandsTable(summaryData));

  container.appendChild(fdiv);
  container.appendChild(cdiv);
}
