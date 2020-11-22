import { get } from './request.js';
import { config } from "./config";
import { dom } from './dom.js';

function makeSummaryTable(dataPoints, title) {
  var titles = [title];
  var data = [title];
  summaryData.forEach(flow => {
    titles.push(flow[0]);
    data.push(flow[1]);
  });
  titles.push({ role: 'annotation' });
  data.push('');

  return [titles, data];
}

function makeSummaryFlowsTable(summaryData) {
  return makeSummaryTable(summaryData.summaryFlos, "Flows");
}

function makeSummaryCommandsTable(summaryData) {
  return makeSummaryTable(summaryData.summaryFlos, "Commands");
}

function drawSummaryChart(container, summaryTable) {
  var flowsData = google.visualization.arrayToDataTable(summaryTable);
  var options = {
    height: 100,
    width: '100%',
    legend: { position: 'top', maxLines: 3 },
    bar: { groupWidth: '75%' },
    isStacked: 'percent',
  };
  var chart = new google.visualization.BarChart(container);

  chart.draw(flowsData, options);
}

export function drawSummary(container, summaryData) {
  const fdiv = <div class='summaryChart'></div>;
  const cdiv = <div class='summaryChart'></div>;
  container.appendChild(fdiv);
  container.appendChild(cdiv);

  drawSummaryChart(fdiv, makeSummaryFlowsTable(summaryData));
  drawSummaryChart(cdiv, makeSummaryCommandsTable(summaryData));
}
