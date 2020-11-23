import { get } from './request.js';
import { config } from "./config";
import { dom } from './dom.js';
import { colorOf } from './color.js';


function makeSummaryTable(dataPoints, title) {
  var titles = [title];
  var data = [title];
  dataPoints.forEach(flow => {
    titles.push(flow[0]);
    data.push(flow[1]);
  });
  titles.push({ role: 'annotation' });
  data.push('');

  return [titles, data];
}

function makeSummaryFlowsTable(summaryData) {
  const data = makeSummaryTable(summaryData.summaryFlows, "Flows");
  const colors = data[0].slice(1).map(flow => colorOf(flow));

  return { data, colors };
}

function makeSummaryCommandsTable(summaryData) {
  return { data: makeSummaryTable(summaryData.summaryCommands, "Commands") };
}

function drawSummaryChart(container, summaryTable) {
  const { data, colors } = summaryTable;
  var flowsData = google.visualization.arrayToDataTable(data);
  var options = {
    height: 100,
    width: '100%',
    legend: { position: 'top', maxLines: 3 },
    bar: { groupWidth: '75%' },
    isStacked: 'percent',
    colors
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
