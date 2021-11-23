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

function makeSummaryProjectsTable(summaryData) {
  return { data: makeSummaryTable(summaryData.summaryProjects, "Projects") };
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

function navigation(router, links) {
  if (!links) return <span />;

  return <div class='pagination'>
    {
      <button disabled={!links.prev} class='btn-prev' onclick={() => router.navigate(`/summaries/${links.prev.from}/${links.prev.to}/${links.prev.period}`)}>&lt;&lt;</button>
    }
    {
      <button disabled={!links.next} class='btn-next' onclick={() => router.navigate(`/summaries/${links.next.from}/${links.next.to}/${links.next.period}`)}>&gt;&gt;</button>
    }
  </div>;

}

export function drawSummary(container, router, summaryData, links) {
  const periodDiv = <div class='period'>
    <em>From:</em><span class='date'>{summaryData.summaryPeriod[0]}</span>
    <em>To:</em><span class='date'>{summaryData.summaryPeriod[1]}</span>
  </div>;
  const lnks = navigation(router, links);
  const fdiv = <div class='summaryChart'></div>;
  const cdiv = <div class='summaryChart'></div>;
  container.appendChild(periodDiv);
  container.appendChild(lnks);
  container.appendChild(fdiv);
  container.appendChild(cdiv);

  drawSummaryChart(fdiv, makeSummaryFlowsTable(summaryData));
  drawSummaryChart(cdiv, makeSummaryCommandsTable(summaryData));
  drawSummaryChart(cdiv, makeSummaryProjectsTable(summaryData));
}
