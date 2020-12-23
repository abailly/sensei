import { get } from './request.js';
import { formatISODate } from './date.js';
import { drawNotes } from './notes.js';
import { drawCommands } from './commands.js';
import { drawSummary } from './summary.js';
import { dom, clearElement } from './dom.js';
import { config } from "./config";
import { colorOf } from './color.js';
import {drawTimeline} from "./css-timeline.js";

/**
   Draw a timeline chart in given element with given data
*/
function drawChart(container, selectedDate, flowData, rowLabelling = (_ => selectedDate)) {
  const chart = new google.visualization.Timeline(container);
  const dataTable = new google.visualization.DataTable();

  dataTable.addColumn({ type: 'string', id: 'Role' });
  dataTable.addColumn({ type: 'string', id: 'Flow Type' });
  dataTable.addColumn({ type: 'string', id: 'style', role: 'style' });
  dataTable.addColumn({ type: 'date', id: 'Start' });
  dataTable.addColumn({ type: 'date', id: 'End' });
  flowData.forEach(flow =>
    dataTable.addRow([rowLabelling(flow), flow.flowType, colorOf(flow.flowType), new Date(flow.flowStart), new Date(flow.flowEnd)])
  );
  chart.draw(dataTable);
}

/**
   Create a new div container for a timeline
*/
function createTimelineContainer(day, data, notesData) {
  const detailsName = 'checkbox-' + day;
  const notesName = 'notes-checkbox-' + day;
  const commandsName = 'cmd-checkbox-' + day;
  const summaryName = 'summary-checkbox-' + day;
  const chart = <div id={'chart-' + day} class="timeline-chart" />;
  const chart2 = <div class="timeline-chart" />;
  const notesDiv = <div id={'notes-' + day} class="timeline-chart" />;
  const commandsDiv = <div id={'commands-' + day} class="timeline-chart" />;
  const summaryDiv = <div id={'summary-' + day} class="summary" />;
  const details = <input type="checkbox" id={detailsName} />;
  const notes = <input type="checkbox" id={notesName} />;
  const commands = <input type="checkbox" id={commandsName} />;
  const summary = <input type="checkbox" id={summaryName} />;

  const container =
    <div id={day} class='timeline'>
      <div class='timeline-controls'>
        <label for={detailsName}>Expand</label>
        {details}
        <label for={notesName}>Notes</label>
        {notes}
        <label for={commandsName}>Commands</label>
        {commands}
        <label for={summaryName}>Summary</label>
        {summary}
      </div>
      {chart}
      {notesDiv}
      {commandsDiv}
      {summaryDiv}
      {chart2}
    </div>;

  details.addEventListener('change', (e) => {
    if (e.target.checked) {
      // drawChart(chart, day, data, f => f.flowType);
      drawTimeline(chart2, day, data);
    } else {
      // drawChart(chart, day, data);
      drawTimeline(chart2, day, data);
    }
  });

  notes.addEventListener('change', (e) => {
    if (e.target.checked) {
      get(`/flows/${config.user}/${day}/notes`, (notesData) =>
        drawNotes(notesDiv, notesData));
    } else {
      clearElement(notesDiv);
    }
  });

  commands.addEventListener('change', (e) => {
    if (e.target.checked) {
      get(`/flows/${config.user}/${day}/commands`, (commandsData) =>
        drawCommands(commandsDiv, commandsData));
    } else {
      clearElement(commandsDiv);
    }
  });


  summary.addEventListener('change', (e) => {
    if (e.target.checked) {
      get(`/flows/${config.user}/${day}/summary`, (summaryData) =>
        drawSummary(summaryDiv, summaryData));
    } else {
      clearElement(summaryDiv);
    }
  });

  document.getElementById('timelines').appendChild(container);
  // drawChart(chart, day, data);
  drawTimeline(chart2, day, data);
}

/**
   Draw several timeline charts within the `timelines` container, each for a different
   data
   For now, we assume the data is a list of days
*/
function drawCharts(flowData) {
  flowData.forEach((f) => {
    const day = formatISODate(new Date(f.groupTime));
    const data = f.subGroup.leafViews;
    createTimelineContainer(day, data);
  });
}

function fetchFlowData(selectedDate) {
  get(`/flows/${config.user}/` + selectedDate, (flowData) => {
    createTimelineContainer(selectedDate, flowData);
  });
};

function fetchAllFlowData() {
  get(`/flows/${config.user}?group=Day`, drawCharts);
};

export default function timeline() {
  const obj = {};

  obj.fetchFlowData = fetchFlowData;
  obj.fetchAllFlowData = fetchAllFlowData;

  return obj;
}
