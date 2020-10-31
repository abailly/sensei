import { get } from './request.js';
import { formatISODate } from './date.js';
import dom from './dom.js';


function colorOf(flowType) {
  switch (flowType) {
    case 'Learning':
      return "#ff8822";
    case 'Experimenting':
      return "#0022dd";
    case 'Troubleshooting':
      return "#ee1111";
    case 'Flowing':
      return "#00dd22";
    case 'Rework':
      return "#4500dd";
    case 'Note':
      return "#000000";
    default:
      return "#ffffff";
  }
}

/**
   Draw a timeline chart in given element with given data
*/
function drawChart(container, selectedDate, flowData) {
  const chart = new google.visualization.Timeline(container);
  const dataTable = new google.visualization.DataTable();

  dataTable.addColumn({ type: 'string', id: 'Role' });
  dataTable.addColumn({ type: 'string', id: 'Flow Type' });
  dataTable.addColumn({ type: 'string', id: 'style', role: 'style' });
  dataTable.addColumn({ type: 'date', id: 'Start' });
  dataTable.addColumn({ type: 'date', id: 'End' });
  flowData.forEach(flow => dataTable.addRow([selectedDate, flow.flowType, colorOf(flow.flowType), new Date(flow.flowStart), new Date(flow.flowEnd)]));
  var options = {
  };
  chart.draw(dataTable, options);
}

/**
   Create a new div container for a timeline
*/
function createTimelineContainer(name) {
  const container = <div id={name} class='timeline' />;
  document.getElementById('timelines').appendChild(container);
  return container;
}

function clearTimelines() {
  const timelines = document.getElementById('timelines');
  while (timelines.firstChild) {
    timelines.removeChild(timelines.firstChild);
  }
}

/**
   Draw several timeline charts within the `timelines` container, each for a different
   data
   For now, we assume the data is a list of days
*/
function drawCharts(flowData) {
  flowData.forEach((f) => {
    const day = formatISODate(new Date(f.groupTime));
    const container = createTimelineContainer('timeline_' + day);
    drawChart(container, day, f.subGroup.leafViews);
  });
}

function fetchFlowData(selectedDate) {
  const xhr = new XMLHttpRequest();
  get('/flows/arnaud/' + selectedDate, (flowData) => {
    const container = createTimelineContainer('timeline_' + selectedDate);
    // assumes flowData is a single leaf group
    drawChart(container, selectedDate, flowData);
  });
};

function fetchAllFlowData() {
  get('/flows/arnaud?group=Day', drawCharts);
};


export default function timeline() {
  const obj = {};
  // assumes google object exists
  obj.fetchFlowData = fetchFlowData;
  obj.fetchAllFlowData = fetchAllFlowData;
  obj.clearTimelines = clearTimelines;
  return obj;
}
