function formatISODate(date) {
  const year = date.getFullYear();
  let month = date.getMonth() + 1;
  let dt = date.getDate();

  if (dt < 10) {
    dt = '0' + dt;
  }
  if (month < 10) {
    month = '0' + month;
  }

  return year + '-' + month + '-' + dt;
}

function colorOf(flowType) {
  switch (flowType) {
    case 'Learning':
      return "#aaaa00";
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
  const container = document.createElement("div");
  container.setAttribute('id', name);
  container.setAttribute('class', 'timeline');
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

function get(url, callback) {
  const xhr = new XMLHttpRequest();
  xhr.open('GET', url);
  xhr.onload = function() {
    if (xhr.status >= 200 && xhr.status < 300) {
      try {
        const flowData = JSON.parse(xhr.responseText);
        callback(flowData);
      } catch (e) {
        // JSON.parse can throw a SyntaxError
        if (e instanceof SyntaxError) {
          alert("invalid JSON payload" + xhr.responseText);
        }
        throw e;
      }
    } else {
      alert('Request failed.  Returned status of ' + xhr.status);
    }
  };
  xhr.send();
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
  google.charts.load('current', { 'packages': ['timeline'] });
  obj.fetchFlowData = fetchFlowData;
  obj.fetchAllFlowData = fetchAllFlowData;
  obj.clearTimelines = clearTimelines;
  return obj;
}
