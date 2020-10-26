
/**
   Draw a timeline chart in given element with given data
*/
function drawChart(target, flowData) {
  const container = document.getElementById(target);
  const chart = new google.visualization.Timeline(container);
  const dataTable = new google.visualization.DataTable();

  dataTable.addColumn({ type: 'string', id: 'Flow Type' });
  dataTable.addColumn({ type: 'date', id: 'Start' });
  dataTable.addColumn({ type: 'date', id: 'End' });
  flowData.forEach(flow => dataTable.addRow([flow.flowType, new Date(flow.flowStart), new Date(flow.flowEnd)]));
  chart.draw(dataTable);
}

function fetchFlowData() {
  const xhr = new XMLHttpRequest();
  xhr.open('GET', '/flows/arnaud/2020-10-26');
  xhr.onload = function() {
    if (xhr.status >= 200 && xhr.status < 300) {
      try {
        const flowData = JSON.parse(xhr.responseText);
        drawChart('timeline', flowData);
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
};

function draw() {
  google.charts.load('current', { 'packages': ['timeline'] });
  google.charts.setOnLoadCallback(fetchFlowData);
}

document.addEventListener('DOMContentLoaded', draw);
