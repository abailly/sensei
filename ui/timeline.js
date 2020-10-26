
/**
   Draw a timeline chart in given element with given data
*/
function drawChart(target, flowData) {
  const container = document.getElementById(target);
  const chart = new google.visualization.Timeline(container);
  const dataTable = new google.visualization.DataTable();

  dataTable.addColumn({ type: 'string', id: 'Role' });
  dataTable.addColumn({ type: 'string', id: 'Flow Type' });
  dataTable.addColumn({ type: 'date', id: 'Start' });
  dataTable.addColumn({ type: 'date', id: 'End' });
  flowData.forEach(flow => dataTable.addRow(['1', flow.flowType, new Date(flow.flowStart), new Date(flow.flowEnd)]));
  var options = {
    timeline: { showRowLabels: false }
  };
  chart.draw(dataTable, options);
}

function fetchFlowData(selectedDate) {
  const xhr = new XMLHttpRequest();
  xhr.open('GET', '/flows/arnaud/' + selectedDate);
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

document.addEventListener('DOMContentLoaded', () => {
  google.charts.load('current', { 'packages': ['timeline'] });
  document.getElementById('flowDate').addEventListener('change', (e) => {
    const selectedDate = e.target.value;
    fetchFlowData(selectedDate);
  });

  document.getElementById('selectAll').addEventListener('change', (e) => {
    if (e.target.checked) {
      document.getElementById('flowDate').disabled = true;
    } else {
      document.getElementById('flowDate').disabled = false;
    }
  });
});
