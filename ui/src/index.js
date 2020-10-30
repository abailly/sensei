import timeline from './timeline.js';
import summary from './summary.js';
import style from './style.css';


document.addEventListener('DOMContentLoaded', () => {
  google.charts.load('current', { 'packages': ['timeline', 'calendar'] });
  const tl = timeline();
  const sum = summary();

  document.getElementById('flowDate').addEventListener('change', (e) => {
    tl.clearTimelines();
    const selectedDate = e.target.value;
    tl.fetchFlowData(selectedDate);
  });

  document.getElementById('selectAll').addEventListener('change', (e) => {
    tl.clearTimelines();
    if (e.target.checked) {
      document.getElementById('flowDate').disabled = true;
      tl.fetchAllFlowData();
    } else {
      document.getElementById('flowDate').disabled = false;
    }
  });

  document.getElementById('summary').addEventListener('change', (e) => {
    sum.clearSummaries();
    if (e.target.checked) {
      sum.fetchAllSummaryData();
    }
  });
});
