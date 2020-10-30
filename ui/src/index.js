import timeline from './timeline.js';
import style from './style.css';


document.addEventListener('DOMContentLoaded', () => {
  const tl = timeline();

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
});