import { dom, clear } from './dom';
import timeline from './timeline.js';
import summary from './summary.js';

/// Draw the charts
export default function charts() {
  const content = <div class="content">
    <div class="controls">
      <label for="flowDate">Select a date</label>
      <input type="date" id="flowDate" name="flowDate" />
      <label for="selectAll">All</label>
      <input type="checkbox" id="selectAll" />
      <label for="summary">Summary</label>
      <input type="checkbox" id="summary" />
    </div>
    <div id="summaries"></div>
    <div id="timelines"></div>
  </div>;

  clear('main');
  document.getElementById('main').appendChild(content);

  const tl = timeline();
  const sum = summary();

  document.getElementById('flowDate').addEventListener('change', (e) => {
    clear('timelines');
    const selectedDate = e.target.value;
    tl.fetchFlowData(selectedDate);
  });

  document.getElementById('selectAll').addEventListener('change', (e) => {
    clear('timelines');
    if (e.target.checked) {
      document.getElementById('flowDate').disabled = true;
      tl.fetchAllFlowData();
    } else {
      document.getElementById('flowDate').disabled = false;
    }
  });

  document.getElementById('summary').addEventListener('change', (e) => {
    clear('summaries');
    if (e.target.checked) {
      sum.fetchAllSummaryData();
    }
  });
  return content;
}
