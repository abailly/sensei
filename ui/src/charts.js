import { dom, clear } from './dom';
import timeline from './timeline.js';
import { nextDay, previousDay } from './date.js';
import { pagination } from './page';
import { LocalDate } from "@js-joda/core";

/// Draw the charts
export default function charts(router, someDate) {
  const next = nextDay(LocalDate.parse(someDate));
  const prev = previousDay(LocalDate.parse(someDate));
  const links = {
    prev: { page: prev },
    next: { page: next }
  };
  const content = <div class="content">
    <div class="controls">
      <label for="flowDate">Select a date</label>
      <input type="date" id="flowDate" name="flowDate" value={someDate} />
      <label for="selectAll">All</label>
      <input type="checkbox" id="selectAll" />
      {pagination('flows', router, links)}
    </div>
    <div id="timelines"></div>
  </div>;

  clear('main');
  document.getElementById('main').appendChild(content);

  const tl = timeline(router);

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

  if (someDate) {
    tl.fetchFlowData(someDate);
  }

  return content;
}
