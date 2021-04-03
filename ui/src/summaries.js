import { dom, clear } from './dom';
import {config} from './config';
import {get} from './request';
import {currentMonthPeriod} from "./date";
import { drawSummary } from './summary.js';

/// Draw the charts
export default function summaries() {
  const summaryDiv = <div id="summaries"></div>;
  const content = <div class="content">
        <div class="controls">
        <input type="radio" id="monthly" name="period" value="monthly" checked/>
        <label for="monthly">Monthly</label>
        <input type="radio" id="weekly" name="period" value="weekly"/>
        <label for="weekly">Weekly</label>
        <input type="radio" id="daily" name="period" value="daily"/>
        <label for="daily">Daily</label>
        </div>
    { summaryDiv }
  </div>;

  clear('main');
  document.getElementById('main').appendChild(content);

  document.getElementById('monthly').addEventListener('change', (e) => {
    const [start,end] = currentMonthPeriod();
    clear('summaries');
    if (e.target.checked) {
      get(`/api/flows/${config.user}/summary?from=${start}&to=${end}`, (summaryData) =>
        drawSummary(summaryDiv, summaryData));
    }
  });

  return content;
}
