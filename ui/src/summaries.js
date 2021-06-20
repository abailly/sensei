import { dom, clear, clearElement } from './dom';
import { config } from './config';
import { get } from './request';
import { currentMonthPeriod, currentWeekPeriod, currentYearPeriod, localNow, parseDate } from "./date";
import { drawSummary } from './summary.js';


export function summaries(router, container, from, to, period) {
  const baseDate = from ? parseDate(from) : localNow();
  const summaryDiv = <div id="summaries"></div>;
  const content = <div class="content">
    <div class="controls">
      <input type="radio" id="yearly" name="period" value="Year" />
      <label for="yearly">Yearly</label>
      <input type="radio" id="monthly" name="period" value="Month" />
      <label for="monthly">Monthly</label>
      <input type="radio" id="weekly" name="period" value="Week" />
      <label for="weekly">Weekly</label>
    </div>
    {summaryDiv}
  </div>;

  clearElement(container);
  container.appendChild(content);

  if (period) {
    ['yearly', 'monthly', 'weekly'].map((id) => {
      const elem = document.getElementById(id);
      if (elem.value == period) {
        elem.checked = true;
      } else {
        elem.checked = false;
      }
    });
  }

  document.getElementById('yearly').addEventListener('change', (e) => {
    const [start, end] = currentYearPeriod(baseDate);
    clear('summaries');
    if (e.target.checked) {
      get(`/api/flows/${config.user}/summary?from=${start}&to=${end}&period=${e.target.value}`, (summaryData, links) =>
        drawSummary(summaryDiv, router, summaryData, links));
    }
  });

  document.getElementById('monthly').addEventListener('change', (e) => {
    const [start, end] = currentMonthPeriod(baseDate);
    clear('summaries');
    if (e.target.checked) {
      get(`/api/flows/${config.user}/summary?from=${start}&to=${end}&period=Month`, (summaryData, links) =>
        drawSummary(summaryDiv, router, summaryData, links));
    }
  });

  document.getElementById('weekly').addEventListener('change', (e) => {
    const [start, end] = currentWeekPeriod(baseDate);
    clear('summaries');
    if (e.target.checked) {
      get(`/api/flows/${config.user}/summary?from=${start}&to=${end}&period=Week`, (summaryData, links) =>
        drawSummary(summaryDiv, router, summaryData, links));
    }
  });

  if (from && to && period) {
    get(`/api/flows/${config.user}/summary?from=${from}&to=${to}&period=${period}`, (summaryData, links) =>
      drawSummary(summaryDiv, router, summaryData, links));
  }

  return content;
}
