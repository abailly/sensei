import { get } from './request.js';
import { formatISODate, nextDay } from './date.js';
import { drawCommands } from './commands.js';
import { drawSummary } from './summary.js';
import { drawProjectsSelector } from './project.js';
import { dom, clearElement } from './dom.js';
import { config } from "./config";
import { LocalDateTime, LocalDate } from "@js-joda/core";
import Timeline from "./timeline/Timeline";

/**
 Create a new div container for a timeline
 */
function createTimelineContainer(router, day, data, notesData) {
  const detailsName = 'checkbox-' + day;
  const notesName = 'notes-checkbox-' + day;
  const commandsName = 'cmd-checkbox-' + day;
  const summaryName = 'summary-checkbox-' + day;
  const projectsName = 'projects-selector-' + day;
  const chart = <div class="timeline-chart" />;
  const commandsDiv = <div id={'commands-' + day} class="timeline-chart" />;
  const summaryDiv = <div id={'summary-' + day} class="summary" />;
  const details = <input type="checkbox" id={detailsName} />;
  const notes = <input type="checkbox" id={notesName} />;
  const commands = <input type="checkbox" id={commandsName} />;
  const summary = <input type="checkbox" id={summaryName} />;
  const projects = drawProjectsSelector(projectsName, data);

  let timeline = undefined;

  const container =
    <div id={day} class='timeline'>
      <div class='timeline-controls'>
        <label for={detailsName}>Expand</label>
        {details}
        <label for={notesName}>Notes</label>
        {notes}
        <label for={commandsName}>Commands</label>
        {commands}
        <label for={summaryName}>Summary</label>
        {summary}
        <label for={projectsName}>Projects</label>
        {projects}
      </div>
      {commandsDiv}
      {summaryDiv}
      {chart}
    </div>;

  details.addEventListener('change', (e) => {
    if (e.target.checked) {
      timeline.draw(f => f.viewType);
    } else {
      timeline.draw();
    }
  });

  notes.addEventListener('change', (e) => {
    if (e.target.checked) {
      get(router, `/api/flows/${config.userProfile.userName}/${day}/notes`, (notesData) => {
        timeline.drawNotes(notesData);
      });
    } else {
      timeline.clearNotes();
    }
  });

  commands.addEventListener('change', (e) => {
    if (e.target.checked) {
      get(router, `/api/flows/${config.userProfile.userName}/${day}/commands`, (commandsData) =>
        drawCommands(commandsDiv, commandsData));
    } else {
      clearElement(commandsDiv);
    }
  });


  summary.addEventListener('change', (e) => {
    if (e.target.checked) {
      get(router, `/api/flows/${config.userProfile.userName}/summary?from=${day}&to=${nextDay(LocalDate.parse(day))}`, (summaryData) =>
        drawSummary(summaryDiv, router, summaryData));
    } else {
      clearElement(summaryDiv);
    }
  });


  projects.addEventListener('change', (e) => {
    if (e.target.value == "all") {
      timeline.draw();
    } else {
      timeline.draw(f => f.flowProject);
    }
  });


  document.getElementById('timelines').appendChild(container);
  timeline = new Timeline(chart, day, data);
  timeline.draw();
}

/**
 Draw several timeline charts within the `timelines` container, each for a different
 data
 For now, we assume the data is a list of days
 */
function drawCharts(router) {
  return function(flowData) {
    flowData.forEach((f) => {
      const day = formatISODate(LocalDateTime.parse(f.groupTime));
      const data = f.subGroup.map(l => l.contents);
      createTimelineContainer(router, day, data);
    });
  };
}

function fetchFlowData(router) {
  return function(selectedDate) {
    get(router, `/api/flows/${config.userProfile.userName}/` + selectedDate, (flowData) => {
      createTimelineContainer(router, selectedDate, flowData);
    });
  };
};

function fetchAllFlowData(router) {
  return function() {
    get(router, `/api/flows/${config.userProfile.userName}?group=Day`, drawCharts(router));
  };
};

export default function timeline(router) {
  const obj = {};

  obj.fetchFlowData = fetchFlowData(router);
  obj.fetchAllFlowData = fetchAllFlowData(router);

  return obj;
}
