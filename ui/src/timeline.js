import {get} from './request.js';
import {formatISODate} from './date.js';
import {drawCommands} from './commands.js';
import {drawSummary} from './summary.js';
import {dom, clearElement} from './dom.js';
import {config} from "./config";
import {LocalDateTime} from "@js-joda/core";
import Timeline from "./timeline/Timeline";

/**
 Create a new div container for a timeline
 */
function createTimelineContainer(day, data, notesData) {
    const detailsName = 'checkbox-' + day;
    const notesName = 'notes-checkbox-' + day;
    const commandsName = 'cmd-checkbox-' + day;
    const summaryName = 'summary-checkbox-' + day;
    const chart = <div class="timeline-chart"/>;
    const commandsDiv = <div id={'commands-' + day} class="timeline-chart"/>;
    const summaryDiv = <div id={'summary-' + day} class="summary"/>;
    const details = <input type="checkbox" id={detailsName}/>;
    const notes = <input type="checkbox" id={notesName}/>;
    const commands = <input type="checkbox" id={commandsName}/>;
    const summary = <input type="checkbox" id={summaryName}/>;

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
            </div>
            {commandsDiv}
            {summaryDiv}
            {chart}
        </div>;

    details.addEventListener('change', (e) => {
        if (e.target.checked) {
            timeline.draw(f => f.flowType);
        } else {
            timeline.draw();
        }
    });

    notes.addEventListener('change', (e) => {
        if (e.target.checked) {
            get(`/api/flows/${config.user}/${day}/notes`, (notesData) => {
                timeline.drawNotes(notesData);
            });
        } else {
            timeline.clearNotes();
        }
    });

    commands.addEventListener('change', (e) => {
        if (e.target.checked) {
            get(`/api/flows/${config.user}/${day}/commands`, (commandsData) =>
                drawCommands(commandsDiv, commandsData));
        } else {
            clearElement(commandsDiv);
        }
    });


    summary.addEventListener('change', (e) => {
        if (e.target.checked) {
            get(`/api/flows/${config.user}/${day}/summary`, (summaryData) =>
                drawSummary(summaryDiv, summaryData));
        } else {
            clearElement(summaryDiv);
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
function drawCharts(flowData) {
    flowData.forEach((f) => {
        const day = formatISODate(LocalDateTime.parse(f.groupTime));
        const data = f.subGroup.leafViews;
        createTimelineContainer(day, data);
    });
}

function fetchFlowData(selectedDate) {
    get(`/api/flows/${config.user}/` + selectedDate, (flowData) => {
        createTimelineContainer(selectedDate, flowData);
    });
};

function fetchAllFlowData() {
    get(`/api/flows/${config.user}?group=Day`, drawCharts);
};

export default function timeline() {
    const obj = {};

    obj.fetchFlowData = fetchFlowData;
    obj.fetchAllFlowData = fetchAllFlowData;

    return obj;
}
