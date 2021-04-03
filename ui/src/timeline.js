import {get} from './request.js';
import {formatISODate} from './date.js';
import {drawNotes} from './notes.js';
import {drawCommands} from './commands.js';
import {drawSummary} from './summary.js';
import {dom, clearElement} from './dom.js';
import {config} from "./config";
import {colorOf} from './color.js';
import {drawTimeline} from "./css-timeline.js";
import {drawCssNotes} from "./css-notes.js";
import {LocalDate, LocalDateTime} from "@js-joda/core";

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
            drawTimeline(chart, day, data, f => f.viewType);
        } else {
            drawTimeline(chart, day, data);
        }
    });

    notes.addEventListener('change', (e) => {
        if (e.target.checked) {
            get(`/api/flows/${config.user}/${day}/notes`, (notesData) => {
                drawCssNotes(chart, day, notesData);
            });
        } else {
            document.getElementById("notes-" + day).remove();
            document.getElementById('title-notes-' + day).remove();
            const regExp = new RegExp('^note-' + day + '.*$');
            Array.from(document.body.childNodes).forEach(node => {
                if (node.id !== undefined && regExp.test(node.id)) {
                    node.remove();
                }
            });
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
            get(`/api/flows/${config.user}/summary?from=${day}&to=${nextDay(day)}`, (summaryData) =>
                drawSummary(summaryDiv, summaryData));
        } else {
            clearElement(summaryDiv);
        }
    });

    document.getElementById('timelines').appendChild(container);
    drawTimeline(chart, day, data);
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
