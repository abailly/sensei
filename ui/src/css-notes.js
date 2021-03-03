import {dom} from './dom.js';
import {ChronoUnit, LocalDateTime, LocalTime} from "@js-joda/core";
import {config} from "./config";
import showdown from "showdown";

const THIRTY_MINUTES = 30;
const HALF_HOUR_WIDTH = 100;

export function drawCssNotes(container, day, notesData) {
    let previousTime = LocalTime.parse(config.userProfile.userStartOfDay);

    const timelineContainer = container.firstChild;
    const notesDiv = <div id={'notes-' + day} class="timeline-chart" />;
    const noteList = <ul />;

    function formatNote(note) {
        return "<div class='note'>" +
            new showdown.Converter({ simplifiedAutoLink: true }).makeHtml('#### ' + new Date(note.noteStart).toLocaleTimeString() + '\n\n' + note.noteContent) +
            "</div>";
    }

    function noteLeftMargin(note) {
        let bulletGap = 8;
        if(note !== notesData[0]) {
            previousTime = LocalDateTime.parse(notesData[notesData.indexOf(note) - 1].noteStart);
            bulletGap = 16;
        }
        return (previousTime.until(LocalDateTime.parse(note.noteStart), ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH - bulletGap;
    }

    function drawTimelineFlow(note) {
        // let flowStartDate = LocalDateTime.parse(note.noteStart);
        return <li style={'width:16px; margin-left:' + noteLeftMargin(note) + 'px;'}>
            <div class='timeline-event' style={'background: blue;'}></div>
        </li>;

    }

    notesData.forEach(note => {
        noteList.appendChild(drawTimelineFlow(note));
    });

    notesDiv.appendChild(noteList);
    timelineContainer.insertBefore(notesDiv, timelineContainer.children[1]);

}