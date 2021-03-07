import {dom} from './dom.js';
import {ChronoUnit, LocalDateTime, LocalTime} from "@js-joda/core";
import {config} from "./config";
import showdown from "showdown";
import {formatISODateTime} from "./date";

const THIRTY_MINUTES = 30;
const HALF_HOUR_WIDTH = 100;

export function drawCssNotes(container, day, notesData) {
    let previousTime = LocalTime.parse(config.userProfile.userStartOfDay);
    const mainNode = document.getElementById("main");
    const mainParentNode = mainNode.parentNode;

    const timelineContainer = container.firstChild;
    const notesHeader = <div id={'title-notes-' + day} class="timeline-header">Notes</div>;
    const notesDiv = <div id={'notes-' + day} class="timeline-chart"/>;
    const noteList = <ul/>;


    clearNotes();
    drawNotes();
    notesDiv.appendChild(noteList);
    timelineContainer.insertBefore(notesHeader, timelineContainer.children[1]);
    timelineContainer.insertBefore(notesDiv, timelineContainer.children[2]);

    function clearNotes() {
        if(document.getElementById('title-notes-' + day)) {
            document.getElementById('title-notes-' + day).remove();
        }
        notesData.map(note => document.getElementById('note-' + note.noteStart))
            .filter(node => node !== null)
            .forEach(node => node.remove());
        notesData.map(note => document.getElementById('notes-' + day))
            .filter(node => node !== null)
            .forEach(node => node.remove());
    }

    function drawNotes() {
        notesData.forEach(note => {
            noteList.appendChild(drawNote(note));
        });
    }

    function drawNote(note) {
        const noteDisplay = <li style={'width:16px; margin-left:' + noteLeftMargin(note) + 'px;'}/>;
        const dialog = <div id={'note-' + note.noteStart} class="c-dialog"/>;
        const dialogBox = <div role="document" class="c-dialog__box"/>;
        const closeButton = <div>X</div>;
        const dialogContent = <div id={'note-desc-' + note.noteStart}/>;
        const button = <div type="button" id={'note-button-' + note.noteStart}><i /></div>;

        noteDisplay.appendChild(button);
        dialog.appendChild(dialogBox);
        dialogBox.appendChild(closeButton);
        dialogBox.appendChild(dialogContent);
        dialogContent.insertAdjacentHTML("beforeend", formatNote(note));
        mainParentNode.insertBefore(dialog, mainNode.nextSibling);

        const setAttributesTo = (node, values) => {
            values.forEach(value => node.setAttribute(value.type, value.value));
        }

        setAttributesTo(dialog, [
            {type: "role", value: dialog},
            {type: "aria-describedby", value: 'note-desc-' + note.noteStart},
            {type: "aria-modal", value: 'true'},
            {type: "aria-hidden", value: 'true'},
            {type: "tabindex", value: '-1'}
        ]);

        setAttributesTo(closeButton, [
            {type: "type", value: "button"},
            {type: "aria-label", value: "Close"},
            {type: "aria-dismiss", value: "dialog"},
        ]);

        setAttributesTo(button, [
            {type: "type", value: "button"},
            {type: "aria-haspopup", value: "dialog"},
            {type: "aria-controls", value: 'note-' + note.noteStart},
            {type: "class", value: "timeline-event timeline-note"}
        ]);

        setAttributesTo(button.firstChild, [{type: "class", value: "fas fa-sticky-note"}]);

        initializeModal(dialog, button, closeButton);

        return noteDisplay;
    }

    function formatNote(note) {
        return "<div class='note'>" +
            new showdown.Converter({simplifiedAutoLink: true}).makeHtml('#### ' + formatISODateTime(LocalDateTime.parse(note.noteStart)) + '\n\n' + note.noteContent) +
            "</div>";
    }

    function noteLeftMargin(note) {
        let bulletGap = 8;
        if (note !== notesData[0]) {
            previousTime = LocalDateTime.parse(notesData[notesData.indexOf(note) - 1].noteStart);
            bulletGap = 16;
        }
        return (previousTime.until(LocalDateTime.parse(note.noteStart), ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH - bulletGap;
    }

    function initializeModal(dialog, button, closeButton) {
        const open = function () {
            dialog.setAttribute('aria-hidden', false);
            mainNode.setAttribute('aria-hidden', true);
        };

        const close = function () {
            dialog.setAttribute('aria-hidden', true);
            mainNode.setAttribute('aria-hidden', false);
        };

        button.addEventListener('click', (event) => {
            event.preventDefault();
            open();
        });

        closeButton.addEventListener('click', (event) => {
            close();
        });
    }
}