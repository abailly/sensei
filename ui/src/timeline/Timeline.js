import {dom} from '../dom.js';
import {ChronoUnit, DateTimeFormatter, LocalDateTime, LocalTime} from "@js-joda/core";
import {config} from "../config";
import {colorOf} from "../color";
import showdown from "showdown";
import {formatISODateTime} from "../date";


const THIRTY_MINUTES = 30;
const HALF_HOUR_WIDTH = 100;

export default function Timeline(container, day, flowData) {
    this.startTimeOfDay = LocalTime.parse(config.userProfile.userStartOfDay);
    this.endTimeOfDay = LocalTime.parse(config.userProfile.userEndOfDay);
    this.container = container;
    this.timelineContainer = <div id={'timeline-container-' + day}/>;
    this.day = day;
    this.header = new TimelineHeader(this.startTimeOfDay, this.endTimeOfDay, flowData);
    this.footer = new TimelineFooter(this.startTimeOfDay, this.endTimeOfDay);
    this.flowsByLabel = new TimelineFlows(this.startTimeOfDay, this.endTimeOfDay, flowData);
    this.noteFlows = new TimelineNotes(this.startTimeOfDay, this.day);

    this.draw = function (rowLabel = (_ => day)) {
        this.header.draw(this.timelineContainer, this.day);
        this.footer.draw(this.timelineContainer, this.day);
        this.noteFlows.draw(this.timelineContainer);
        this.flowsByLabel.draw(this.timelineContainer, rowLabel);
        this.container.appendChild(this.timelineContainer);
    }

    this.drawNotes = function (notes) {
        this.noteFlows.drawNotes(this.timelineContainer, notes);
    }

    this.clearNotes = function() {
        this.noteFlows.clear();
        this.noteFlows = new TimelineNotes(this.startTimeOfDay, this.day);
    };

}

function TimelineHeader(startTimeOfDay, endTimeOfDay, flowData) {
    this.headerFlows = [];
    this.hasBeenDrawn = false;
    this.startTimeOfDay = startTimeOfDay;
    this.endTimeOfDay = endTimeOfDay;
    this.addFlow = function (flow) {
        this.headerFlows.push(new HeaderFlow(LocalDateTime.parse(flow.flowStart), LocalDateTime.parse(flow.flowEnd), flow.flowType, this.startTimeOfDay, this.endTimeOfDay));
    }

    flowData.forEach(flow => {
        this.addFlow(flow);
    });

    this.draw = function (container, day) {
        if (!this.hasBeenDrawn) {
            const timelineHeaderContainer = <div id={'timeline-header-' + day} class='timeline-header'/>;
            const items = <ul/>;
            this.headerFlows.forEach((flow, index) => {
                let flowStartDate = flow.startDate;
                let flowEndDate = flow.endDate;
                const flowWidth = Math.abs((flowStartDate.until(flowEndDate, ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH);
                const item = <li
                    style={'width:' + flowWidth + 'px; margin-left:' + flowLeftMargin(flow, index) + 'px;'}>
                    <div>
                        <h2>{flowStartDate.format(DateTimeFormatter.ofPattern("HH:mm"))}</h2>
                        <h2>{flowEndDate.format(DateTimeFormatter.ofPattern("HH:mm"))}</h2>
                        <h3>{flow.flowType}</h3>
                    </div>
                </li>;
                items.appendChild(item);
            });
            timelineHeaderContainer.appendChild(items);
            container.appendChild(timelineHeaderContainer);
            this.hasBeenDrawn = true;
        }

        function flowLeftMargin(flow, index) {
            if (index === 0) {
                return (flow.startTimeOfDay.until(flow.startDate, ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH;
            }
            return 0;
        }
    }

}

function HeaderFlow(startDate, endDate, flowType, startTimeOfDay, endTimeOfDay) {

    this.startDate = startDate;
    this.endDate = endDate;
    this.flowType = flowType;
    this.startTimeOfDay = startTimeOfDay;
    this.endTimeOfDay = endTimeOfDay;

}

function TimelineFlows(startTimeOfDay, endTimeOfDay, flowData) {
    this.startTimeOfDay = startTimeOfDay;
    this.endTimeOfDay = endTimeOfDay;
    this.flowMap = new Map();
    this.hasBeenDrawn = false;
    this.containerChilds = [];


    this.initializeFlows = function (rowLabel) {
        this.flowMap = new Map();
        flowData.forEach(flow => {
            if (this.flowMap.has(rowLabel(flow))) {
                const flowRow = this.flowMap.get(rowLabel(flow));
                const timelineFlow = new TimelineFlow(this.startTimeOfDay, this.endTimeOfDay, flow, LocalDateTime.parse(flowRow[flowRow.length - 1].flow.flowEnd));
                flowRow.push(timelineFlow);
            } else {
                const timelineFlow = new TimelineFlow(this.startTimeOfDay, this.endTimeOfDay, flow, this.startTimeOfDay);
                this.flowMap.set(rowLabel(flow), new Array(timelineFlow));
            }
        });
    }

    this.draw = function (container, rowLabel) {
        this.initializeFlows(rowLabel);
        this.containerChilds.forEach(child => child.remove());
        this.flowMap.forEach((flows, rowLabel) => {
            const flowTitle = <div class='timeline-title'/>;
            flowTitle.appendChild(<div>
                <h3>{rowLabel}</h3>
            </div>);
            const flowsContainer = <div class='timeline-events'/>;
            const flowEvents = <ul/>;
            flows.forEach((flow, index) => {
                flow.draw(flowEvents, index);
                flowsContainer.appendChild(flowEvents);
            });
            this.containerChilds.push(flowTitle);
            this.containerChilds.push(flowsContainer);
            const penultimateChild = container.children[container.children.length - 1];
            container.insertBefore(flowTitle, penultimateChild);
            container.insertBefore(flowsContainer, penultimateChild);
        });
        this.hasBeenDrawn = true;
    }
}

function TimelineFlow(startTimeOfday, endTimeOfDay, flow, previousFlowEnd) {
    this.startTimeOfDay = startTimeOfday;
    this.endTimeOfDay = endTimeOfDay;
    this.flow = flow;
    this.previousFlowEnd = previousFlowEnd;

    this.flowLeftMargin = function (index) {
        return (this.previousFlowEnd.until(LocalDateTime.parse(this.flow.flowStart), ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH;
    }


    this.draw = function (container, index) {
        let flowStartDate = LocalDateTime.parse(this.flow.flowStart);
        let flowEndDate = LocalDateTime.parse(this.flow.flowEnd);
        const flowWidth = Math.abs((flowStartDate.until(flowEndDate, ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH);
        container.appendChild(<li
            style={'width:' + flowWidth + 'px; margin-left:' + this.flowLeftMargin(index) + 'px;'}>
            <div class='timeline-event' style={'background: ' + colorOf(this.flow.flowType) + ';'}></div>
        </li>);
    }
}

function TimelineFooter(startTimeOfDay, endTimeOfDay) {

    this.startTimeOfDay = startTimeOfDay;
    this.endTimeOfDay = endTimeOfDay;
    this.hasBeenDrawn = false;

    this.draw = function (container, day) {
        if (!this.hasBeenDrawn) {
            const timelineFooterContainer = <div id={'timeline-time-' + day} class='timeline-time'/>;
            const footerItems = <ul/>;
            const nbHalfHours = (this.endTimeOfDay.hour() - this.startTimeOfDay.hour()) * 2
            for (let i = 0; i < nbHalfHours; i++) {
                const time = this.startTimeOfDay.plusMinutes(i * THIRTY_MINUTES);
                footerItems.appendChild(<li>{time.format(DateTimeFormatter.ofPattern('HH:mm'))}</li>);
            }
            timelineFooterContainer.appendChild(footerItems);
            container.appendChild(timelineFooterContainer);
            this.hasBeenDrawn = true;
        }
    }
}

function TimelineNotes(startTimeOfDay, day) {

    this.startTimeOfDay = startTimeOfDay;
    this.notes = [];
    this.hasBeenDrawn = false;
    this.notesTitle= <div id={'title-notes-' + day} class="timeline-title">Notes</div>;
    this.notesDiv = <div id={'notes-' + day} class="timeline-chart"/>;

    this.drawNotes = function (container, notes) {
        this.notes = notes.map((note, index, notes) => new TimelineNote(note, this.startTimeOfDay, index, notes));
        this.draw(container);
    }

    this.draw = function (container) {
        if(!this.hasBeenDrawn && this.notes.length) {
            const noteList = <ul/>;

            this.notes.forEach(note => {
                note.draw(noteList);
            });
            this.notesDiv.appendChild(noteList);
            container.insertBefore(this.notesTitle, container.children[1]);
            container.insertBefore(this.notesDiv, container.children[2]);
            this.hasBeenDrawn = true;
        }
    }

    this.clear = function() {
        this.notes.forEach(note => note.clear());
        while(this.notesDiv.firstChild) {
            this.notesDiv.firstChild.remove();
        }
        this.notesDiv.remove();
        this.notesTitle.remove();
        this.hasBeenDrawn = false;
    }
}

function TimelineNote(note, startTimeOfDay, index, notes) {
    this.note = note;
    this.previousNoteTime = startTimeOfDay;
    this.previousNote = index > 0 ? notes[index - 1] : undefined;
    this.modalNotes = [];

    this.noteLeftMargin = function() {
        let bulletGap = 8;
        if (this.previousNote !== undefined) {
            this.previousNoteTime = LocalDateTime.parse(this.previousNote.noteStart);
            bulletGap = 16;
        }
        return (this.previousNoteTime.until(LocalDateTime.parse(this.note.noteStart), ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH - bulletGap;
    }

    this.noteDisplay = <li style={'width:16px; margin-left:' + this.noteLeftMargin() + 'px;'}/>;
    this.button = <div type="button" id={'note-button-' + this.note.noteStart}><i /></div>;

    const setAttributesTo = (node, values) => {
        values.forEach(value => node.setAttribute(value.type, value.value));
    }

    this.draw = function(container) {
        setAttributesTo(this.button, [
            {type: "type", value: "button"},
            {type: "aria-haspopup", value: "dialog"},
            {type: "aria-controls", value: 'note-' + this.note.noteStart},
            {type: "class", value: "timeline-event timeline-note"}
        ]);

        setAttributesTo(this.button.firstChild, [{type: "class", value: "fas fa-sticky-note"}]);

        const modalNote = new ModalNote(this.note);
        this.modalNotes.push(modalNote);
        this.button.addEventListener('click', (event) => {
            event.preventDefault();
            modalNote.open();
        });
        modalNote.draw();

        this.noteDisplay.appendChild(this.button);
        container.appendChild(this.noteDisplay);
    }

    this.clear = function() {
        this.modalNotes.forEach(modal => modal.clear());
        this.noteDisplay.remove();
        this.button.remove();
    }
}

function ModalNote(note) {
    this.mainNode = document.body.children[0];
    this.mainParentNode = document.body;
    this.note = note;
    this.dialog = <div id={'note-' + this.note.noteStart} class="c-dialog"/>;

    const setAttributesTo = (node, values) => {
        values.forEach(value => node.setAttribute(value.type, value.value));
    }


    this.formatNote = function() {
        return "<div class='note'>" +
            new showdown.Converter({simplifiedAutoLink: true}).makeHtml('#### ' + formatISODateTime(LocalDateTime.parse(this.note.noteStart)) + '\n\n' + this.note.noteContent) +
            "</div>";
    }

    this.open = function() {
        this.dialog.setAttribute('aria-hidden', false);
        this.mainNode.setAttribute('aria-hidden', true);
    }

    this.close = function() {
        this.dialog.setAttribute('aria-hidden', true);
        this.mainNode.setAttribute('aria-hidden', false);
    }

    this.draw = function() {
        const dialogBox = <div role="document" class="c-dialog__box"/>;
        const closeButton = <div>X</div>;
        const dialogContent = <div id={'note-desc-' + this.note.noteStart}/>;

        setAttributesTo(this.dialog, [
            {type: "role", value: "dialog"},
            {type: "aria-describedby", value: 'note-desc-' + this.note.noteStart},
            {type: "aria-modal", value: 'true'},
            {type: "aria-hidden", value: 'true'},
            {type: "tabindex", value: '-1'}
        ]);

        setAttributesTo(closeButton, [
            {type: "type", value: "button"},
            {type: "aria-label", value: "Close"},
            {type: "aria-dismiss", value: "dialog"},
        ]);

        closeButton.addEventListener('click', (event) => {
            event.preventDefault();
            this.close();
        });

        this.dialog.appendChild(dialogBox);
        dialogBox.appendChild(closeButton);
        dialogBox.appendChild(dialogContent);
        dialogContent.insertAdjacentHTML("beforeend", this.formatNote());
        this.mainParentNode.insertBefore(this.dialog, this.mainNode.nextSibling);
    }

    this.clear = function() {
        this.dialog.remove();
    }
}
