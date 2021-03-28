import {dom} from './dom.js';
import {colorOf} from './color.js';
import {config} from "./config";
import {ChronoUnit, DateTimeFormatter, LocalDateTime, LocalTime} from "@js-joda/core";

const THIRTY_MINUTES = 30;
const HALF_HOUR_WIDTH = 100;

export function drawTimeline(container, day, flowData, rowLabel = (_ => day)) {
    const startTime = LocalTime.parse(config.userProfile.userStartOfDay);
    const endTime = LocalTime.parse(config.userProfile.userEndOfDay);
    const flowMap = new Map();

    const timelineContainer = !container.firstChild ? <div id={'timeline-container-' + day}/> : container.firstChild;
    const timelineFlowsContainer = !container.firstChild ?
        <div id={'timeline-flows-' + day} class='timeline-flows'/> : container.firstChild.firstChild;
    const timelineFlows = !container.firstChild ? <ul/> : container.firstChild.firstChild.firstChild;
    const timelineTimeContainer = !container.firstChild ?
        <div id={'timeline-time-' + day} class='timeline-time'/> : container.firstChild.lastChild;
    const timelineTime = !container.firstChild ? <ul/> : container.firstChild.lastChild.lastChild;

    function flowLeftMargin(flow) {
        const viewTypeRow = flowMap.get(flow.viewType);

        function isFirstFlow() {
            return viewTypeRow.indexOf(flow) === 0;
        }

        function isAfterFirstFlow() {
            return viewTypeRow.indexOf(flow) > 0;
        }

        if (viewTypeRow !== undefined) {
            if (isFirstFlow()) {
                return (startTime.until(LocalDateTime.parse(flow.flowStart), ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH;
            }
            if (isAfterFirstFlow()) {
                const previousFlowEnd = LocalDateTime.parse(viewTypeRow[viewTypeRow.indexOf(flow) - 1].flowEnd);
                return (previousFlowEnd.until(LocalDateTime.parse(flow.flowStart), ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH;
            }
        }
        if (flowData[0] !== flow) {
            return 0;
        }
        const firstFlowStartTime = LocalDateTime.parse(flowData[0].flowStart);
        return firstFlowStartTime.toLocalTime().compareTo(startTime) !== 0 ? (startTime.until(firstFlowStartTime.toLocalTime(), ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH : 0;
    }

    function drawTimelineFlow(flow) {
        let flowStartDate = LocalDateTime.parse(flow.flowStart);
        let flowEndDate = LocalDateTime.parse(flow.flowEnd);
        const flowWidth = Math.abs((flowStartDate.until(flowEndDate, ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH);
        return <li style={'width:' + flowWidth + 'px; margin-left:' + flowLeftMargin(flow) + 'px;'}>
            <div class='timeline-event' style={'background: ' + colorOf(flow.viewType) + ';'}></div>
        </li>;

    }

    function drawTimelineHour(time) {
        return <li>{time.format(DateTimeFormatter.ofPattern('HH:mm'))}</li>;

    }

    function drawTimelineHours() {
        if (!timelineTime.hasChildNodes()) {
            const nbHalfHours = (endTime.hour() - startTime.hour()) * 2
            for (let i = 0; i < nbHalfHours; i++) {
                const time = startTime.plusMinutes(i * THIRTY_MINUTES);
                timelineTime.appendChild(drawTimelineHour(time));
            }
            timelineTimeContainer.appendChild(timelineTime);
        }
    }

    function drawTimelineTimeFlows(flow) {
        if(!timelineFlows.childNodes || timelineFlows.childNodes.length !== flowData.length) {
            let flowStartDate = LocalDateTime.parse(flow.flowStart);
            let flowEndDate = LocalDateTime.parse(flow.flowEnd);
            const flowWidth = Math.abs((flowStartDate.until(flowEndDate, ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH);
            const timelineFlow = <li
                style={'width:' + flowWidth + 'px; margin-left:' + flowLeftMargin(flow) + 'px;'}>
                <div>
                    <h2>{flowStartDate.format(DateTimeFormatter.ofPattern("HH:mm"))}</h2>
                    <h2>{flowEndDate.format(DateTimeFormatter.ofPattern("HH:mm"))}</h2>
                    <h3>{flow.viewType}</h3>
                </div>
            </li>;
            timelineFlows.appendChild(timelineFlow);
        }
    }

    function toMap() {
        flowData.forEach(flow => {
            drawTimelineTimeFlows(flow);
            if (flowMap.has(rowLabel(flow))) {
                flowMap.get(rowLabel(flow)).push(flow);
            } else {
                flowMap.set(rowLabel(flow), new Array(flow));
            }
        });
        return flowMap;
    }

    function clearContainer() {

        Array.from(timelineContainer.children)
            .filter(node => isNotFlowAndTimeAndNote(node))
            .forEach(node => {
                timelineContainer.removeChild(node);
            });

        function isWithId(node, id) {
            return node.getAttribute("id") === id;
        }

        function isNotFlowAndTimeAndNote(node) {
            return !isWithId(node, 'timeline-flows-' + day)
                && !isWithId(node, 'timeline-time-' + day)
                && !isWithId(node, 'notes-' + day);
        }
    }

    clearContainer();
    if (!timelineFlowsContainer.firstChild) {
        timelineFlowsContainer.appendChild(timelineFlows);
        timelineContainer.appendChild(timelineFlowsContainer);
    }

    toMap().forEach((flows, rowLabel) => {
        const timelineHeader = <div class='timeline-header'/>;
        timelineHeader.appendChild(<div>
            <h3>{rowLabel}</h3>
        </div>);
        const timelineEvents = <div class='timeline-events'/>;
        const timelineEvent = <ul/>;
        flows.forEach(flow => {
            timelineEvent.appendChild(drawTimelineFlow(flow));
            timelineEvents.appendChild(timelineEvent);
        })
        timelineContainer.appendChild(timelineHeader);
        timelineContainer.appendChild(timelineEvents);
    })

    drawTimelineHours();
    timelineContainer.appendChild(timelineTimeContainer);
    if (!container.firstChild) {
        container.appendChild(timelineContainer);
    }
}
