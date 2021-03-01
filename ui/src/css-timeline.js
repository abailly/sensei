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

    const timelineContainer = <div id={'timeline-container-' + day}/>;
    const timelineTimeContainer = <div class='timeline-time'/>;
    const timelineTime = <ul/>;
    const timelineFlowsContainer = <div class='timeline-flows'/>;
    const timelineFlows = <ul/>;

    function flowLeftMargin(flow) {
        const flowTypeRow = flowMap.get(flow.flowType);

        function isFirstFlow() {
            return flowTypeRow.indexOf(flow) === 0;
        }

        function isAfterFirstFlow() {
            return flowTypeRow.indexOf(flow) > 0;
        }

        if (flowTypeRow !== undefined) {
            if (isFirstFlow()) {
                return (startTime.until(LocalDateTime.parse(flow.flowStart), ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH;
            }
            if (isAfterFirstFlow()) {
                const previousFlowEnd = LocalDateTime.parse(flowTypeRow[flowTypeRow.indexOf(flow) - 1].flowEnd);
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
            <div class='timeline-event' style={'background: ' + colorOf(flow.flowType) + ';'}></div>
        </li>;

    }

    function drawTimelineHour(time) {
        return <li>{time.format(DateTimeFormatter.ofPattern('HH:mm'))}</li>;

    }

    function drawTimelineHours() {
        const nbHalfHours = (endTime.hour() - startTime.hour()) * 2
        for (let i = 0; i < nbHalfHours; i++) {
            const time = startTime.plusMinutes(i * THIRTY_MINUTES);
            timelineTime.appendChild(drawTimelineHour(time));
        }
        timelineTimeContainer.appendChild(timelineTime);
    }

    function drawTimelineTimeFlows(flow) {
        let flowStartDate = LocalDateTime.parse(flow.flowStart);
        let flowEndDate = LocalDateTime.parse(flow.flowEnd);
        const flowWidth = Math.abs((flowStartDate.until(flowEndDate, ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH);
        const timelineFlow = <li
            style={'width:' + flowWidth + 'px; margin-left:' + flowLeftMargin(flow) + 'px;'}>
            <div>
                <h2>{flowStartDate.format(DateTimeFormatter.ofPattern("HH:mm"))}</h2>
                <h2>{flowEndDate.format(DateTimeFormatter.ofPattern("HH:mm"))}</h2>
                <h3>{flow.flowType}</h3>
            </div>
        </li>;
        timelineFlows.appendChild(timelineFlow);
    }

    function toMap() {
        flowData.forEach(flow => {
            drawTimelineTimeFlows(flow);
            if (flowMap.has(rowLabel(flow))) {
                flowMap.get(rowLabel(flow)).push(flow);
            } else {
                flowMap.set(rowLabel(flow), new Array(flow));
            }
        })
        return flowMap;
    }

    function clearContainer() {
        while (container.firstChild) {
            container.removeChild(container.firstChild);
        }
    }

    clearContainer();
    timelineFlowsContainer.appendChild(timelineFlows);
    timelineContainer.appendChild(timelineFlowsContainer);
    let index = 2;

    toMap().forEach((flows, rowLabel) => {
        const timelineHeader = <div class='timeline-header'/>;
        timelineHeader.style.gridRow = index;
        timelineHeader.appendChild(<div><h3>{rowLabel}</h3></div>);
        const timelineEvents = <div class='timeline-events'/>;
        timelineEvents.style.gridRow = index;
        const timelineEvent = <ul/>;
        flows.forEach(flow => {
            timelineEvent.appendChild(drawTimelineFlow(flow));
            timelineEvents.appendChild(timelineEvent);
        })
        timelineContainer.appendChild(timelineHeader);
        timelineContainer.appendChild(timelineEvents);
        index++;
    })

    drawTimelineHours();
    timelineTimeContainer.style.gridRow = index;
    timelineContainer.appendChild(timelineTimeContainer);
    container.appendChild(timelineContainer);
}