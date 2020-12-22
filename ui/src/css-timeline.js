import {dom} from './dom.js';
import {formatISOTime} from "./date.js";
import {colorOf} from './color.js';
import {DateTimeFormatter, LocalTime} from "@js-joda/core";

const THIRTY_MINUTES = 30;
const MILLISECONDS = 1000;
const SECONDS = 60;
const HALF_HOUR_WIDTH = 100;

function drawTimelineFlow(flow) {
    let flowStartDate = new Date(flow.flowStart);
    let flowEndDate = new Date(flow.flowEnd);
    const flowWidth = Math.abs((((flowEndDate.getTime() - flowStartDate.getTime()) / MILLISECONDS / SECONDS) / THIRTY_MINUTES) * HALF_HOUR_WIDTH);
    return <li style={'width:'+flowWidth+'px'}>
        <div><h2>{formatISOTime(flowStartDate)}</h2><h2>{formatISOTime(flowEndDate)}</h2><h3 style="text-overflow: ellipsis;white-space: nowrap;overflow: hidden">{flow.flowType}</h3></div>
        <div style={'background: '+colorOf(flow.flowType)+'; position: absolute;left: 0;bottom: -16px;height: 8px;border-radius: 8px;width: 100%;'}></div></li>;

}

function drawTimelineHour(time) {
    return <li>{time.format(DateTimeFormatter.ofPattern('HH:mm'))}</li>;

}

export function drawTimeline(container, day, userStartOfDay, userEndOfDay, flowData) {
    const timelineContainer = <div id={'timeline-container-' + day}/>;
    const timelineHeader = <div class='timeline-header' />;
    const timelineEventsContainer = <div />;
    const timelineEvents = <ul class='timeline-events'/>;
    const timelineHours = <ul class='timelines-years'/>;

    function drawTimelineHours() {
        const startTime = LocalTime.parse(userStartOfDay);
        const nbHalfHours = (LocalTime.parse(userEndOfDay).hour() - startTime.hour()) * 2
        for (let i = 0; i < nbHalfHours; i++) {
            const time =  startTime.plusMinutes(i * THIRTY_MINUTES);
            timelineHours.appendChild(drawTimelineHour(time));
        }
    }

    timelineHeader.appendChild(<h3>{day}</h3>);
    drawTimelineHours();
    flowData.forEach(flow => {
        timelineEvents.appendChild(drawTimelineFlow(flow));
        timelineEventsContainer.appendChild(timelineEvents);
    })
    timelineEventsContainer.appendChild(timelineHours);
    timelineContainer.appendChild(timelineHeader);
    timelineContainer.appendChild(timelineEventsContainer);
    container.appendChild(timelineContainer);
}