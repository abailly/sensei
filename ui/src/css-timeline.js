import {dom} from './dom.js';
import {colorOf} from './color.js';
import { config } from "./config";
import {ChronoUnit, DateTimeFormatter, LocalDateTime, LocalTime} from "@js-joda/core";

const THIRTY_MINUTES = 30;
const HALF_HOUR_WIDTH = 100;

export function drawTimeline(container, day, flowData) {
    const timelineContainer = <div id={'timeline-container-' + day}/>;
    const timelineHeader = <div class='timeline-header' />;
    const timelineEventsContainer = <div />;
    const timelineEvents = <ul class='timeline-events'/>;
    const timelineHours = <ul class='timelines-years'/>;
    const startTime = LocalTime.parse(config.userStartOfDay);
    const endTime = LocalTime.parse(config.userEndOfDay);

    function flowLeftMargin(flow) {
        if(flowData[0] !== flow) {
            return 0;
        }
        const firstFlowStartTime = LocalDateTime.parse(flowData[0].flowStart);
        return firstFlowStartTime.toLocalTime().compareTo(startTime) !== 0 ? (startTime.until(firstFlowStartTime.toLocalTime(), ChronoUnit.MINUTES) / THIRTY_MINUTES) * HALF_HOUR_WIDTH : 0;
    }

    function drawTimelineFlow(flow) {
        let flowStartDate = LocalDateTime.parse(flow.flowStart);
        let flowEndDate = LocalDateTime.parse(flow.flowEnd);
        const flowWidth = Math.abs((flowStartDate.until(flowEndDate, ChronoUnit.MINUTES)/ THIRTY_MINUTES) * HALF_HOUR_WIDTH);
        return <li style={'width:'+flowWidth+'px; margin-left:'+flowLeftMargin(flow)+'px'}>
            <div><h2>{flowStartDate.format(DateTimeFormatter.ofPattern("HH:mm"))}</h2><h2>{flowEndDate.format(DateTimeFormatter.ofPattern("HH:mm"))}</h2><h3 style="text-overflow: ellipsis;white-space: nowrap;overflow: hidden">{flow.flowType}</h3></div>
            <div style={'background: '+colorOf(flow.flowType)+'; position: absolute;left: 0;bottom: -16px;height: 8px;border-radius: 8px;width: 100%;'}></div></li>;

    }

    function drawTimelineHour(time) {
        return <li>{time.format(DateTimeFormatter.ofPattern('HH:mm'))}</li>;

    }

    function drawTimelineHours() {
        const nbHalfHours = (endTime.hour() - startTime.hour()) * 2
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