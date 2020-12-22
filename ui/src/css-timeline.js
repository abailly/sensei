import {dom} from './dom.js';
import {formatISOTime} from "./date.js";
import {colorOf} from './color.js';

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

function drawTimelineHour(start) {
    return <li>{formatISOTime(new Date(start))}</li>;

}

export function drawTimeline(container, day, flowData) {
    const timelineContainer = <div id={'timeline-container-' + day}/>;
    const timelineEvents = <ul class='timeline-events'/>;
    const timelineHours = <ul class='timelines-years'/>;

    function drawTimelineHours() {
        const firstFlow = flowData[0];
        const lastFlow = flowData.slice(-1)[0];
        const nbHalfHours = (new Date(lastFlow.flowEnd).getHours() - new Date(firstFlow.flowStart).getHours()) * 2
        const firsFlowStartDate = new Date(firstFlow.flowStart);
        for (let i = 0; i < nbHalfHours; i++) {
            const hour = new Date(firsFlowStartDate.getFullYear(), firsFlowStartDate.getMonth(), firsFlowStartDate.getDate(), firsFlowStartDate.getHours(), firsFlowStartDate.getMinutes() + i * THIRTY_MINUTES);
            timelineHours.appendChild(drawTimelineHour(hour));
        }
    }

    drawTimelineHours();
    flowData.forEach(flow => {
        timelineEvents.appendChild(drawTimelineFlow(flow));
        timelineContainer.appendChild(timelineEvents);
    })
    timelineContainer.appendChild(timelineHours);
    container.appendChild(timelineContainer);
}