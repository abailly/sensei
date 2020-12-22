import {dom} from './dom.js';
import {formatISOTime} from "./date.js";

import {describe, expect, test} from '@jest/globals';
import {drawTimeline} from "./css-timeline";

const flowData = [
    {
        "flowStart": "2020-12-17T08:30:00",
        "flowType": "Flowing",
        "flowEnd": "2020-12-17T09:00:00"
    }, {
        "flowStart": "2020-12-17T09:00:00",
        "flowType": "Daily",
        "flowEnd": "2020-12-17T09:30:00"
    }, {
        "flowStart": "2020-12-17T09:30:00",
        "flowType": "Meeting",
        "flowEnd": "2020-12-17T10:15:00"
    }, {
        "flowStart": "2020-12-17T10:15:00",
        "flowType": "Meeting",
        "flowEnd": "2020-12-17T12:00:00"
    }, {
        "flowStart": "2020-12-17T12:00:00",
        "flowType": "Other",
        "flowEnd": "2020-12-17T13:40:00"
    }, {
        "flowStart": "2020-12-17T13:40:00",
        "flowType": "Meeting",
        "flowEnd": "2020-12-17T14:00:00"
    }, {
        "flowStart": "2020-12-17T14:00:00",
        "flowType": "Meeting",
        "flowEnd": "2020-12-17T15:00:00"
    }, {
        "flowStart": "2020-12-17T15:00:00",
        "flowType": "Rework",
        "flowEnd": "2020-12-17T17:00:00"
    },
    {
        "flowStart": "2020-12-17T17:00:00",
        "flowType": "Meeting",
        "flowEnd": "2020-12-17T17:00:00"
    }
];

describe('CSS Timeline', () => {
    test('expect timiline to be drawn', () => {
        let container = document.createElement("div");
        drawTimeline(container, '2020-12-17', flowData);
        expect(container).toMatchSnapshot();
    });
});