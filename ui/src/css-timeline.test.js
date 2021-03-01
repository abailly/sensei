import {dom} from './dom.js';
import {formatISOTime} from "./date.js";
import {config} from "./config";

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

    beforeEach(() => {
        config.userProfile = {
            userFlowTypes: {
                "Other": "#33ffd4",
                "Learning": "#ff8822",
                "Flowing": "#00dd22",
                "Troubleshooting": "#ee1111",
                "Rework": "#4500dd",
                "Meeting": "#fff203",
                "Experimenting": "#0022dd"
            }
        };
    });

    test('expect timeline to be drawn', () => {
        config.userProfile.userStartOfDay = '08:30:00';
        config.userProfile.userEndOfDay = '17:00:00';
        let container = document.createElement("div");
        drawTimeline(container, '2020-12-17', flowData);
        expect(container).toMatchSnapshot();
    });

    test('expect timeline to be drawn with negative left margin when flow start before user start of day', () => {
        config.userProfile.userStartOfDay = '09:00:00';
        config.userProfile.userEndOfDay = '17:00:00';
        let container = document.createElement("div");
        drawTimeline(container, '2020-12-17', flowData);
        expect(container).toMatchSnapshot();
    });

    test('expect timeline to be drawn with positive left margin when flow start after user start of day', () => {
        config.userProfile.userStartOfDay = '08:00:00';
        config.userProfile.userEndOfDay = '17:00:00';
        let container = document.createElement("div");
        drawTimeline(container, '2020-12-17', flowData);
        expect(container).toMatchSnapshot();
    });

    test('expect timeline to be expand at selected date', () => {
        config.userProfile.userStartOfDay = '08:00:00';
        config.userProfile.userEndOfDay = '17:00:00';
        let container = document.createElement("div");
        drawTimeline(container, '2020-12-17', flowData, f => f.flowType);
        expect(container).toMatchSnapshot();
    });
});