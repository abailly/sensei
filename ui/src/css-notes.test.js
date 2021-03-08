import {dom} from './dom.js';
import {config} from "./config";

import {describe, expect, test} from '@jest/globals';
import {drawCssNotes} from "./css-notes";

const notes = [{
    "noteStart": "2020-11-16T09:30:00",
    "noteContent": "# Note\r## Première note"
}, {
    "noteStart": "2020-11-16T10:55:00",
    "noteContent": "# Note\r## Seconde note"
}, {
    "noteStart": "2020-11-16T12:20:00",
    "noteContent": "# Note\r## Troisième note"
}];

describe('CSS Notes', () => {

    let mainContainer;
    let container;

    beforeEach(() => {
        config.userProfile = {};
        container = document.createElement("div");
        mainContainer = <div id="main" />;
        container.appendChild(<div/>);
        mainContainer.appendChild(container);
        document.body.appendChild(mainContainer);
    });

    afterEach(() => {
        document.body.removeChild(mainContainer);
    });

    test('expect notes to be drawn', () => {
        config.userProfile.userStartOfDay = '08:30:00';
        config.userProfile.userEndOfDay = '17:00:00';

        drawCssNotes(container, '2020-12-16', notes);

        expect(document.body).toMatchSnapshot();
    });

    test('expect previous drawn notes to be removed', () => {
        config.userProfile.userStartOfDay = '08:30:00';
        config.userProfile.userEndOfDay = '17:00:00';

        drawCssNotes(container, '2020-12-16', notes);
        drawCssNotes(container, '2020-12-16', notes);

        expect(document.body).toMatchSnapshot();
    });
});