import {formatISODate, formatISODateTime} from './date.js';
import {describe, expect, test} from '@jest/globals';
import {LocalDate, LocalDateTime} from "@js-joda/core";

describe('ISO Formatter', () => {

    describe('Date formatter', () => {
        test('format date', () => {
            expect(formatISODate(LocalDate.of(2020, 11, 30))).toBe('2020-11-30');
        });
    });

    describe('DateTime formatter', () => {
        test('format date and time', () => {
            expect(formatISODateTime(LocalDateTime.of(2020, 11, 30, 10, 24, 32))).toBe('2020-11-30 10:24');
        })
    })

});