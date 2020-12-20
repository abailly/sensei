import {formatISODate, formatISOTime} from './date.js';
import {describe, expect, test} from '@jest/globals';

describe('ISO Formatter', () => {
    test('format basic date', () => {
        expect(formatISODate(new Date(2020, 10, 30))).toBe('2020-11-30');
    });
    test('format with month on 2 digits', () => {
        expect(formatISODate(new Date(2020, 1, 28))).toBe('2020-02-28');
    });
    test('format with day on 2 digits', () => {
        expect(formatISODate(new Date(2020, 2, 1))).toBe('2020-03-01');
    });
    test('format basic time', () => {
        expect(formatISOTime(new Date(2020, 10, 30, 11, 35))).toBe('11:35');
    });
    test('format hour on 2 digits', () => {
        expect(formatISOTime(new Date(2020, 10, 30, 8, 35))).toBe('08:35');
    });
    test('format minutes on 2 digits', () => {
        expect(formatISOTime(new Date(2020, 10, 30, 8))).toBe('08:00');
    });
});
