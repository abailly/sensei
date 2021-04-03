import {LocalDate, DateTimeFormatter, TemporalAdjusters, DayOfWeek} from "@js-joda/core";

export function formatISODate(date) {
  return date.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
}

export function formatISODateTime(dateTime) {
    return dateTime.format(DateTimeFormatter.ofPattern('yyyy-MM-dd HH:mm'))
}

/* Compute next day given some ISO8601 formatted day.
This is currently done naively, by computing a `Date` from the given day
string, adding the number of seconds corresponding to a day and reformatting the 
Date back.
*/
export function nextDay(day) {
  const date = new Date(day);
  const result = new Date(date.getTime() + (24 * 3600 * 1000));

  return formatISODateTime(result);
}

/* Returns a 2-element list of strings representing the start and end of current month. 
*/
export function currentMonthPeriod() {
  const date = LocalDate.now();
  const startOfMonth = formatISODate(date.withDayOfMonth(1));
  const endOfMonth = formatISODate(date.plusMonths(1).withDayOfMonth(1));
  return [startOfMonth, endOfMonth];
}

/* Returns a 2-element list of strings representing the start and end of current week. 
*/
export function currentWeekPeriod() {
  const date = LocalDate.now();
  const startOfWeek = formatISODate(date.with(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY)));
  const endOfWeek = formatISODate(date.with(TemporalAdjusters.nextOrSame(DayOfWeek.SUNDAY)));
  return [startOfWeek, endOfWeek];
}

/* Returns a 2-element list of strings representing the start and end of current year.
*/
export function currentYearPeriod() {
  const date = LocalDate.now();
  const startOfYear = formatISODate(date.withDayOfYear(1));
  const endOfYear = formatISODate(date.plusYears(1).withDayOfYear(1));
  return [startOfYear, endOfYear];
}

