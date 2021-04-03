import {LocalDate, DateTimeFormatter} from "@js-joda/core";

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

function lastDayOfMonth(date) {
  var lastDay = 30;
  switch(date.month()) {
  case 0:  case 2:  case 4:  case 6:  case 7:
  case 9: case 11:
    lastDay = 31;
  case 1:
    if (date.year() % 4 == 0 && date.year() % 100 != 0) {
      lastDay = 29;
    } else {
      lastDay = 28;
    }
  default:
    lastDay = 30;
  }
  return LocalDate.of(date.year(),date.month(),lastDay);
}

/* Returns a 2-element list of strings representing the start and end of current month. 
*/
export function currentMonthPeriod() {
  const date = LocalDate.now();
  const startOfMonth = formatISODate(LocalDate.of(date.year(),date.month(),1));
  const endOfMonth = formatISODate(lastDayOfMonth(date));
  return ['2021-03-01', '2021-03-31'];
}

